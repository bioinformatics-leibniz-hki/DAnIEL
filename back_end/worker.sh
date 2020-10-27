#!/usr/bin/env bash

# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

# Process projects in the queue file
# This script will be started by the back end docker container

queue_file=$DANIEL_USERDAT_DIR/queue
queue_file_check_interval=10
jobs_per_project=$(nproc | awk '{print $1-1}')
conda_prefix=/opt/conda/envs/
snakefile=/app/back_end/daniel.snakefile.py
threads=10

# start crontabs e.g. for auto removing old projects
echo "1 * * * * root find $DANIEL_USERDAT_DIR -maxdepth 1 -mindepth 1 -type d -mtime +35 | grep -v -E 'example|templates' | xargs rm -rf" > /etc/cron.d/remove_projects
chmod 600 /etc/cron.d/remove_projects
service cron start

cp /app/back_end/msmtprc /etc/msmtprc

function notify_mail {
	project_id=$1
	email_address=$(grep -oP '(?<="email_address": ")[^"]*' $DANIEL_USERDAT_DIR/$project_id/input/project.json)
	
	# no email address present
	[[ -z "$email_address" ]] && return 0

	html_report_file=$DANIEL_USERDAT_DIR/$project_id/analysis/Selected_Analysis/summary/all.html
	
	cat <<'EOF' \
	| sed -e "s/PROJECT/$project_id/g" \
	| s-nail -a $html_report_file -s "Your DAnIEL project is finished" $email_address
Dear Sir or Madam,

Your project PROJECT in DAnIEL webserver is finished.
Interactive analysis is available within the next 30 days at https://sbi.hki-jena.de/daniel/latest/?project_id=PROJECT.
Additionally, we attached a HTML summary report about the project.
Please do not forget to cite the paper: DAnIEL: a complete webserver for Describing, Analyzing and Integrating fungal Ecology to effectively study the systems of Life.

Kind regards,

DAnIEL webserver team

Systems Biology and Bioinformatics
Leibniz Institute for Natural Product Research and Infection Biology
Hans Knöll Institute (HKI)
Adolf-Reichwein-Straße 23
07745 Jena
Germany
EOF
}

source activate base
while true; do
	if [ -f "$queue_file" ]; then
		if [ `wc -l $queue_file | awk '{print $1}'` -ge "1" ]; then
			cur_project_id=$(head -n 1 $queue_file)
			echo "Process project $cur_project_id"
			project_dir=$DANIEL_USERDAT_DIR/$cur_project_id/
			configfile=$project_dir/input/project.json

			cd $project_dir
			snakemake \
				--keep-going \
				--nolock \
				--use-conda \
				--conda-prefix $conda_prefix \
				--snakefile $snakefile \
				--configfile $configfile \
				--jobs $jobs_per_project \
				--cores $threads

			notify_mail $cur_project_id
			# remove project from queue
			sed -i '1d' $queue_file		
		else
			echo "no project in queue"
		fi
	else
			echo "No queue file at $queue_file"
	fi
	sleep $queue_file_check_interval
done
