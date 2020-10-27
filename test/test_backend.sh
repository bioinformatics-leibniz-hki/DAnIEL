#!/usr/bin/env sh

# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

echo test project using various params

cd $DANIEL_USERDAT_DIR/test_project
snakemake \
	--nolock \
	--use-conda \
	--conda-prefix /opt/conda/envs/ \
	--snakefile /app/back_end/daniel.snakefile.py \
	--configfile input/project.json \
	--jobs $jobs_per_project \
	--forcerun input_report
