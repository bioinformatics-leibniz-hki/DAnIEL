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

export DANIEL_REPO_DIR=$PWD/..
export DANIEL_USERDAT_DIR=$DANIEL_REPO_DIR/test

# test deployment
cd ..
./deploy.sh
cd $DANIEL_USERDAT_DIR

# test snakemake exists
docker exec funexplorer_backend_1 snakemake --version

# test queue
docker exec funexplorer_backend_1 bash -c "echo test_project >> /userdat/queue" \
	&& sleep 20 \
	# test if queue is now empty and back end took project
	&& test -s queue || :
docker exec funexplorer_backend_1 bash -c "pkill snakemake"

# test steps
docker exec funexplorer_backend_1 bash -c "bash /userdat/test_backend.sh"
