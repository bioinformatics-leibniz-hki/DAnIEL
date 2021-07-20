#!/usr/bin/env sh
#
# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

# knit Rmd
#find front_end/ | grep Rmd$ | xargs -i -P $(nproc) R -e "rmarkdown::render('{}')"

# source optional file with environment variables:
# DANIEL_DIR, DANIEL_DB_DIR and DANIEL_USERDAT_DIR
set -o allexport
source .env || :
set +o allexport

# build images
export COMPOSE_DOCKER_CLI_BUILD=1 
export DOCKER_BUILDKIT=1

git pull
git lfs pull

docker-compose pull
docker-compose build --parallel
docker-compose push
docker-compose up

# docker swarm mode
#docker stack rm daniel
#docker stack deploy -c docker-compose.yml daniel

