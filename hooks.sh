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

#
# Hooks to run e.g. before every git push
#

# Reformat R files in all directories
R -e "styler::style_dir('.')"

# deploy library
R -e "devtools::install('danielLib', upgrade = 'never')" 
R -e "devtools::document('danielLib')"

# depoly container
docker-compose build --parallel
docker-compose up
