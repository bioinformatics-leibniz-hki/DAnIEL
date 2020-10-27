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
# Integrate existing BioProjects to the server
#

# cd to projects db directory first
# id in first and NCBI BioProject in third column of db.csv
parallel -j 5 --colsep "," --skip-first-line \
	grabseqs sra -m ../meta.csv -o {1}/sra -t 5 {3} :::: db.csv
