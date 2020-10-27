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

#
# Searches for references of type pmid[0-9]+ in a file to create bibtex references 
#
# USAGE: create_pubmed_references.sh report.Rmd
#

file=${1:-/dev/stdin}

xargs -i grep -oE 'pmid[0-9]+' $file \
	| sed 's/^pmid//g' \
	| xargs pubmed_id_to_bibtex.sh