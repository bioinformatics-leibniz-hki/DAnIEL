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
# 1. download article xml of pubmed id
# 2. parse xml to bibtex using pubmed2bibtex.xsl
#

pubmed_id=${1:-/dev/stdin}
pubmed_xml_to_bibtex_xslt=${3:-"pubmed2bibtex.xsl"}

efetch -mode xml -db pubmed -id $pubmed_id \
	| xsltproc $pubmed_xml_to_bibtex_xslt /dev/stdin 2>/dev/null

# do not spam NCBI server
sleep 0.3
