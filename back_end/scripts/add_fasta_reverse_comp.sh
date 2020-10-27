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
# Append the reverse complement of all sequences of a fasta file
# Added sequences will have the sufix _rev in their header
# USAGE: add_fasta_reverse_comp.sh [in_file] [out_file]
#

in_file=${1:-/dev/stdin}
out_file=${2:-/dev/stdout}

cat $in_file \
	| bioawk -c fastx '{print ">"$name"_rev";print revcomp($seq)}' \
	>> $out_file