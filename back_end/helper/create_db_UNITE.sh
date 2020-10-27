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
# Creates UNITE data base
# IN:	UNITE release fasta file
# OUT:	Parsed taxonomy and classifiers of the input db
#

in_fasta=${1:-/dev/stdin}
out_dir=${2:-out}

mkdir -p $out_dir

# rename header
bioawk -c fastx '$seq ~ /^[ATGCatgc]+$/ {split($name, names, "|"); $name=names[3]; $seq=toupper($seq); print ">"$name"\n"$seq}' $in_fasta \
	 > $out_dir/refdb.fasta
qiime tools import \
	--input-path $out_dir/refdb.fasta \
	--output-path $out_dir/refdb.fasta.qza \
	--type 'FeatureData[Sequence]'

# create qiime2 taxonomy
grep ">" $in_fasta \
	| awk '{split($0, names, "|"); print names[3]"\t"names[5]";t__"names[3]}' \
	| sed 's/unidentified//g' \
	| sed 's/sp;/sp.;/' \
	> $out_dir/refdb.tax
qiime tools import \
	--type 'FeatureData[Taxonomy]' \
	--input-format HeaderlessTSVTaxonomyFormat \
	--input-path $out_dir/refdb.tax \
	--output-path $out_dir/refdb.tax.qza

qiime feature-classifier fit-classifier-naive-bayes \
	--i-reference-reads $out_dir/refdb.fasta.qza \
	--i-reference-taxonomy $out_dir/refdb.tax.qza \
	--o-classifier $out_dir/refdb.ITS.classifier.qza

# Create qiime2 classifier
# function train_region_qiime2_clf {
# 	fwd_primer=$1
# 	rev_primer=$2
# 	name=$3

# 	qiime feature-classifier extract-reads \
# 		--i-sequences $out_dir/refdb.fasta.qza \
# 		--p-f-primer $fwd_primer \
# 		--p-r-primer $rev_primer \
# 		--o-reads $out_dir/refdb.$name.fasta.qza

# 	qiime feature-classifier fit-classifier-naive-bayes \
# 		--i-reference-reads $out_dir/refdb.$name.fasta.qza \
# 		--i-reference-taxonomy $out_dir/refdb.tax.qza \
# 		--o-classifier $out_dir/refdb.$name.classifier.qza
# }

# train_region_qiime2_clf "TCCGTAGGTGAACCTGCGG" "GCTGCGTTCTTCATCGATGC" "ITS1"
# train_region_qiime2_clf "GCATCGATGAAGAACGCAGC" "TCCTCCGCTTATTGATATGC" "ITS2"
