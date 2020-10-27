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
# Creates THF data base
# IN:	THF release fasta file and csv taxonomy annotation
# OUT:	Parsed taxonomy and classifiers of the input db
#

in_fasta=${1:-/dev/stdin}
in_tax_csv=$2
out_dir=${2:-out}

mkdir -p $out_dir

# create fasta
ln -s $in_fasta $out_dir/refdb.fasta
qiime tools import \
	--input-path $out_dir/refdb.fasta \
	--output-path $out_dir/refdb.fasta.qza \
	--type 'FeatureData[Sequence]'

# create THF taxonomy
cat $in_tax_csv \
	| awk 'BEGIN {FS=","} NR>1 {print $8"\tk__"$1";p__"$2";c__"$3";o__"$4";f__"$5";g__"$6";s__"$6"_"$7";t__"$8}' \
	>  $out_dir/refdb.tax

qiime tools import \
	--type 'FeatureData[Taxonomy]' \
	--input-format HeaderlessTSVTaxonomyFormat \
	--input-path $out_dir/refdb.tax \
	--output-path $out_dir/refdb.tax.qza

qiime feature-classifier fit-classifier-naive-bayes \
	--i-reference-reads $out_dir/refdb.fasta.qza \
	--i-reference-taxonomy $out_dir/refdb.tax.qza \
	--o-classifier $out_dir/refdb.ITS.classifier.qza