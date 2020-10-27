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
# Creates a rooted tree from a fasta file of representative denoised sequences
#

in_fasta=${1:-/dev/stdin}
out_dir=${2:-out}

mkdir -p $out_dir
qiime tools import \
	--input-path $in_fasta \
	--type 'FeatureData[Sequence]' \
	--output-path $out_dir/repseqs.qza
qiime alignment mafft \
	--i-sequences $out_dir/repseqs.qza \
	--o-alignment $out_dir/repseqs.aligned.qza
qiime alignment mask \
	--i-alignment $out_dir/repseqs.aligned.qza \
	--o-masked-alignment $out_dir/repseqs.aligned.masked.qza
qiime phylogeny fasttree \
	--i-alignment $out_dir/repseqs.aligned.masked.qza \
	--o-tree $out_dir/repseqs.tree.qza
qiime phylogeny midpoint-root \
	--i-tree $out_dir/repseqs.tree.qza \
	--o-rooted-tree $out_dir/repseqs.tree.rooted.qza
qiime tools export \
	--input-path $out_dir/repseqs.tree.rooted.qza \
	--output-path $out_dir \
	--output-format NewickDirectoryFormat
qiime tools export \
	--input-path $out_dir/repseqs.aligned.masked.qza \
	--output-path $out_dir \
	--output-format AlignedDNASequencesDirectoryFormat