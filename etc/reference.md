Reference
================

# Tools

The general framework is based on Snakemake (Koster and Rahmann, 2012),
R, Tidyverse, and
Docker.

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Tools

</caption>

<thead>

<tr>

<th style="text-align:left;">

Pipeline
step

</th>

<th style="text-align:left;">

Tool

</th>

<th style="text-align:left;">

Description

</th>

<th style="text-align:left;">

Version

</th>

<th style="text-align:left;">

Citation

</th>

<th style="text-align:left;">

Source

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;vertical-align: middle !important;" rowspan="5">

analysis

</td>

<td style="text-align:left;">

BAnOCC

</td>

<td style="text-align:left;width: 20em; ">

compositionality aware correlations between species

</td>

<td style="text-align:left;">

1.1

</td>

<td style="text-align:left;">

Schwager *et al.* (2017)

</td>

<td style="text-align:left;">

[Anaconda](https://anaconda.org/bioconda/bioconductor-banocc)

</td>

</tr>

<tr>

<td style="text-align:left;">

caret

</td>

<td style="text-align:left;width: 20em; ">

R package for Machine Learning

</td>

<td style="text-align:left;">

6

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

[Anaconda](https://anaconda.org/r/r-caret)

</td>

</tr>

<tr>

<td style="text-align:left;">

phyloseq

</td>

<td style="text-align:left;width: 20em; ">

R package for handling samples, features, and trees

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

McMurdie and Holmes (2013)

</td>

<td style="text-align:left;">

[Anaconda](https://anaconda.org/bioconda/bioconductor-phyloseq)

</td>

</tr>

<tr>

<td style="text-align:left;">

SparCC / FastSpar

</td>

<td style="text-align:left;width: 20em; ">

sparseness aware correlations between species

</td>

<td style="text-align:left;">

0.0.1

</td>

<td style="text-align:left;">

Watts *et al.* (2019), Friedman and Alm (2012)

</td>

<td style="text-align:left;">

[Anaconda](https://anaconda.org/bioconda/fastspar)

</td>

</tr>

<tr>

<td style="text-align:left;">

vegan

</td>

<td style="text-align:left;width: 20em; ">

Diversity
analysis

</td>

<td style="text-align:left;">

2

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

[Anaconda](https://anaconda.org/bioconda/r-vegan)

</td>

</tr>

<tr>

<td style="text-align:left;vertical-align: middle !important;" rowspan="2">

denoising

</td>

<td style="text-align:left;">

DADA2

</td>

<td style="text-align:left;width: 20em; ">

Denoising reads to get Amplicon Sequencing Variants (ASV)

</td>

<td style="text-align:left;">

1.14

</td>

<td style="text-align:left;">

Callahan *et al.* (2016)

</td>

<td style="text-align:left;">

[Anaconda](https://anaconda.org/bioconda/bioconductor-dada2)

</td>

</tr>

<tr>

<td style="text-align:left;">

PIPITS

</td>

<td style="text-align:left;width: 20em; ">

Denoising reads to get Operational Taxonomic Units (OTU)

</td>

<td style="text-align:left;">

2.4

</td>

<td style="text-align:left;">

Gweon *et al.* (2015)

</td>

<td style="text-align:left;">

[Anaconda](https://anaconda.org/bioconda/pipits)

</td>

</tr>

<tr>

<td style="text-align:left;">

phylotyping

</td>

<td style="text-align:left;">

QIIME2

</td>

<td style="text-align:left;width: 20em; ">

Taxonomic classifications of representative sequences, tree building

</td>

<td style="text-align:left;">

r2019.7

</td>

<td style="text-align:left;">

Bolyen *et al.*
(2019)

</td>

<td style="text-align:left;">

[Anaconda](https://anaconda.org/qiime2/qiime2)

</td>

</tr>

<tr>

<td style="text-align:left;vertical-align: middle !important;" rowspan="3">

qc

</td>

<td style="text-align:left;">

FastQC

</td>

<td style="text-align:left;width: 20em; ">

Assessing sequence read quality

</td>

<td style="text-align:left;">

0.11.8

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

[Anaconda](https://anaconda.org/bioconda/fastqc)

</td>

</tr>

<tr>

<td style="text-align:left;">

MultiQC

</td>

<td style="text-align:left;width: 20em; ">

Combining FastQC results from multiple samples

</td>

<td style="text-align:left;">

1.7

</td>

<td style="text-align:left;">

Ewels *et al.* (2016)

</td>

<td style="text-align:left;">

[Anaconda](https://anaconda.org/bioconda/multiqc)

</td>

</tr>

<tr>

<td style="text-align:left;">

trimmomatic

</td>

<td style="text-align:left;width: 20em; ">

QC reads by filtering and trimming

</td>

<td style="text-align:left;">

0.39

</td>

<td style="text-align:left;">

Bolger *et al.* (2014)

</td>

<td style="text-align:left;">

[Anaconda](https://anaconda.org/bioconda/trimmomatic)

</td>

</tr>

<tr>

<td style="text-align:left;">

raw

</td>

<td style="text-align:left;">

grabseqs

</td>

<td style="text-align:left;width: 20em; ">

Downlaoding raw reads from NCBI
SRA

</td>

<td style="text-align:left;">

0.5

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

[Anaconda](https://anaconda.org/louiejtaylor/grabseqs)

</td>

</tr>

</tbody>

</table>

# Parameters

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Parameters

</caption>

<thead>

<tr>

<th style="text-align:left;">

Pipeline step

</th>

<th style="text-align:left;">

Description

</th>

<th style="text-align:left;">

Value type

</th>

<th style="text-align:left;">

Default value

</th>

<th style="text-align:left;">

Choices

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

analysis

</td>

<td style="text-align:left;width: 20em; ">

Inter species correlation
method

</td>

<td style="text-align:left;">

nominal

</td>

<td style="text-align:left;">

SparCC

</td>

<td style="text-align:left;">

<ul>

<li>

SparCC

</li>

<li>

BAnOCC

</li>

<li>

Spearman

</li>

</ul>

</td>

</tr>

<tr>

<td style="text-align:left;vertical-align: middle !important;" rowspan="3">

denoising

</td>

<td style="text-align:left;width: 20em; ">

Denoising method

</td>

<td style="text-align:left;">

nominal

</td>

<td style="text-align:left;">

ASV (DADA2)

</td>

<td style="text-align:left;">

<ul>

<li>

OTU (PIPITS)

</li>

<li>

ASV (DADA2)

</li>

</ul>

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Include singletons

</td>

<td style="text-align:left;">

binary

</td>

<td style="text-align:left;">

FALSE

</td>

<td style="text-align:left;">

<ul>

<li>

TRUE

</li>

<li>

FALSE

</li>

</ul>

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

ITS
region

</td>

<td style="text-align:left;">

nominal

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

<ul>

<li>

ITS1

</li>

<li>

ITS2

</li>

</ul>

</td>

</tr>

<tr>

<td style="text-align:left;vertical-align: middle !important;" rowspan="5">

features

</td>

<td style="text-align:left;width: 20em; ">

Minimum abundance to count prevalence (%)

</td>

<td style="text-align:left;">

Float \[0,100\]

</td>

<td style="text-align:left;">

0.01

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Minimum prevalence (%)

</td>

<td style="text-align:left;">

Float \[0,100\]

</td>

<td style="text-align:left;">

20

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Normalization method

</td>

<td style="text-align:left;">

nominal

</td>

<td style="text-align:left;">

CLR (centered log-ratio)

</td>

<td style="text-align:left;">

<ul>

<li>

CLR (centered log-ratio)

</li>

<li>

CSS (Cumulative sum scaling)

</li>

<li>

TSS (Total sum scaling)

</li>

<li>

Rarefaction

</li>

<li>

Raw (No normalization)

</li>

</ul>

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Phylogeny

</td>

<td style="text-align:left;">

nominal

</td>

<td style="text-align:left;">

Index Fungorum 2016

</td>

<td style="text-align:left;">

Index Fungorum 2016

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Taxonomic
rank

</td>

<td style="text-align:left;">

nominal

</td>

<td style="text-align:left;">

Species

</td>

<td style="text-align:left;">

<ul>

<li>

Strain

</li>

<li>

Species

</li>

<li>

Genus

</li>

<li>

Family

</li>

<li>

Order

</li>

<li>

Class

</li>

<li>

Phylum

</li>

</ul>

</td>

</tr>

<tr>

<td style="text-align:left;vertical-align: middle !important;" rowspan="3">

phylotyping

</td>

<td style="text-align:left;width: 20em; ">

Minimum BLAST identity (fraction)

</td>

<td style="text-align:left;">

Float \[0,1\]

</td>

<td style="text-align:left;">

0.8

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Reference database

</td>

<td style="text-align:left;">

nominal

</td>

<td style="text-align:left;">

UNITE 8.0 dynamic

</td>

<td style="text-align:left;">

<ul>

<li>

UNITE 8.0 dynamic

</li>

<li>

UNITE 8.0 dynamic\_s

</li>

</ul>

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Sequence classifier

</td>

<td style="text-align:left;">

nominal

</td>

<td style="text-align:left;">

Qiime2 Naive Bayes

</td>

<td style="text-align:left;">

<ul>

<li>

Qiime2 BLAST Consensus

</li>

<li>

Qiime2 Naive
Bayes

</li>

</ul>

</td>

</tr>

<tr>

<td style="text-align:left;vertical-align: middle !important;" rowspan="9">

qc

</td>

<td style="text-align:left;width: 20em; ">

Sequences to Trim (Adapters and Primers in FASTA format)

</td>

<td style="text-align:left;">

string

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Additional adapters

</td>

<td style="text-align:left;">

nominal

</td>

<td style="text-align:left;">

<ul>

<li>

NexteraPE-PE.fa

</li>

<li>

TruSeq2-PE.fa

</li>

<li>

TruSeq3-PE-2.fa

</li>

</ul>

</td>

<td style="text-align:left;">

<ul>

<li>

NexteraPE-PE.fa

</li>

<li>

TruSeq2-PE.fa

</li>

<li>

TruSeq3-PE-2.fa

</li>

<li>

TruSeq2-SE.fa

</li>

<li>

TruSeq3-SE.fa

</li>

</ul>

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Include reverse complement of primer sequences

</td>

<td style="text-align:left;">

binary

</td>

<td style="text-align:left;">

TRUE

</td>

<td style="text-align:left;">

<ul>

<li>

TRUE

</li>

<li>

FALSE

</li>

</ul>

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Minimum number of QC reads

</td>

<td style="text-align:left;">

integer

</td>

<td style="text-align:left;">

1000

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Minimim quality of leading bases (Phred)

</td>

<td style="text-align:left;">

integer

</td>

<td style="text-align:left;">

25

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Minimim quality of trailing bases (Phred)

</td>

<td style="text-align:left;">

integer

</td>

<td style="text-align:left;">

25

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Minimum read length (bp)

</td>

<td style="text-align:left;">

integer

</td>

<td style="text-align:left;">

100

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Phred Score Format

</td>

<td style="text-align:left;">

nominal

</td>

<td style="text-align:left;">

phred33

</td>

<td style="text-align:left;">

<ul>

<li>

phred33

</li>

<li>

phred64

</li>

</ul>

</td>

</tr>

<tr>

<td style="text-align:left;width: 20em; ">

Exclude sample if any of these tests failed

</td>

<td style="text-align:left;">

nominal

</td>

<td style="text-align:left;">

<ul>

<li>

Adapter content

</li>

<li>

Per base N content

</li>

<li>

Per base sequence quality

</li>

<li>

Per sequence quality scores

</li>

<li>

Sequence length distribution

</li>

<li>

Minimum number of QC reads

</li>

</ul>

</td>

<td style="text-align:left;">

<ul>

<li>

Adapter content

</li>

<li>

Per base N content

</li>

<li>

Per base sequence quality

</li>

<li>

Per sequence quality scores

</li>

<li>

Per tile sequence quality

</li>

<li>

Sequence length distribution

</li>

<li>

Minimum number of QC
reads

</li>

</ul>

</td>

</tr>

</tbody>

</table>

# Projects

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Projects

</caption>

<thead>

<tr>

<th style="text-align:left;">

ITS
region

</th>

<th style="text-align:left;">

Bioproject

</th>

<th style="text-align:left;">

Citation

</th>

<th style="text-align:left;">

Description

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;width: 6em; ">

ITS1

</td>

<td style="text-align:left;">

<a href="https://www.ncbi.nlm.nih.gov/bioproject/PRJNA286273">PRJNA286273
</a>

</td>

<td style="text-align:left;width: 8em; ">

Leung *et al.* (2016)

</td>

<td style="text-align:left;width: 30em; ">

skin
mycobiome

</td>

</tr>

<tr>

<td style="text-align:left;width: 6em; vertical-align: middle !important;" rowspan="2">

ITS2

</td>

<td style="text-align:left;">

<a href="https://www.ncbi.nlm.nih.gov/bioproject/PRJNA356769">PRJNA356769
</a>

</td>

<td style="text-align:left;width: 8em; ">

Nash *et al.* (2017)

</td>

<td style="text-align:left;width: 30em; ">

Human Microbiome
Project

</td>

</tr>

<tr>

<td style="text-align:left;">

<a href="https://www.ncbi.nlm.nih.gov/bioproject/PRJNA505509">PRJNA505509
</a>

</td>

<td style="text-align:left;width: 8em; ">

Heisel *et al.* (2019)

</td>

<td style="text-align:left;width: 30em; ">

NICU and breastmilk

</td>

</tr>

</tbody>

</table>

# Browsers

DAnIEL was sucessfully tested with the following
browers:

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<caption>

Browsers

</caption>

<thead>

<tr>

<th style="text-align:left;">

Browser

</th>

<th style="text-align:left;">

OS

</th>

<th style="text-align:left;">

Versions

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Firefox

</td>

<td style="text-align:left;">

GNU/Linux

</td>

<td style="text-align:left;">

70.0

</td>

</tr>

</tbody>

</table>

# References

<div id="refs" class="references">

<div id="ref-Koster2012">

\[1\] Koster, J., and Rahmann, S. Snakemake–a scalable bioinformatics
workflow engine. *Bioinformatics (Oxford, England)* **28,** 2520–2
(2012).

</div>

<div id="ref-Schwager2017">

\[2\] Schwager, E. *et al.* A bayesian method for detecting pairwise
associations in compositional data. *PLoS computational biology* **13,**
e1005852 (2017).

</div>

<div id="ref-McMurdie2013">

\[3\] McMurdie, P.J., and Holmes, S. Phyloseq: An r package for
reproducible interactive analysis and graphics of microbiome census
data. *PloS one* **8,** e61217 (2013).

</div>

<div id="ref-Watts2019">

\[4\] Watts, S.C. *et al.* FastSpar: Rapid and scalable correlation
estimation for compositional data. *Bioinformatics (Oxford, England)*
**35,** 1064–1066 (2019).

</div>

<div id="ref-Friedman2012">

\[5\] Friedman, J., and Alm, E.J. Inferring correlation networks from
genomic survey data. *PLoS computational biology* **8,** e1002687
(2012).

</div>

<div id="ref-Callahan2016">

\[6\] Callahan, B.J. *et al.* DADA2: High-resolution sample inference
from illumina amplicon data. *Nature methods* **13,** 581–3 (2016).

</div>

<div id="ref-Gweon2015">

\[7\] Gweon, H.S. *et al.* PIPITS: An automated pipeline for analyses of
fungal internal transcribed spacer sequences from the illumina
sequencing platform. *Methods in ecology and evolution* **6,** 973–980
(2015).

</div>

<div id="ref-Bolyen2019">

\[8\] Bolyen, E. *et al.* Reproducible, interactive, scalable and
extensible microbiome data science using qiime 2. *Nature biotechnology*
**37,** 852–857 (2019).

</div>

<div id="ref-Ewels2016">

\[9\] Ewels, P. *et al.* MultiQC: Summarize analysis results for
multiple tools and samples in a single report. *Bioinformatics (Oxford,
England)* **32,** 3047–8 (2016).

</div>

<div id="ref-Bolger2014">

\[10\] Bolger, A.M. *et al.* Trimmomatic: A flexible trimmer for
illumina sequence data. *Bioinformatics (Oxford, England)* **30,**
2114–20 (2014).

</div>

<div id="ref-Leung2016">

\[11\] Leung, M.H.Y. *et al.* Skin fungal community and its correlation
with bacterial community of urban chinese individuals. *Microbiome*
**4,** 46 (2016).

</div>

<div id="ref-Nash2017">

\[12\] Nash, A.K. *et al.* The gut mycobiome of the human microbiome
project healthy cohort. *Microbiome* **5,** 153 (2017).

</div>

<div id="ref-Heisel2019">

\[13\] Heisel, T. *et al.* Breastmilk and nicu surfaces are potential
sources of fungi for infant mycobiomes. *Fungal genetics and biology :
FG & B* **128,** 29–35 (2019).

</div>

</div>
