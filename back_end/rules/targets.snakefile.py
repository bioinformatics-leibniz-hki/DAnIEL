#!/usr/bin/env snakemake

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
# List of targets for the pipeline.
# Do not put rules here, otherwise the main rule all
# will not be executed n the default snakemake call
#

# getting raw reads
TARGET_EXT_DOWNLAOD = expand(INPUT_DIR + "reads/{sample}_{mate}.raw.fq.gz", sample = DOWNLOAD_SAMPLES, mate = [1, 2])
TARGET_RAW_LOCAL = expand(PROJECT_DIR + "raw/{sample}_{mate}.raw.fq.gz", sample = LOCAL_SAMPLES, mate = [1, 2])
TARGET_EXT_RAW =  expand(PROJECT_DIR + "raw/{sample}_{mate}.raw.fq.gz", sample = DOWNLOAD_SAMPLES, mate = [1, 2])
TARGET_DEMUX = expand(PROJECT_DIR + "raw/{sample}_{mate}.raw.fq.gz", sample = MUXED_SAMPLES, mate = [1, 2])
TARGET_RAW_PROJECTS = expand(PROJECT_DIR + "raw/{sample}_{mate}.raw.fq.gz", sample = PROJECTS_SAMPLES, mate = [1, 2])
TARGET_RAW = TARGET_EXT_DOWNLAOD +  TARGET_EXT_RAW + TARGET_RAW_LOCAL + TARGET_DEMUX + TARGET_RAW_PROJECTS

# QC
TARGET_BEFORE_FASTQC = expand(QC_DIR + "before/{read}.raw_fastqc.zip", read = READS)
TARGET_BEFORE_MULTIQC = [QC_DIR + "before/multiqc_report.html"]
TARGET_TRIMMOMATIC = expand(QC_DIR + "after/{read}.qc.fq.gz", read = READS)
TARGET_AFTER_FASTQC = expand(QC_DIR + "after/{read}.qc_fastqc.zip", read = READS)
TARGET_AFTER_MULTIQC = [QC_DIR + "after/multiqc_report.html"]
TARGET_FINAL_QC = [QC_DIR + "final/final_samples.txt"]
TARGET_QC = (
        TARGET_BEFORE_FASTQC + TARGET_BEFORE_MULTIQC +
        TARGET_TRIMMOMATIC + TARGET_AFTER_FASTQC +
        TARGET_AFTER_MULTIQC + TARGET_FINAL_QC
)

# Denoising
denoising_methods_d = {
        "otu_pipits"        : DENOISING_DIR + ".denoised.pipits.done",
        "asv_dada2"                : DENOISING_DIR + ".denoised.dada2.done",
}
TARGET_DENOISING_METHOD = [denoising_methods_d[DENOISING_PARAMS["denoising_method"]]]
TARGET_REPSEQ = [DENOISING_DIR + "denoised.nwk"] + [DENOISING_DIR + "denoised.aligned.fasta"]
TARGET_DENOISING = TARGET_DENOISING_METHOD + TARGET_REPSEQ

# Phylotyping
phylotyping_methods_d = {
        "qiime2_blast"        : PHYLOTYPING_DIR + ".phylotyped.qiime2_blast.done",
        "qiime2_nb"                : PHYLOTYPING_DIR + ".phylotyped.qiime2_nb.done"
}
TARGET_PHYLOTYPING_METHOD = [phylotyping_methods_d[PHYLOTYPING_PARAMS["sequence_classifier"]]]
TARGET_PHYLOTYPING_CONVERT = [PHYLOTYPING_DIR + "phylotyped.csv"]
TARGET_PHYLOTYPING = [TARGET_PHYLOTYPING_METHOD + TARGET_PHYLOTYPING_CONVERT]

# Feature generation
TARGET_FEATURES = [FEATURES_DIR + "features.csv", FEATURES_DIR + "phyloseq.rds"]

# Analysis
TARGET_CORRELATION_PREP = [CORRELATION_DIR + ".prepare_groupwise_correlation.done"]
TARGET_CORRELATION_METHOD = [f'{CORRELATION_DIR}.correlation.{ANALYSIS_PARAMS["correlation_method"]}.done']
TARGET_CORRELATION = TARGET_CORRELATION_PREP + TARGET_CORRELATION_METHOD
TARGET_STATISTICS = [STATISTICS_DIR + "stat.csv"]
TARGET_ML = [ML_DIR + "ml.RData"]
TARGET_ANALYSIS = TARGET_CORRELATION + TARGET_STATISTICS + TARGET_ML

TARGET_REPORT_INPUT = [INPUT_DIR + "report.html"]
TARGET_REPORT_QC = [QC_DIR + "report.html"]
TARGET_REPORT_DENOISING = [DENOISING_DIR + "report.html"]
TARGET_REPORT_PHYLOTYPING = [PHYLOTYPING_DIR + "report.html"]
TARGET_REPORT_FEATURES = [FEATURES_DIR + "report.html"]
TARGET_REPORT_CORRELATIONS = [CORRELATION_DIR + "report.html"]
TARGET_REPORT_ML = [ML_DIR + "report.html"]
TARGET_REPORT_STATISTICS = [STATISTICS_DIR + "report.html"]
TARGET_REPORT_ANALYSIS = TARGET_REPORT_CORRELATIONS + TARGET_REPORT_ML + TARGET_REPORT_STATISTICS
TARGET_REPORT = TARGET_REPORT_INPUT + TARGET_REPORT_QC + TARGET_REPORT_DENOISING + TARGET_REPORT_PHYLOTYPING \
        + TARGET_REPORT_FEATURES + TARGET_REPORT_ANALYSIS

TARGET_SUMMARY = [SUMMARY_DIR + "report.html"]
TARGET_REPORT_ALL = [SUMMARY_DIR + "all.html"]

# misc targets
TARGET_PROJECT_JSON = [INPUT_DIR + "project.json.validated"]
TARGET_CHECKSUMS = [INPUT_DIR + "reads/checksums.csv"]
TARGET_MISC = TARGET_PROJECT_JSON

# pooling all targets except cleaning
TARGET_ALL = TARGET_MISC + TARGET_RAW + TARGET_QC + TARGET_REPORT_ALL\
        + TARGET_DENOISING + TARGET_PHYLOTYPING + TARGET_FEATURES \
        + TARGET_ANALYSIS + TARGET_REPORT + TARGET_SUMMARY + TARGET_CHECKSUMS

# this is the final target rule executed by default
TARGET_ALL_CLEAN = PROJECT_DIR + ".clean.done"
