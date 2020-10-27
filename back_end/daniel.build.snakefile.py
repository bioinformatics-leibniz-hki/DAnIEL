#!/usr/bin/env snakemake -s

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
# This is a stub Snakefile used to create environments only
#

TARGETS = [
    "denoising.done",
    "analysis.done",
    "features.done",
    "qc.done",
    "multiqc.done",
    "qiime.done",
    "raw.done",
]


rule all:
    input:
        TARGETS,
    shell:
        "Build Snakefile Finished."


rule env_analysis:
    conda:
        "envs/analysis.conda_env.yml"
    output:
        "analysis.done",
    shell:
        "echo $CONDA_PREFIX > analysis.done"


rule env_denoising:
    conda:
        "envs/denoising.conda_env.yml"
    output:
        "denoising.done",
    shell:
        "echo $CONDA_PREFIX > denoising.done"


rule env_features:
    conda:
        "envs/features.conda_env.yml"
    output:
        "features.done",
    shell:
        "echo $CONDA_PREFIX > features.done"


rule env_multiqc:
    output:
        "multiqc.done",
    wrapper:
        "0.38.0/bio/multiqc"


rule env_qc:
    conda:
        "envs/qc.conda_env.yml"
    output:
        "qc.done",
    shell:
        "echo $CONDA_PREFIX > qc.done"


rule env_qiime:
    conda:
        "envs/qiime2.conda_env.yml"
    output:
        "qiime.done",
    shell:
        "echo $CONDA_PREFIX > qiime.done"


rule env_raw:
    conda:
        "envs/raw.conda_env.yml"
    output:
        "raw.done",
    shell:
        "echo $CONDA_PREFIX > raw.done"
