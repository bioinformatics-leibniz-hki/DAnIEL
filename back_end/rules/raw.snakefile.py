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

rule demultiplex:
        input:
                reads_dir = INPUT_DIR + "reads",
                samples_csv = INPUT_DIR + "samples.csv"
        output:
                samples = TARGET_DEMUX
        params:
                raw_dir = RAW_DIR
        conda:
                "../envs/raw.conda_env.yml"
        shell:
                """
                mkdir -p {params.raw_dir}
                demux_by_header.R {input.samples_csv} {input.reads_dir} {params.raw_dir}
                """

rule parse_input_files:
        """
        Parse input files
        - remove read comment and comment after + (SRA format does not work with PIPITS)
        """
        input:
                read_file = INPUT_DIR + "reads/{sample}_{mate}.raw.fq.gz"
        output:
                read_file = RAW_DIR + "{sample}_{mate}.raw.fq.gz"
        params:
                bioawk_path = SCRIPT_DIR + "parse_fastq.bioawk"
        wildcard_constraints:
                sample = "|".join(SAMPLES)
        conda:
                "../envs/raw.conda_env.yml"
        shell:
                """
                zcat {input.read_file} \
                        | bioawk -c fastx -f {params.bioawk_path} \
                        | gzip \
                        > {output.read_file}
                """

rule rename_sra:
        input:
                read = INPUT_DIR + "reads/{accession}_{mate}.fastq.gz",
        output:
                read = INPUT_DIR + "reads/{accession}_{mate}.raw.fq.gz"
        wildcard_constraints:
                accession = "|".join(DOWNLOAD_SAMPLES)
        shell:
                """
                mv {input} {output}
                """

rule download_sra_paired:
        output:
                read1 = INPUT_DIR + "reads/{accession}_1.fastq.gz",
                read2 = INPUT_DIR + "reads/{accession}_2.fastq.gz"
        wildcard_constraints:
                accession = "|".join(DOWNLOAD_SAMPLES)
        params:
                accession = "{accession}",
                outdir = INPUT_DIR + "reads/"
        conda:
                "../envs/raw.conda_env.yml"
        threads:
                1
        log:
                INPUT_DIR + "download_sra_paired_{accession}.log"
        shell:
                """
                mkdir -p {params.outdir}
                grabseqs sra -r 3 -t {threads} -o {params.outdir} {params.accession}
                """
