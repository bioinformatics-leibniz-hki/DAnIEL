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

rule create_qc_dir:
        output:
                QC_DIR
        shell:
                "mkdir -p {output}"

rule create_trimming_fasta:
        input:
                INPUT_DIR + "project.json.validated"
        output:
                QC_DIR + "trimm_seqs.fna"
        params:
                adapter_fasta = QC_PARAMS["adapter_fasta"],
                additional_adapter_files = QC_PARAMS["additional_adapter_files"],
                include_revcomp_primers = QC_PARAMS["include_revcomp_primers"] and QC_PARAMS["adapter_fasta"] != "",
                adapter_dir = DB_DIR + "adapters/"
        run:
            import os
            with open(str(output), "a+") as adapter_file:
                for f in params["additional_adapter_files"]:
                    f = params["adapter_dir"] + f
                    adapter_file.write(open(f).read())

                if len(params["adapter_fasta"]) > 0:
                    adapter_file.write(params["adapter_fasta"])

            if params["include_revcomp_primers"]:
                os.system(f"add_fasta_reverse_comp.sh <(echo -e '{params.adapter_fasta}') {output}")


rule before_qc:
        """
        QC check of raw reads before trimming
        """
        input:
                RAW_DIR + "{sample}_{mate}.raw.fq.gz"
        output:
                QC_DIR + "before/{sample}_{mate}.raw_fastqc.zip"
        params:
                qc_before_dir = QC_DIR + "before/"
        conda:
                "../envs/qc.conda_env.yml"
        shell:
                """
                mkdir -p {params.qc_before_dir}
                fastqc {input} -o {params.qc_before_dir}
                """

rule after_qc:
        """
        QC check of qc reads after trimming
        """
        input:
                QC_DIR + "after/{sample}_{mate}.qc.fq.gz"
        output:
                QC_DIR + "after/{sample}_{mate}.qc_fastqc.zip"
        params:
                qc_after_dir = QC_DIR + "after/"
        conda:
                "../envs/qc.conda_env.yml"
        shell:
                """
                mkdir -p {params.qc_after_dir}
                fastqc {input} -o {params.qc_after_dir}
                """

rule before_multiqc:
        input:
                TARGET_BEFORE_FASTQC
        output:
                QC_DIR + "before/multiqc_report.html"
        params:
                "--module fastqc"
        log:
                QC_DIR + "before/multiqc.log"
        wrapper:
                "0.38.0/bio/multiqc"

rule after_multiqc:
        input:
                TARGET_AFTER_FASTQC
        output:
                QC_DIR + "after/multiqc_report.html"
        params:
                "--module fastqc --module cutadapt"
        log:
                QC_DIR + "after/multiqc.log"
        wrapper:
                "0.38.0/bio/multiqc"

rule cutadapt:
        input:
                forward = RAW_DIR + "{sample}_1.raw.fq.gz",
                reverse = RAW_DIR + "{sample}_2.raw.fq.gz",
                trimm_seqs = QC_DIR + "trimm_seqs.fna"
        output:
                forward = QC_DIR + "after/{sample}_1.qc.fq.gz",
                reverse = QC_DIR + "after/{sample}_2.qc.fq.gz"
        params:
                min_quality_leading = QC_PARAMS["min_quality_leading"],
                min_quality_trailing = QC_PARAMS["min_quality_trailing"],
                qc_error_rate = QC_PARAMS["qc_error_rate"],
                min_read_length = QC_PARAMS["min_read_length"]
        threads:
                MAX_THREADS
        conda:
                "../envs/qc.conda_env.yml"
        shell:
        # info log files should be disabled in production due to space issues. Then, cores can be increased from 1 to max
        # See: https://benjjneb.github.io/dada2/ITS_workflow.html
                """
                cutadapt \
                        --quality-cutoff {params.min_quality_leading},{params.min_quality_trailing} \
                        --error-rate {params.qc_error_rate} \
                        --minimum-length {params.min_read_length} \
                        --cores {threads} \
                        -b file:{input.trimm_seqs} \
                        -B file:{input.trimm_seqs} \
                        -o {output.forward} \
                        -p {output.reverse} \
                        {input.forward} {input.reverse}
                """

checkpoint final_qc:
        """
        Final QC check to exclude samples
        This is a checkpoint with triggers reevaluation of DAG to update further
        targets like denoising.
        """
        input:
                target = TARGET_AFTER_MULTIQC,
                project_json = INPUT_DIR + "project.json"
        output:
                final_samples = QC_DIR + "final/final_samples.txt",
                done = QC_DIR + "final/.done"
        params:
                qc_dir = QC_DIR,
                qc_exclusion_criteria = ",".join(QC_PARAMS["qc_exclusion_criteria"]),
                min_qc_read_count = QC_PARAMS["min_qc_read_count"]
        shell:
                """
                source deactivate

                if ["{params.qc_exclusion_criteria}" != "OrderedDict()"]; then
                    exclusion_arg="--qc-exclusion-criteria {params.qc_exclusion_criteria}"
                fi

                qc_tests.R \
                        --qc-dir {params.qc_dir} \
                        --min-qc-read-count {params.min_qc_read_count} \
                        $exclusion_arg
                touch {output.done}
                """
