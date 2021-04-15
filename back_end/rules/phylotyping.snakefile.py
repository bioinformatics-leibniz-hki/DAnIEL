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

rule create_phylotyping_dir:
        output:
                PHYLOTYPING_DIR
        shell:
                "mkdir -p {output}"

rule phylotyping_qiime2_naive_bayes:
        input:
                target_denoising = TARGET_DENOISING
        output:
                done = PHYLOTYPING_DIR + ".phylotyped.qiime2_nb.done",
        params:
                denoised_fasta_path = DENOISING_DIR + "/tree/repseqs.qza",
                classifier = DB_DIR + "reference_db/" + PHYLOTYPING_PARAMS["ref_database"] + "/refdb.ITS.classifier.qza",
                phylotyping_dir = PHYLOTYPING_DIR,
                phylotyped_qza = PHYLOTYPING_DIR + "phylotyped.qza"
        threads:
                MAX_THREADS
        conda:
                "../envs/qiime2.conda_env.yml"
        shell:
                """
                qiime feature-classifier classify-sklearn \
                        --i-classifier {params.classifier} \
                        --i-reads {params.denoised_fasta_path} \
                        --o-classification {params.phylotyped_qza} && \
                qiime tools export \
                        --input-path {params.phylotyped_qza} \
                        --output-path {params.phylotyping_dir} && \
                touch {output.done}
                """

rule phylotyping_qiime2_consensus_blast:
        input:
                target_denoising = TARGET_DENOISING
        output:
                done = PHYLOTYPING_DIR + ".phylotyped.qiime2_blast.done",
        params:
                denoised_fasta_path = DENOISING_DIR + "/tree/repseqs.qza",
                ref_fasta_qza = DB_DIR + "reference_db/" + PHYLOTYPING_PARAMS["ref_database"] + "/refdb.fasta.qza",
                ref_tax_qza = DB_DIR + "reference_db/" + PHYLOTYPING_PARAMS["ref_database"] + "/refdb.tax.qza",
                frac_identity = PHYLOTYPING_PARAMS["frac_identity"],
                phylotyping_dir = PHYLOTYPING_DIR,
                phylotyped_qza = PHYLOTYPING_DIR + "phylotyped.qza"
        threads:
                MAX_THREADS
        conda:
                "../envs/qiime2.conda_env.yml"
        shell:
                """
                qiime feature-classifier classify-consensus-blast \
                        --i-query {params.denoised_fasta_path} \
                        --i-reference-reads {params.ref_fasta_qza} \
                        --i-reference-taxonomy {params.ref_tax_qza} \
                        --p-perc-identity {params.frac_identity} \
                        --o-classification {params.phylotyped_qza} && \
                qiime tools export \
                        --input-path {params.phylotyped_qza} \
                        --output-path {params.phylotyping_dir} \
                # Unify name of confidence score
                sed -i '1 s/Consensus/Confidence/' {params.phylotyping_dir}/taxonomy.tsv && \
                touch {output.done}
                """

rule convert_qiime2_taxonomy:
        input:
                target = TARGET_PHYLOTYPING_METHOD
        params:
                taxonomy_tsv = PROJECT_DIR + "phylotyping/{param_set}/taxonomy.tsv"
        output:
                taxonomy_csv = PROJECT_DIR + "phylotyping/{param_set}/phylotyped.csv"
        shell:
                """
                qiime_tax_to_csv.R \
                        --qiime2-tax-tsv-path {params.taxonomy_tsv} \
                        --phylotyped-csv-path {output.taxonomy_csv}
                """
