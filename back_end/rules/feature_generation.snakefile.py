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

rule generate_features:
        input:
                target_phylotyping = TARGET_PHYLOTYPING,
                samples_csv = INPUT_DIR + "samples.csv",
                phylotyped_csv = PHYLOTYPING_DIR + "phylotyped.csv"
        params:
                denoised_csv = DENOISING_DIR + "denoised.csv",
                taxonomic_rank = FEATURES_PARAMS["taxonomic_rank"],
                normalization_method = FEATURES_PARAMS["normalization_method"],
                min_abundance = FEATURES_PARAMS["min_abundance"],
                min_prevalence = FEATURES_PARAMS["min_prevalence"],
                group_prevalence = FEATURES_PARAMS["group_prevalence"],
                unknown_strategy = FEATURES_PARAMS["unknown_strategy"]
        output:
                features_norm_csv = FEATURES_DIR + "features.csv",
                features_raw_csv = FEATURES_DIR + "features.raw.csv",
                features_tss_csv = FEATURES_DIR + "features.tss.csv",
                features_meta_csv = FEATURES_DIR + "features.meta.csv"
        shell:
                """
                source deactivate
                generate_features.R \
                        --denoised-csv {params.denoised_csv} \
                        --phylotyped-csv {input.phylotyped_csv} \
                        --samples-meta-csv {input.samples_csv} \
                        --group-prevalence {params.group_prevalence} \
                        --out-norm-csv {output.features_norm_csv} \
                        --out-raw-csv {output.features_raw_csv} \
                        --out-tss-csv {output.features_tss_csv} \
                        --out-meta-csv {output.features_meta_csv} \
                        --taxonomic-rank {params.taxonomic_rank} \
                        --min-abundance-perc {params.min_abundance} \
                        --min-prevalence-perc {params.min_prevalence} \
                        --normalization-method {params.normalization_method} \
                        --unknown-strategy {params.unknown_strategy}
                """

rule generate_phyloseq:
        input:
                features_csv = FEATURES_DIR + "features.tss.csv",
                samples_csv = INPUT_DIR + "samples.csv"
        output:
                out_rds = FEATURES_DIR + "phyloseq.rds"
        params:
                taxonomy_csv = DB_DIR + "phylogenies/" + FEATURES_PARAMS["phylo_database"] + "/taxonomy.csv.gz",
                tree_nwk = DB_DIR + "phylogenies/" + FEATURES_PARAMS["phylo_database"] + "/" + FEATURES_PARAMS["taxonomic_rank"] +  ".newick",
                taxonomic_rank = FEATURES_PARAMS["taxonomic_rank"]
        shell:
                """
                source deactivate
                create_phyloseq_object.R \
                        --features-csv {input.features_csv} \
                        --taxonomic-rank {params.taxonomic_rank} \
                        --samples-csv {input.samples_csv} \
                        --taxonomy-csv {params.taxonomy_csv} \
                        --tree-nwk {params.tree_nwk} \
                        --out-rds {output.out_rds}
                """
