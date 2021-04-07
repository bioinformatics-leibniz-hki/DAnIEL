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

def correlation_groups(wildcards):
        """
        Snakemake input function to get all correlation groups after checkpoint prepare_groupwise_correlation
        """
        with open(checkpoints.prepare_groupwise_correlation.get().output["correlation_groups"]) as f:
                groups = f.read().splitlines()
                return groups

def correlation_groups_raw_features(wildcards):
        return [f'{CORRELATION_DIR}{group}/features.raw.csv' for group in correlation_groups(wildcards)]

def correlation_groups_done(wildcards, method):
        return [f'{CORRELATION_DIR}{group}/.correlation.{method}.done' for group in correlation_groups(wildcards)]

checkpoint prepare_groupwise_correlation:
        """
        Checkpoint becasue it is not clear if there are enough sampels for all groups
        """
        input:
                features_csv = FEATURES_DIR + "features.csv",
                features_raw_csv = FEATURES_DIR + "features.raw.csv",
                samples_csv = INPUT_DIR + "samples.csv",
        output:
                done = CORRELATION_DIR + ".prepare_groupwise_correlation.done",
                correlation_groups = CORRELATION_DIR + "groups.txt"
        params:
                correlation_dir = CORRELATION_DIR,
                correlation_grouping = ANALYSIS_PARAMS["correlation_grouping"],
                min_samples = ANALYSIS_PARAMS["correlation_min_samples"],
                min_features = ANALYSIS_PARAMS["correlation_min_features"],
        shell:
                """
                source deactivate
                prepare_groupwise_correlation.R \
                        --samples-meta-csv {input.samples_csv} \
                        --features-csv {input.features_csv} \
                        --features-raw-csv {input.features_raw_csv} \
                        --correlation-dir {params.correlation_dir} \
                        --correlation-grouping {params.correlation_grouping} \
                        --min-features {params.min_features} \
                        --min-samples {params.min_samples} &&
                touch {output.done}
                """

rule correlation_sparcc:
        input:
                correlation_groups = lambda x: correlation_groups_done(x, method = "sparcc")
        output:
                CORRELATION_DIR + ".correlation.sparcc.done"
        shell:
                """
                touch {output}
                """

rule correlation_banocc:
        input:
                correlation_groups = lambda x: correlation_groups_done(x, method = "banocc")
        output:
                CORRELATION_DIR + ".correlation.banocc.done"
        shell:
                """
                touch {output}
                """

rule correlation_spearman:
        input:
                correlation_groups = lambda x: correlation_groups_done(x, method = "spearman")
        output:
                CORRELATION_DIR + ".correlation.spearman.done"
        shell:
                """
                touch {output}
                """

rule correlation_sparcc_group:
        input:
                features_raw_csv = CORRELATION_DIR + "{correlation_group}/features.raw.csv",
                prepare_correlation = CORRELATION_DIR + ".prepare_groupwise_correlation.done"
        output:
                done = CORRELATION_DIR + "{correlation_group}/.correlation.sparcc.done"
        params:
                # must be equal to bootstraps and iterations
                repetitions = ANALYSIS_PARAMS["sparcc_repetitions"],
                dir = CORRELATION_DIR + "{correlation_group}/"
        threads: 10
        conda:
                "../envs/analysis.conda_env.yml"
        shell:
                """
                mkdir -p "{params.dir}bootstraps"

                conda_prefix=$CONDA_PREFIX

                source deactivate
                    csv_to_tsv_biom.R \
                        -i "{input.features_raw_csv}" \
                        -o "{params.dir}counts.tsv"
                source activate $conda_prefix

                fastspar \
                        --yes \
                        --iterations {params.repetitions} \
                        --otu_table "{params.dir}counts.tsv" \
                        --correlation "{params.dir}cor.tsv" \
                        --covariance "{params.dir}cov.tsv"

                fastspar_bootstrap \
                        --otu_table "{params.dir}counts.tsv" \
                        --number {params.repetitions} \
                        --prefix "{params.dir}bootstraps/counts"

                parallel --jobs {threads} fastspar \
                        --yes \
                        --iterations {params.repetitions} \
                        --otu_table "{params.dir}bootstraps/counts_{{}}.tsv" \
                        --correlation "{params.dir}bootstraps/cor_{{}}.tsv" \
                        --covariance "{params.dir}bootstraps/cov_{{}}.tsv" \
                        ::: $(seq 0 {params.repetitions} | head -n -1)

                fastspar_pvalues \
                        --otu_table "{params.dir}counts.tsv" \
                        --correlation "{params.dir}cor.tsv" \
                        --prefix "{params.dir}bootstraps/cor_" \
                        --permutations {params.repetitions} \
                        --outfile "{params.dir}pval.tsv"

                rm -rf "{params.dir}bootstraps"

                source deactivate
                summarize_sparcc.R \
                        --cor-tsv "{params.dir}cor.tsv" \
                        --pval-tsv "{params.dir}pval.tsv" \
                        --out-csv "{params.dir}results.csv"
                source activate $conda_prefix

                # always continue reporting
                touch "{output}"
                """

rule correlation_banocc_group:
        input:
                features_raw_csv = CORRELATION_DIR + "{correlation_group}/features.raw.csv",
                prepare_correlation = CORRELATION_DIR + ".prepare_groupwise_correlation.done"
        output:
                done = CORRELATION_DIR + "{correlation_group}.correlation.banocc.done"
        params:
                dir = CORRELATION_DIR + "{correlation_group}",
                chains = ANALYSIS_PARAMS["banocc_chains"],
                iters = ANALYSIS_PARAMS["banocc_iters"],
                warmup = ANALYSIS_PARAMS["banocc_warmup"],
                alpha = ANALYSIS_PARAMS["banocc_alpha"]
        threads: 10
        shell:
                """
                source deactivate
                banocc.R \
                        --features-raw-csv "{input.features_raw_csv}" \
                        --results-csv "{params.dir}results.csv" \
                        --convergence-plot "{params.dir}convergence.pdf" \
                        --stat-summary-csv "{params.dir}banocc_summary.csv" \
                        --chains {params.chains} \
                        --iters {params.iters} \
                        --warmup {params.warmup} \
                        --conf-alpha {params.alpha} \
                        --out-rdata {params.dir}environment.RData \
                        --out-rds {params.dir}banocc_model.rds \
                        --threads {threads}

                # always continue reporting
                touch "{output}"
                """

rule correlation_spearman_group:
        input:
                features_csv = CORRELATION_DIR + "{correlation_group}/features.csv",
                prepare_correlation = CORRELATION_DIR + ".prepare_groupwise_correlation.done"
        output:
                done = CORRELATION_DIR + "{correlation_group}/.correlation.spearman.done"
        params:
                dir = CORRELATION_DIR + "{correlation_group}/"
        shell:
                """
                source deactivate
                correlation.R \
                        --features-csv "{input.features_csv}" \
                        --results-csv  "{params.dir}results.csv" \
                        --method spearman

                test -f "{params.dir}results.csv" && \
                        touch "{output}"
                """

rule statistics:
        input:
                features_csv = FEATURES_DIR + "features.csv",
                samples_csv = INPUT_DIR + "samples.csv"
        output:
                out_csv = STATISTICS_DIR + "stat.csv"
        params:
                groupings = "###".join(ANALYSIS_PARAMS["analysis_groupings"])
        shell:
                """
                source deactivate
                statistics.R \
                        --features-csv {input.features_csv} \
                        --samples-csv {input.samples_csv} \
                        --groupings {params.groupings} \
                        --out-csv {output.out_csv}
                """

rule ml:
        input:
                features_csv = FEATURES_DIR + "features.csv",
                samples_csv = INPUT_DIR + "samples.csv"
        output:
                out_rdata = ML_DIR + "ml.RData"
        params:
                out_dir = ML_DIR,
                min_samples_per_class = 3,
                groupings = "###".join(ANALYSIS_PARAMS["analysis_groupings"])
        threads: 10
        shell:
                """
                mkdir -p "{params.out_dir}"
                source deactivate
                ml.R \
                        --features-csv {input.features_csv} \
                        --samples-csv {input.samples_csv} \
                        --out-rdata {output.out_rdata} \
                        --min-samples-per-class {params.min_samples_per_class} \
                        --groupings {params.groupings} \
                        --threads {threads}
                """
