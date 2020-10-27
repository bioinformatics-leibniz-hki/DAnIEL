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

# No conda envs here because external dependencies needed (ggnewscale and danielLib)
# Use base env instead. Still reproducible due to docker container

rule input_report:
        input:
                target = TARGET_QC
        output:
                out_html = INPUT_DIR + "report.html"
        params:
                in_rmd = REPORT_DIR + "input.Rmd",
                project_dir = PROJECT_DIR,
                db_dir = DB_DIR,
                tmp_dir = INPUT_DIR
        shell:
                """
                source deactivate
                report.R \
                        --in-rmd {params.in_rmd} \
                        --project-dir {params.project_dir} \
                        --db-dir {params.db_dir} \
                        --tmp-dir {params.tmp_dir} \
                        --out-html {output.out_html}
                """

rule qc_report:
        input:
                target = TARGET_QC
        output:
                out_html = QC_DIR + "report.html"
        params:
                in_rmd = REPORT_DIR + "qc.Rmd",
                project_dir = PROJECT_DIR,
                db_dir = DB_DIR,
                tmp_dir = QC_DIR
        shell:
                """
                source deactivate
                report.R \
                        --in-rmd {params.in_rmd} \
                        --project-dir {params.project_dir} \
                        --db-dir {params.db_dir} \
                        --tmp-dir {params.tmp_dir} \
                        --out-html {output.out_html}
                """

rule denoising_report:
        input:
                target = TARGET_DENOISING
        output:
                out_html = DENOISING_DIR + "report.html"
        params:
                in_rmd = REPORT_DIR + "denoising.Rmd",
                project_dir = PROJECT_DIR,
                db_dir = DB_DIR,
                tmp_dir = DENOISING_DIR
        shell:
                """
                source deactivate
                report.R \
                        --in-rmd {params.in_rmd} \
                        --project-dir {params.project_dir} \
                        --db-dir {params.db_dir} \
                        --tmp-dir {params.tmp_dir} \
                        --out-html {output.out_html}
                """

rule phylotyping_report:
        input:
                target = TARGET_PHYLOTYPING
        output:
                out_html = PHYLOTYPING_DIR + "report.html"
        params:
                in_rmd = REPORT_DIR + "phylotyping.Rmd",
                project_dir = PROJECT_DIR,
                db_dir = DB_DIR,
                tmp_dir = PHYLOTYPING_DIR
        shell:
                """
                source deactivate
                report.R \
                        --in-rmd {params.in_rmd} \
                        --project-dir {params.project_dir} \
                        --db-dir {params.db_dir} \
                        --tmp-dir {params.tmp_dir} \
                        --out-html {output.out_html}
                """

rule features_report:
        input:
                target = TARGET_FEATURES
        output:
                out_html = FEATURES_DIR + "report.html"
        params:
                in_rmd = REPORT_DIR + "features.Rmd",
                project_dir = PROJECT_DIR,
                db_dir = DB_DIR,
                tmp_dir = FEATURES_DIR
        shell:
                """
                source deactivate
                report.R \
                        --in-rmd {params.in_rmd} \
                        --project-dir {params.project_dir} \
                        --db-dir {params.db_dir} \
                        --tmp-dir {params.tmp_dir} \
                        --out-html {output.out_html}
                """

rule correlation_report:
        input:
                target = TARGET_CORRELATION_METHOD
        output:
                out_html = CORRELATION_DIR + "report.html"
        params:
                in_rmd = REPORT_DIR + "correlations.Rmd",
                project_dir = PROJECT_DIR,
                db_dir = DB_DIR,
                tmp_dir = CORRELATION_DIR
        shell:
                """
                source deactivate
                report.R \
                        --in-rmd {params.in_rmd} \
                        --project-dir {params.project_dir} \
                        --db-dir {params.db_dir} \
                        --tmp-dir {params.tmp_dir} \
                        --out-html {output.out_html}
                """

rule ml_report:
        input:
                target = TARGET_ML
        output:
                out_html = ML_DIR + "report.html"
        params:
                in_rmd = REPORT_DIR + "ml.Rmd",
                project_dir = PROJECT_DIR,
                db_dir = DB_DIR,
                tmp_dir = ML_DIR
        shell:
                """
                source deactivate
                report.R \
                        --in-rmd {params.in_rmd} \
                        --project-dir {params.project_dir} \
                        --db-dir {params.db_dir} \
                        --tmp-dir {params.tmp_dir} \
                        --out-html {output.out_html}
                """

rule statistics_report:
        input:
                target = TARGET_STATISTICS
        output:
                out_html = STATISTICS_DIR + "report.html"
        params:
                in_rmd = REPORT_DIR + "statistics.Rmd",
                project_dir = PROJECT_DIR,
                db_dir = DB_DIR,
                tmp_dir = STATISTICS_DIR
        shell:
                """
                source deactivate
                report.R \
                        --in-rmd {params.in_rmd} \
                        --project-dir {params.project_dir} \
                        --db-dir {params.db_dir} \
                        --tmp-dir {params.tmp_dir} \
                        --out-html {output.out_html}
                """

rule summary_report:
        input:
                target = TARGET_REPORT
        output:
                out_html = SUMMARY_DIR + "report.html"
        params:
                in_rmd = REPORT_DIR + "summary.Rmd",
                project_dir = PROJECT_DIR,
                db_dir = DB_DIR,
                tmp_dir = SUMMARY_DIR
        shell:
                """
                source deactivate
                report.R \
                        --in-rmd {params.in_rmd} \
                        --project-dir {params.project_dir} \
                        --db-dir {params.db_dir} \
                        --tmp-dir {params.tmp_dir} \
                        --out-html {output.out_html}
                """

rule all_report:
        input:
                target = TARGET_REPORT
        output:
                out_html = SUMMARY_DIR + "all.html"
        params:
                in_rmd = REPORT_DIR + "all.Rmd",
                project_dir = PROJECT_DIR,
                reports_dir = REPORT_DIR,
                db_dir = DB_DIR,
                tmp_dir = SUMMARY_DIR
        shell:
                """
                source deactivate
                report.R \
                        --in-rmd {params.in_rmd} \
                        --project-dir {params.project_dir} \
                        --reports-dir {params.reports_dir} \
                        --db-dir {params.db_dir} \
                        --tmp-dir {params.tmp_dir} \
                        --out-html {output.out_html}
                """
