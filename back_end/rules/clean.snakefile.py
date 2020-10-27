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

rule clean:
        input:
                target_all = TARGET_ALL
        output:
                PROJECT_DIR + ".clean.done"
        shell:
                """
                # remove all read files
                find . | grep -E '\.fq\.gz$' | xargs rm

                # remove fastqc reports (redundant because of multiqc reports)
                find qc/*/*/*fastqc.* | xargs rm

                touch {output}
                """
