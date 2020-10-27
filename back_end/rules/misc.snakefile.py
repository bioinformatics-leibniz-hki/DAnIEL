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

rule validate_project_json:
        input:
                INPUT_DIR + "project.json"
        output:
                INPUT_DIR + "project.json.validated"
        shell:
                "validate_input_json.py {input} && touch {output}"

rule calculate_read_checksums:
        input:
                local_fastq = expand(INPUT_DIR + "reads/{sample}_{mate}.raw.fq.gz", mate = [1, 2], sample = LOCAL_SAMPLES),
                target_raw = TARGET_RAW
        output:
                checksums_csv = INPUT_DIR + "reads/checksums.csv"
        params:
                directory = INPUT_DIR + "reads"
        shell:
                """
                file_checksums.R --directory {params.directory} --out-csv {output.checksums_csv}
                """
