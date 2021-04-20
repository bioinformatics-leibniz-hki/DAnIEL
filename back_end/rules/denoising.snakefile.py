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

def denoising_fasta(wildcards):
        """
        Snakemake input function to get all denoised fasta paths after checkpoint final_qc
        """
        with open(checkpoints.final_qc.get().output["final_samples"]) as f:
                samples = f.read().splitlines()
                paths = expand(DENOISING_DIR + "{sample}/denoised.fasta", sample = samples)
                return paths

def denoising_csv(wildcards):
        """
        Snakemake input function to get all denoised csv paths after checkpoint final_qc
        """
        with open(checkpoints.final_qc.get().output["final_samples"]) as f:
                samples = f.read().splitlines()
                paths = expand(DENOISING_DIR + "{sample}/denoised.csv", sample = samples)
                return paths

rule repseq_tree:
        """
        Creates a rooted Newick tree of denoised sequences
        """
        input:
                TARGET_DENOISING_METHOD,
        output:
                repseq_tree = DENOISING_DIR + "denoised.nwk",
                repseq_msa = DENOISING_DIR + "denoised.aligned.fasta"
        params:
                denoised_fasta = DENOISING_DIR + "denoised.fasta",
                out_dir = DENOISING_DIR + "tree"
        conda:
                "../envs/qiime2.conda_env.yml"
        shell:
                """
                create_tree.sh {params.denoised_fasta} {params.out_dir} && \
                mv {params.out_dir}/tree.nwk {output.repseq_tree} && \
                mv {params.out_dir}/aligned-dna-sequences.fasta {output.repseq_msa} ||
                    touch {output.repseq_tree} {output.repseq_msa}
                """

rule filter_repseq_alignment:
    """
    Get prevalent representative sequences from MSA for fast displaying
    """
    input:
        repseq_msa = DENOISING_DIR + "denoised.aligned.fasta",
        denoised_csv = DENOISING_DIR  + "denoised.csv"
    output:
        out_fasta = DENOISING_DIR + "denoised.filtered.aligned.fasta"
    conda:
        "../envs/denoising.conda_env.yml"
    shell:
        """
        filter_prevalent_fasta.R \
                --n-seqs 50 \
                --in-denoised-csv {input.denoised_csv} \
                --in-fasta {input.repseq_msa} \
                --out-fasta {output.out_fasta}
        """


rule dada2_sample:
        """
        Run DADA2 for one sample
        """
        input:
                target_qc = TARGET_QC,
                fwd_qc_fastq = QC_DIR + "final/reads/{sample}_1.qc.fq.gz",
                rev_qc_fastq = QC_DIR + "final/reads/{sample}_2.qc.fq.gz"
        output:
                fasta_path = DENOISING_DIR + "{sample}/denoised.fasta",
                csv_path = DENOISING_DIR + "{sample}/denoised.csv"
        log:    DENOISING_DIR + "{sample}/log.txt"
        params:
                sample = "{sample}",
                out_dir = DENOISING_DIR + "{sample}/",
                min_read_length = QC_PARAMS["min_read_length"],
                max_n = DENOISING_PARAMS["max_n"],
                min_q = DENOISING_PARAMS["min_q"],
                max_ee = DENOISING_PARAMS["max_ee"],
                trunc_q = DENOISING_PARAMS["trunc_q"]
        conda:
                "../envs/denoising.conda_env.yml"
        shell:
                """
                dada2_sample.R \
                        --fwd-qc-fastq {input.fwd_qc_fastq} \
                        --rev-qc-fastq {input.rev_qc_fastq} \
                        --sample-name {params.sample} \
                        --out-dir {params.out_dir} \
                        --max-n {params.max_n} \
                        --min-q {params.min_q} \
                        --max-ee {params.max_ee} \
                        --trunc-q {params.trunc_q} \
                        --min-read-length {params.min_read_length} \
                        2> {log} || \
                        touch {output.fasta_path} {output.csv_path}
                """

rule dada2:
        """
        Final rule to summarize, filter and trim sample wise ASV results
        """
        input:
                fasta_paths = denoising_fasta,
                csv_paths = denoising_csv
        output:
                DENOISING_DIR + ".denoised.dada2.done"
        params:
                denoising_dir = DENOISING_DIR,
                its_region = DENOISING_PARAMS["its_region"]
        threads:
               MAX_THREADS
        conda:
                "../envs/denoising.conda_env.yml"
        shell:
                """
                dada2_combine_samples.R {params.denoising_dir}

                ITSx \
                    -i {params.denoising_dir}/raw_denoised.fasta \
                    -o {params.denoising_dir}/filtered \
                    --cpu {threads} \
                    --save_regions {params.its_region}

                # keep only fungal seqs with the selected subregion
                bioawk \
                    -c fastx \
                    '$name ~ /\"|F|\"/ {{print \">\"$name\"\\n\"$seq}}' \
                    {params.denoising_dir}/filtered.{params.its_region}.fasta \
                    | sed 's/|[A-Z]|ITS[12]//' \
                    > {params.denoising_dir}/denoised.fasta

                filter_denoised_counts.R \
                         {params.denoising_dir}/raw_denoised.csv \
                         {params.denoising_dir}/denoised.fasta \
                         {params.denoising_dir}/denoised.csv

                touch {output}
                """

rule pipits:
        """
        Final rule to run PIPITS
        """
        input:
                otu_table = DENOISING_DIR + "pipits/3-process/otu_table.txt",
                fasta_paths = DENOISING_DIR + "raw_denoised.fasta"
        output:
                done = DENOISING_DIR + ".denoised.pipits.done"
        params:
                denoising_dir = DENOISING_DIR
        conda:
                "../envs/denoising.conda_env.yml"
        shell:
                """
                pipits_combine_samples.R {params.denoising_dir}

                ln {params.denoising_dir}/raw_denoised.fasta \
                    {params.denoising_dir}/denoised.fasta

                ln {params.denoising_dir}/raw_denoised.csv \
                    {params.denoising_dir}/denoised.csv

                touch {output}
                """

rule pipits_parse_repseqs:
        """
        Parse PIPITS repseqs
        """
        input:
                fasta_path = DENOISING_DIR + "pipits/3-process/repseqs.fasta"
        output:
                fasta_path = DENOISING_DIR + "raw_denoised.fasta"
        params:
                bioawk_path = SCRIPT_DIR + "parse_fasta.bioawk"
        conda:
                "../envs/denoising.conda_env.yml"
        shell:
                """
                bioawk -c fastx -f {params.bioawk_path} {input.fasta_path} > {output.fasta_path}
                """

rule pipits_create_readpairs_list:
        input:
                samples = QC_DIR + "final/final_samples.txt"
        output:
                DENOISING_DIR + "pipits/readpairslist.txt"
        run:
                with open(input["samples"], "r") as samples_f:
                        samples = samples_f.read().splitlines()
                        with open(DENOISING_DIR + "pipits/readpairslist.txt", "w") as readpairs_f:
                                get_line = lambda x: x + "\t" + x + "_1.qc.fq.gz" + "\t" + x + "_2.qc.fq.gz"
                                readpairs_f.write("\n".join([get_line(x) for x in samples]))

rule pipits_seq_prep:
        input:
                readpairslist = DENOISING_DIR + "pipits/readpairslist.txt"
        output:
                DENOISING_DIR + "pipits/1-seqprep/prepped.fasta"
        params:
                pipits_dir = DENOISING_DIR + "pipits",
                clean_read_dir = QC_DIR + "final/reads"
        threads:
                MAX_THREADS
        conda:
                "../envs/denoising.conda_env.yml"
        shell:
                """
                mkdir -p {params.pipits_dir} && \
                pispino_seqprep \
                        -i {params.clean_read_dir} \
                        -l {input.readpairslist} \
                        -o {params.pipits_dir}/1-seqprep \
                        -t {threads}
                """

rule pipits_funits:
        input:
                DENOISING_DIR + "pipits/1-seqprep/prepped.fasta"
        output:
                DENOISING_DIR + "pipits/2-funits/ITS.fasta"
        params:
                pipits_dir = DENOISING_DIR + "pipits",
                its_region = DENOISING_PARAMS["its_region"]
        threads:
                MAX_THREADS
        conda:
                "../envs/denoising.conda_env.yml"
        shell:
                """
                mkdir -p {params.pipits_dir} && \
                pipits_funits \
                        -i {params.pipits_dir}/1-seqprep/prepped.fasta \
                        -o {params.pipits_dir}/2-funits \
                        -x {params.its_region} \
                        -t {threads} \
                        -v -r && \
                # work around: keep intermediate files and then removing them
                rm -rf {params.pipits_dir}/2-funits/intermediate
                """

rule pipits_process:
        input:
                its_fasta = DENOISING_DIR + "pipits/2-funits/ITS.fasta",
        output:
                otu_table = DENOISING_DIR + "pipits/3-process/otu_table.txt",
                fasta_path = DENOISING_DIR + "pipits/3-process/repseqs.fasta",
        params:
                out_dir = DENOISING_DIR + "pipits/3-process",
                db_dir = DB_DIR + "pipits_db",
                identity_threshold = DENOISING_PARAMS["identity_threshold"],
                include_singletons = DENOISING_PARAMS["include_singletons"]
        threads:
                MAX_THREADS
        conda:
                "../envs/denoising.conda_env.yml"
        shell:
                """
                # prevent pipits from downloading the data base
                # must be copied because pipits requires write access
                cp -a {params.db_dir} . && \
                pipits_process \
                        -i {input.its_fasta} \
                        -o {params.out_dir} \
                        -d {params.identity_threshold} \
                        -t {threads} \
                        $([[ "{params.include_singletons}" == "False" ]] \
                            && echo "--includeuniqueseqs") \
                        --unite 02.02.2019 && \
                # remove db link
                rm -rf pipits_db
                """
