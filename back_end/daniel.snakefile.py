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

import functools as ft
import os

import pandas as pd

# Disallow slashes in wildcards. They are only used to separate directories.
wildcard_constraints:
    sample = "[^/]+",


if not config:
    raise SystemExit("No config file specified. Aborted.")

VERSION = "CI"

# Max number of cores available for snakemake
# Will be partitioned to all concurrent rules
# Same as argument snakemake --cores
MAX_THREADS = int(os.environ["DANIEL_THREADS"])

# some rules do not profit using all threads
# start multiple instances of this rule instead
# Useful for per sample QC
SMALL_THREADS = min(8, MAX_THREADS)

USERDAT_DIR = os.environ["DANIEL_USERDAT_DIR"] + "/"
DB_DIR = os.environ["DANIEL_DB_DIR"] + "/"
SCRIPT_DIR = os.environ["DANIEL_SCRIPT_DIR"] + "/"
REPORT_DIR = SCRIPT_DIR + "../reports/"

# samples


def str_to_list(x):
    if x == "":
        return []
    if type(x) == str:
        return [x]
    return x


META_SAMPLES = str_to_list(config["input"]["meta_samples"])
LOCAL_SAMPLES = str_to_list(config["input"]["local_samples"])
PROJECTS = []
PROJECTS_SAMPLES = []
try:
    PROJECTS = list(config["input"]["projects"].keys())
    PROJECTS_SAMPLES = [config["input"]["projects"]
                        [x]["samples"] for x in PROJECTS]
    # pool unique samples
    PROJECTS_SAMPLES = list(
        set(ft.reduce(lambda x, y: x + y, PROJECTS_SAMPLES)))
except:
    pass
SRA_SAMPLES = str_to_list(config["input"]["sra_ids"])
DOWNLOAD_SAMPLES = list(set(SRA_SAMPLES + PROJECTS_SAMPLES))
MUXED_SAMPLES = str_to_list(config["input"]["muxed_samples"])
SAMPLES = sorted(
    list(
        set(LOCAL_SAMPLES + SRA_SAMPLES + PROJECTS_SAMPLES + MUXED_SAMPLES)
        & set(META_SAMPLES)
    )
)

FWD_READS = [x + "_1" for x in SAMPLES]
REV_READS = [x + "_2" for x in SAMPLES]
READS = FWD_READS + REV_READS

# Last step (analysis) determines reuired upstream steps


def get_selected_params_name(cur_step, next_step):
    res = config["params"][next_step + "_params"][selected_params[next_step]][
        "selected_" + cur_step + "_params"
    ]
    return res


def get_selected_params_dir(step):
    res = PROJECT_DIR + step + "/" + \
        selected_params[step].replace(" ", "_") + "/"
    return res


selected_params = {}
selected_params["analysis"] = config["selected_analysis_params"]
selected_params["features"] = get_selected_params_name("features", "analysis")
selected_params["phylotyping"] = get_selected_params_name(
    "phylotyping", "features")
selected_params["denoising"] = get_selected_params_name(
    "denoising", "phylotyping")
selected_params["qc"] = get_selected_params_name("qc", "denoising")

PROJECT_DIR = USERDAT_DIR + config["project_id"] + "/"
INPUT_DIR = PROJECT_DIR + "input/"
RAW_DIR = PROJECT_DIR + "raw/"

QC_DIR = get_selected_params_dir("qc")
DENOISING_DIR = get_selected_params_dir("denoising")
PHYLOTYPING_DIR = get_selected_params_dir("phylotyping")
FEATURES_DIR = get_selected_params_dir("features")

ANALYSIS_DIR = get_selected_params_dir("analysis")
CORRELATION_DIR = ANALYSIS_DIR + "correlation/"
ML_DIR = ANALYSIS_DIR + "ml/"
STATISTICS_DIR = ANALYSIS_DIR + "statistics/"
SUMMARY_DIR = ANALYSIS_DIR + "summary/"


def get_params(x): return config["params"][x + "_params"][selected_params[x]]


QC_PARAMS = get_params("qc")
DENOISING_PARAMS = get_params("denoising")
PHYLOTYPING_PARAMS = get_params("phylotyping")
FEATURES_PARAMS = get_params("features")
ANALYSIS_PARAMS = get_params("analysis")

correlation_grouping = ANALYSIS_PARAMS["correlation_grouping"]
CORRELATION_GROUPS = (
    "all"
    if correlation_grouping == "all"
    else pd.read_csv(INPUT_DIR + "/samples.csv")[correlation_grouping].unique()
)

shell.executable("/bin/bash")
shell.prefix("export PATH=%s:$PATH; " % SCRIPT_DIR)

print("This is DAnIEL " + VERSION, file=sys.stderr)
print("Script dir: " + SCRIPT_DIR, file=sys.stderr)
print("Database dir: " + DB_DIR, file=sys.stderr)
print(
    "%i samples used: %s ..." % (len(SAMPLES), ", ".join(SAMPLES[0:5])), file=sys.stderr
)


include: "rules/targets.snakefile.py"


rule all:
    input:
        TARGET_ALL_CLEAN,
    params:
        prj_dir = PROJECT_DIR,
    shell:
        """
        # remove empty log files
        find {params.prj_dir} -size 0 -name *.log -delete
        """


include: "rules/misc.snakefile.py"
include: "rules/raw.snakefile.py"
include: "rules/qc.snakefile.py"
include: "rules/denoising.snakefile.py"
include: "rules/phylotyping.snakefile.py"
include: "rules/feature_generation.snakefile.py"
include: "rules/analysis.snakefile.py"
include: "rules/report.snakefile.py"
include: "rules/clean.snakefile.py"
