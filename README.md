# DAnIEL <a href='https://sbi.hki-jena.de/daniel'><img src='front_end/www/img/DAnIEL_logo.png' align="right" height="80px" /></a>

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4146884.svg)](https://doi.org/10.5281/zenodo.4146884)
[![License](https://img.shields.io/badge/License-BSD%202--Clause-orange.svg)](https://opensource.org/licenses/BSD-2-Clause)
<!-- badges: end -->

A user-friendly web server for fungal ITS amplicon sequencing data

DAnIEL: *D*escribing, *An*alyzing and *I*ntegrating fungal *E*cology to effectively study the systems of *L*ife

## Features
* Analysis of paired-end ITS amplicon sequencing data in any web browser
* Statistics and machine learning between groups of samples
* Correlation networks
* Integration of existing cohorts from [NCBI SRA](https://www.ncbi.nlm.nih.gov/sra)

## Citation
DAnIEL: a user-friendly web server for fungal ITS amplicon sequencing data
Daniel Loos, Lu Zhang, Christine Beemelmanns, Oliver Kurzai, and Gianni Panagiotou
Frontiers in Microbiology, doi: [10.3389/fmicb.2021.720513](https://www.frontiersin.org/articles/10.3389/fmicb.2021.720513)

## Getting Started

1. Set up environmental variables to paths of this repository and the data base directory (not included):

```sh
export DANIEL_DIR=/my-path
export DANIEL_REPO_DIR=$DANIEL_DIR/repo
export DANIEL_USERDAT_DIR=$DANIEL_DIR/userdat
export DANIEL_DB_DIR=$DANIEL_DIR/db
```

2. Clone the repository to build the web server from source:
```
git clone https://github.com/bioinformatics-leibniz-hki/DAnIEL.git $DANIEL_REPO_DIR
cd $DANIEL_DIR
git lfs pull
COMPOSE_DOCKER_CLI_BUILD=1 DOCKER_BUILDKIT=1 docker-compose build --parallel
```

Alternatively, download the docker images from [Docker Hub](https://hub.docker.com/orgs/bioinformaticsleibnizhki):
```
docker pull bioinformaticsleibnizhki/daniel_backend
docker pull bioinformaticsleibnizhki/daniel_frontend
```

The latter method requires to change the image names in the file `docker-compose.yml` to use those pulled from Docker Hub (e.g. replace `ìmage: daniel_backend` with `image: bioinformaticsleibnizhki/daniel_backend:v1.0`).

3. Create msmtp [config file](https://marlam.de/msmtp/msmtp.html) in `back_end/msmtprc` for email notifications.
This file must be mounted to the back end container at `/etc/msmtprc` at runtime.
`notify_mail` function in `worker.sh` will use this to send emails to the users once the pipeline is finished.

4. Deploy a data base directory at `$DANIEL_DB_DIR` e.g. by unzipping the database [DAnIEL DB v 1.0](https://doi.org/10.5281/zenodo.4073125):

```
wget https://zenodo.org/record/4073125/files/daniel_db_v1.0.tar.gz?download=1 \
	-O daniel_db_v1.0.tar.gz
tar -xf daniel_db_v1.0.tar.gz
mv db $DANIEL_DB_DIR
```

Use docker-compose to start DAnIEL webserver containers:

```sh
wget https://raw.githubusercontent.com/bioinformatics-leibniz-hki/DAnIEL/main/docker-compose.yml
docker-compose up -d
```

The front end can be accessed at [http://localhost](http://localhost).

## Development
The software package is divided in the following sections:

* `front_end` - Interactive website to upload data and to visualize the results
* `back_end` - The analysis workflow called from the front end
* `danielLib` - R package containing functions both front end and back end require

The aim of the front end is to create a directory for each project containing all files needed to start the analysis workflow.
It is written in [R Shiny](https://shiny.rstudio.com/).
Input of reactive UI elements are merged into a file `project.json`.
A queue file in the user directory is appended by the project id when the start pipeline button is clicked.

The aim of the back end is to process the analysis workflow.
It is written in [Snakemake](https://snakemake.readthedocs.io/en/stable/).
HTML reports are generated using [R Markdown](https://rmarkdown.rstudio.com/).
They are stored in the directory `back_end/reports`.

Helper scripts e.g. to create bibtex files and the sqlite data base are stored at `back_end/helper`.

New features can be added by creating a new Snakemake rule in the directory `back_end/rules` and adding the result file as a target to the file `targets.snakefile.py`.
A [Conda](https://conda.io/en/latest/) environment can be defined for each rule in the directory `back_end/envs`.
Visualization is done by creating a new shiny module in the directory `front_end/modules` and adding it to the app files `front_end/server.R` and `front_end/ui.R`.

### Techstack

General tools used:

* [docker](https://www.docker.com/) - app containerization
* [conda](https://conda.io/en/latest/) - management of environments and software packages
* [R shiny](https://shiny.rstudio.com/) - Front end UI
* [tidyverse](https://www.tidyverse.org/) - Data manipulation and visualization
* [Snakemake](https://snakemake.readthedocs.io/en/stable/) - management of back end workflow

Tools used to perform the bioinformatical analysis:

* [Cutadapt](https://cutadapt.readthedocs.io/en/stable/) - quality control of raw reads
* [FastQC](https://www.bioinformatics.babraham.ac.uk/projects/fastqc/) - asses quality of read files
* [MultiQC](https://multiqc.info/) - merging QC results from multiple samples
* [PIPITS](https://github.com/hsgweon/pipits) - OTU profiling pipeline
* [DADA2](https://benjjneb.github.io/dada2/index.html) - ASV profiling pipeline
* [FastSpar](https://github.com/scwatts/fastspar) - Correlation analysis aware of sparsity
* [BAnOCC](https://bioconductor.org/packages/release/bioc/html/banocc.html) - Correlation analysis aware of compositionality
* [caret](http://topepo.github.io/caret/index.html) - Machine learning
* [vegan](https://cran.r-project.org/web/packages/vegan/) - Diversity analysis

## Author
**Daniel Loos**

Systems Biology and Bioinformatics

Leibniz Institute for Natural Product Research and Infection Biology

Hans Knöll Institute (HKI)
