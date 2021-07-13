#
# DAnIEL Back end
#

FROM ubuntu:18.04 as base

MAINTAINER Daniel Loos "daniel.loos@leibniz-hki.de"

ENV DEBIAN_FRONTEND=noninteractive \
	PATH=/app/back_end/scripts/:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/conda/bin \
	R_REMOTES_NO_ERRORS_FROM_WARNINGS="true"

SHELL ["/bin/bash", "-c"]
WORKDIR /app
RUN apt-get update && \
	apt-get upgrade -y && \
	apt-get install -y gnupg ca-certificates && \
	apt-key adv --keyserver keyserver.ubuntu.com \
	--recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 && \
	echo deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/ \
	>> /etc/apt/sources.list && \
	apt-get update && \
	apt-get install -y \
	apt-utils \
	build-essential \
	bzip2 \
	cron \
	curl \
	git \
	libcairo2-dev \
	libcurl4-openssl-dev \
	libfontconfig1-dev \
	libssl-dev \
	libv8-dev \
	libxml2-dev \
	locales \
	mailutils \
	msmtp \
	msmtp-mta \
	pandoc \
	pandoc-citeproc \
	r-base \
	s-nail \
	wget && \
	apt-get clean && \
	rm -rf /var/lib/apt/lists/* && \
	sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
	locale-gen

# Set the locale
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
	locale-gen
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8   

#
# Conda
#

FROM base as conda

RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-4.7.10-Linux-x86_64.sh \
	-O ~/miniconda.sh && \
	/bin/bash ~/miniconda.sh -b -p /opt/conda && \
	rm ~/miniconda.sh && \
	/opt/conda/bin/conda clean -tipsy && \
	ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \
	echo ". /opt/conda/etc/profile.d/conda.sh" >> ~/.bashrc && \
	echo "conda activate base" >> ~/.bashrc && \
	conda clean --yes --all

COPY back_end/envs/base.conda_env.yml ./
RUN conda env update --file base.conda_env.yml && \
	conda clean --yes --all

# Snakemake
COPY back_end/daniel.build.snakefile.py back_end/daniel.build.snakefile.py
COPY back_end/envs back_end/envs

ARG DANIEL_SCRIPT_DIR
ARG DANIEL_DB_DIR
ARG DANIEL_USERDAT_DIR

RUN	snakemake \
	--snakefile /app/back_end/daniel.build.snakefile.py \
	--cores 10 \
	--conda-prefix /opt/conda/envs/ \
	--use-conda \
	--create-envs-only && \
	conda clean --yes --all && \
	rm -rf back_end

#
# R
#

FROM base as r

COPY back_end/install_r back_end/install_r/
RUN	Rscript back_end/install_r/install.R

#  DAnIEL lib
COPY danielLib ./danielLib
RUN R -e "devtools::install('danielLib', upgrade = 'never')"

# fix ggtree missing
RUN R -e "devtools::install_bioc('3.11/ggtree', upgrade = 'always')"

#
# Merge R and conda
#

FROM base as base2
COPY --from=conda /opt/conda/ /opt/conda/
COPY --from=r /usr/local/lib/R/site-library /usr/local/lib/R/site-library  

# config files of root home
COPY back_end/home /root/

#
# Production stage
#

FROM base2 as production

# copy front end needed for report all.Rmd
COPY front_end/ ./front_end/
COPY back_end/ ./back_end/
RUN chmod a+x /app/back_end/worker.sh
CMD bash /app/back_end/worker.sh

#
# Development stage
#

FROM base2 as dev

CMD bash /app/back_end/worker.sh
