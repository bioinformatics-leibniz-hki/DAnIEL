FROM rocker/shiny-verse:3.6.1 as base1

MAINTAINER Daniel Loos "daniel.loos@leibniz-hki.de"

# system update
RUN apt-get update && \
	apt-get upgrade -y && \
	apt-get install -y \
		vim \
		apt-utils \
		libmagick++-dev \
		sshpass && \
	apt-get clean

WORKDIR /app

RUN mkdir userdat && chmod a+rwx userdat 

# Expose shiny port
EXPOSE 3838

COPY front_end/install.R ./

FROM base1 as base2
RUN Rscript ./install.R

# install danielLib
COPY danielLib/ ./danielLib
RUN export R_REMOTES_NO_ERRORS_FROM_WARNINGS="true" && \
	R -e "devtools::install('/app/danielLib/')"

COPY front_end/index.html /srv/shiny-server/

#
# Production stage
#

FROM base2 as production
RUN R -e "install.packages('profvis')"

COPY front_end /srv/shiny-server/latest
CMD mv /srv/shiny-server/latest/index.html /srv/shiny-server/index.html

# parse env vars to shiny apps and start shiny server
CMD env > /home/shiny/.Renviron && \
	chown shiny.shiny /home/shiny/.Renviron && \
	runuser -l shiny -c '/usr/bin/shiny-server.sh'

#
# Development stage
#

FROM base2 as dev
RUN R -e "install.packages('profvis')"
# parse env vars to shiny apps and start shiny server
CMD env > /home/shiny/.Renviron && \
	chown shiny:shiny -R /userdat && \
	chown shiny.shiny /home/shiny/.Renviron && \
        ls -R /app && \
	runuser -l shiny -c '/usr/bin/shiny-server.sh'
