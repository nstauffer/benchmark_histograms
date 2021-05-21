# Get Shiny plus tidyverse packages image
FROM rocker/shiny-verse:latest

# General system libraries that might be used
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 

# Install required R packages
# These are just the app dependencies that aren't in the base install
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"

# copy the app to the image
COPY project.Rproj /srv/shiny-server/
COPY app.R /srv/shiny-server/

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server.sh"]