FROM rocker/shiny:latest

# Update dan install system packages
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libxml2-dev \
    libicu-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN Rscript -e "install.packages(c('dplyr','reshape2','readxl','tidyr'))"
RUN Rscript -e "install.packages(c('openxlsx','shinydashboard','tidytext'))"
RUN Rscript -e "install.packages(c('shiny','shinymanager'))"

# Copy aplikasi
COPY app.R /srv/shiny-server/

# Buat folder log
RUN mkdir -p /var/log/shiny-server
RUN chown shiny:shiny /var/log/shiny-server

# Copy Shiny server config (opsional)
# COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Set permission
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

# User non-root
USER shiny

# Run Shiny
CMD ["/usr/bin/shiny-server"]