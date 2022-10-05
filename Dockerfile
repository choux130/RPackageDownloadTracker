FROM rocker/r-ver:4.1.0

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev

RUN Rscript -e "install.packages('remotes', repos='http://cran.rstudio.com/')"
RUN Rscript -e "remotes::install_github('timelyportfolio/dataui')"
RUN Rscript -e "remotes::install_version('shiny', version='1.6.0', upgrade='always')"
RUN Rscript -e "remotes::install_version('reactable', version='0.2.2', upgrade='always')"
RUN Rscript -e "remotes::install_version('shinycssloaders', version='1.0.0', upgrade='always')"
RUN Rscript -e "remotes::install_version('shinyFeedback', version='0.3.0', upgrade='always')"
RUN Rscript -e "remotes::install_version('shinyWidgets', version='0.6.0', upgrade='always')"
RUN Rscript -e "remotes::install_version('cranlogs', version='2.1.1', upgrade='always')"
RUN Rscript -e "remotes::install_version('bs4Dash', version='0.5.0', upgrade='always')"
RUN Rscript -e "remotes::install_version('data.table', version='1.13.6', upgrade='always')"
RUN Rscript -e "remotes::install_version('future', version='1.18.0', upgrade='always')"
RUN Rscript -e "remotes::install_version('future.apply', version='1.7.0', upgrade='always')"
RUN Rscript -e "remotes::install_version('dplyr', version='1.0.7', upgrade='always')"
RUN Rscript -e "remotes::install_version('tidyr', version='1.1.3', upgrade='always')"
RUN Rscript -e "remotes::install_version('xml2', version='1.3.1', upgrade='always')"
RUN Rscript -e "remotes::install_version('rvest', version='1.0.0', upgrade='always')"

COPY ./app /app
WORKDIR /app

EXPOSE 3838
ENTRYPOINT ["Rscript", "./main.R"]