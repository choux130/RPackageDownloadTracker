FROM rocker/shiny-verse:4.0.0

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 

RUN Rscript -e "install.packages('remotes', repos='http://cran.rstudio.com/')"
RUN Rscript -e "remotes::install_github('timelyportfolio/dataui')"
RUN Rscript -e "install.packages('reactable', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('shinyFeedback', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('shinyFeedback', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('cranlogs', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('bs4Dash', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('data.table', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('future', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('future.apply', repos='http://cran.rstudio.com/')"

COPY ./app /srv/shiny-server/

