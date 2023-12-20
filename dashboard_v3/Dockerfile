FROM rocker/verse:4.2.3
RUN apt-get update && apt-get install -y  gdal-bin git-core libcairo2-dev libcurl4-openssl-dev libfribidi-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libharfbuzz-dev libicu-dev libpng-dev libpq-dev libproj-dev libssl-dev libtiff-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.4.2")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.4")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.7")'
RUN Rscript -e 'remotes::install_version("remotes",upgrade="never", version = "2.4.2")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.3.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("thinkr",upgrade="never", version = "0.16")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("RPostgreSQL",upgrade="never", version = "0.7-5")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.1.2")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("dbplyr",upgrade="never", version = "2.3.2")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(scevo);scevo::run_app()"
