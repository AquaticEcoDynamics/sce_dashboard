FROM rocker/r-ver:4.2.3

# System dependencies
RUN apt-get update && apt-get install -y  gdal-bin git-core libcairo2-dev libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libicu-dev libpng-dev libproj-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev cmake libharfbuzz-dev libfribidi-dev libudunits2-dev && rm -rf /var/lib/apt/lists/*

# Not sure about this 
# RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone

EXPOSE 3838

# Get RENV
#RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
#ENV RENV_PATHS_LIBRARY renv/library
#RUN R -e "renv::restore()"


