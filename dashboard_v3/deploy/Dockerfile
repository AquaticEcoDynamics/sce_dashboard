FROM scevo_base

#COPY renv.lock.prod renv.lock
#RUN R -e 'renv::restore()'

RUN R -e 'devtools::install_github("dbca-wa/rivRmon")'
COPY www/ /www/
RUN ls --recursive /www/

COPY scevo_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never",INSTALL_opts = c("--no-lock"))'
RUN rm /app.tar.gz

EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');library(scevo);scevo::run_app()"
