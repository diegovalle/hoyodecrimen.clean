# STAGE 1: renv-related code
FROM rocker/geospatial:4.4.0 AS base

RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

WORKDIR /hoyodecrimen.clean
COPY renv.lock renv.lock

RUN mkdir -p renv
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R

# change default location of cache to project folder
RUN mkdir -p /renv/.cache
ENV RENV_PATHS_CACHE /renv/.cache

# restore
RUN R -e "renv::install('rgdal@1.5-28')"
RUN R -e "renv::restore()"



# Stage 2
FROM rocker/geospatial:4.4.0
COPY --from=base --chown=rstudio /renv/ /renv/

ENV RENV_PATHS_CACHE /renv/.cache

RUN mkdir -p ~/bin
RUN git clone https://github.com/felt/tippecanoe.git && \
        cd tippecanoe && \
        make -j && \
        sudo make install
RUN curl -L https://github.com/protomaps/go-pmtiles/releases/download/v1.19.1/go-pmtiles_1.19.1_Linux_x86_64.tar.gz | tar xvzf - -C /usr/local/bin pmtiles


