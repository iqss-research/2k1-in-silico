FROM rocker/verse:4.2.1 AS gov2k1insilico_base
RUN apt-get update -y && apt-get install -y  libcurl4-openssl-dev libssl-dev libnode-dev  libxml2-dev libicu-dev make pandoc  make  git libcurl4-openssl-dev libssl-dev  libcurl4-openssl-dev libssl-dev  zlib1g-dev  libfreetype6-dev libfribidi-dev libharfbuzz-dev git libxml2-dev libfontconfig1-dev libgit2-dev pandoc libicu-dev libcurl4-openssl-dev libssl-dev libjpeg-dev libpng-dev libtiff-dev make zlib1g-dev  git libgit2-dev libcurl4-openssl-dev libssl-dev  git  make zlib1g-dev  libicu-dev make pandoc  pandoc  libicu-dev  libssl-dev  libfreetype6-dev libfribidi-dev libharfbuzz-dev libxml2-dev libfontconfig1-dev pandoc libicu-dev libcurl4-openssl-dev libssl-dev libjpeg-dev libpng-dev libtiff-dev make  libfreetype6-dev libfribidi-dev libharfbuzz-dev libfontconfig1-dev libjpeg-dev libpng-dev libtiff-dev  libxml2-dev libicu-dev pandoc make  libxml2-dev libcurl4-openssl-dev libssl-dev  libfontconfig1-dev libfreetype6-dev  libfreetype6-dev libfribidi-dev libharfbuzz-dev libfontconfig1-dev  git libgit2-dev libcurl4-openssl-dev libssl-dev make  libxml2-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages(c("renv","remotes"))'
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'

FROM gov2k1insilico_base
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'
COPY Gov2k1inSilico_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(Gov2k1inSilico);Gov2k1inSilico::run_app()"
