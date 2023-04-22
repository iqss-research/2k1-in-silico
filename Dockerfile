FROM rocker/verse:4.2.1
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libicu-dev libssl-dev libv8-dev make zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.2.0")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.4.2")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.4")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.7")'
RUN Rscript -e 'remotes::install_version("VGAM",upgrade="never", version = "1.1-8")'
RUN Rscript -e 'remotes::install_version("remotes",upgrade="never", version = "2.4.2")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("reshape2",upgrade="never", version = "1.4.4")'
RUN Rscript -e 'remotes::install_version("mvtnorm",upgrade="never", version = "1.1-3")'
RUN Rscript -e 'remotes::install_version("markdown",upgrade="never", version = "1.5")'
RUN Rscript -e 'remotes::install_version("latex2exp",upgrade="never", version = "0.9.6")'
RUN Rscript -e 'remotes::install_version("katex",upgrade="never", version = "1.4.1")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.4.1")'
RUN Rscript -e 'remotes::install_version("fst",upgrade="never", version = "0.9.8")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.14.8")'
RUN Rscript -e 'remotes::install_version("shinyBS",upgrade="never", version = "0.61.1")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(Gov2k1inSilico);Gov2k1inSilico::run_app()"
