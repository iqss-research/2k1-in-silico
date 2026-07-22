# syntax=docker/dockerfile:1.20
FROM rocker/r-ver:4.6.1@sha256:8c6bcd19aae3490ebe75f7d06842f4046dc2ce23822d74eac450844af826e108 AS builder

ENV DEBIAN_FRONTEND=noninteractive \
    RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
    RENV_CONFIG_CACHE_ENABLED=TRUE \
    RENV_PATHS_CACHE=/root/.cache/R/renv \
    RENV_PATHS_LIBRARY=/opt/renv/library

RUN apt-get update && apt-get install --yes --no-install-recommends \
      build-essential \
      ca-certificates \
      cmake \
      curl \
      gfortran \
      git \
      libcurl4-openssl-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libfribidi-dev \
      libgit2-dev \
      libharfbuzz-dev \
      libhunspell-dev \
      libicu-dev \
      libjpeg-dev \
      libpng-dev \
      libssl-dev \
      libtiff-dev \
      libv8-dev \
      libxml2-dev \
      pandoc \
      zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src
COPY renv.lock renv.lock
RUN --mount=type=cache,target=/root/.cache/R/renv \
    curl --fail --location --silent --show-error \
      https://cloud.r-project.org/src/contrib/renv_1.2.3.tar.gz \
      --output /tmp/renv_1.2.3.tar.gz \
    && echo "f462b55ace5436b6bed18e01ac1267649fc196ad8505f55c8546704eda104bfb  /tmp/renv_1.2.3.tar.gz" | sha256sum --check \
    && R CMD INSTALL /tmp/renv_1.2.3.tar.gz \
    && R --vanilla -e 'stopifnot(as.character(packageVersion("renv")) == "1.2.3"); renv::restore(lockfile = "renv.lock", library = Sys.getenv("RENV_PATHS_LIBRARY"), prompt = FALSE)'

COPY . .
RUN R CMD INSTALL --library="${RENV_PATHS_LIBRARY}" . \
    && R --vanilla -e '.libPaths(Sys.getenv("RENV_PATHS_LIBRARY")); pkgload::load_all(".", quiet = TRUE); testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

FROM rocker/r-ver:4.6.1@sha256:8c6bcd19aae3490ebe75f7d06842f4046dc2ce23822d74eac450844af826e108 AS runtime

ENV DEBIAN_FRONTEND=noninteractive \
    HOME=/home/app \
    PORT=3838 \
    RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
    R_LIBS_SITE=/opt/renv/library

RUN apt-get update && apt-get install --yes --no-install-recommends \
      ca-certificates \
      curl \
      libcurl4 \
      libfontconfig1 \
      libfreetype6 \
      libfribidi0 \
      libharfbuzz0b \
      libhunspell-1.7-0 \
      libicu74 \
      libjpeg-turbo8 \
      libpng16-16t64 \
      libtiff6 \
      libnode109 \
      libxml2 \
    && apt-get purge --yes \
      build-essential \
      gcc \
      g++ \
      gfortran \
      libc6-dev \
      linux-libc-dev \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd --gid 10001 app \
    && useradd --uid 10001 --gid app --home-dir /home/app --create-home --shell /usr/sbin/nologin app

COPY --from=builder --chown=app:app /opt/renv/library /opt/renv/library

USER 10001:10001
WORKDIR /home/app
EXPOSE 3838
HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=3 \
  CMD curl --fail --silent --show-error "http://127.0.0.1:${PORT}/" >/dev/null || exit 1

CMD ["R", "--vanilla", "-e", "app <- Gov2k1inSilico::runGov2k1(); shiny::runApp(app, host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '3838')), launch.browser = FALSE)"]
