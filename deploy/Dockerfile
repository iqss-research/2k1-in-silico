FROM rocker/r-ver:4.6.1@sha256:555a0e7734b17f3901f01c8e379f87d797a0e6344a4cc3b246329ed3f0689809 AS builder

ARG DEBIAN_FRONTEND=noninteractive
ENV R_LIBS_USER=/opt/r-library \
    RENV_PATHS_CACHE=/opt/renv-cache

RUN sed -i \
      -e 's|http://archive.ubuntu.com/ubuntu|https://mirror.math.princeton.edu/pub/ubuntu|g' \
      -e 's|http://security.ubuntu.com/ubuntu|https://mirror.math.princeton.edu/pub/ubuntu|g' \
      /etc/apt/sources.list.d/ubuntu.sources \
    && apt-get -o Acquire::Retries=5 update --error-on=any \
    && apt-get -o Acquire::Retries=5 install --no-install-recommends -y \
      build-essential \
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
      libicu-dev \
      libjpeg-dev \
      libnode-dev \
      libpng-dev \
      libssl-dev \
      libtiff-dev \
      libuv1-dev \
      libxml2-dev \
      pandoc \
      zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src
COPY renv.lock /src/renv.lock
RUN mkdir -p "$R_LIBS_USER" "$RENV_PATHS_CACHE" \
    && R --no-init-file -e 'install.packages("renv", repos = "https://cloud.r-project.org")' \
    && R --no-init-file -e 'renv::restore(lockfile = "/src/renv.lock", library = Sys.getenv("R_LIBS_USER"), prompt = FALSE)'

COPY . /src
RUN R --no-init-file CMD INSTALL --library="$R_LIBS_USER" . \
    && R --no-init-file --file=/src/tests/container-smoke.R

FROM rocker/r-ver:4.6.1@sha256:555a0e7734b17f3901f01c8e379f87d797a0e6344a4cc3b246329ed3f0689809

ARG DEBIAN_FRONTEND=noninteractive
ENV R_LIBS_USER=/opt/r-library \
    SHINY_PORT=3000

RUN sed -i \
      -e 's|http://archive.ubuntu.com/ubuntu|https://mirror.math.princeton.edu/pub/ubuntu|g' \
      -e 's|http://security.ubuntu.com/ubuntu|https://mirror.math.princeton.edu/pub/ubuntu|g' \
      /etc/apt/sources.list.d/ubuntu.sources \
    && apt-get -o Acquire::Retries=5 update --error-on=any \
    && apt-get -o Acquire::Retries=5 upgrade --no-install-recommends -y \
    && apt-get -o Acquire::Retries=5 install --no-install-recommends -y \
      libcurl4t64 \
      libfontconfig1 \
      libfreetype6 \
      libfribidi0 \
      libgit2-1.7 \
      libharfbuzz0b \
      libicu74 \
      libjpeg-turbo8 \
      libnode109 \
      libpng16-16t64 \
      libssl3t64 \
      libtiff6 \
      libuv1t64 \
      libxml2 \
      pandoc \
      tini \
      zlib1g \
    && groupadd --gid 10001 app \
    && useradd --uid 10001 --gid app --no-create-home --home-dir /app --shell /usr/sbin/nologin app \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder --chown=10001:10001 /opt/r-library /opt/r-library
WORKDIR /app
USER 10001:10001
EXPOSE 3000
ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["R", "--no-init-file", "-e", ".libPaths(c(Sys.getenv('R_LIBS_USER'), .libPaths())); Gov2k1inSilico::runGov2k1(host='0.0.0.0', port=as.integer(Sys.getenv('SHINY_PORT')), launch_browser=FALSE)"]
