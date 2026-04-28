FROM rocker/r-ver:4.4.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libuv1-dev \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "\
  options(warn=2); \
  pkgs <- c('shiny','bslib','bsicons','DT','ggplot2','dplyr'); \
  install.packages(pkgs, repos='https://cloud.r-project.org', dependencies=NA); \
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly=TRUE)]; \
  if (length(missing) > 0) stop(paste('Packages failed to install:', paste(missing, collapse=', ')))"

COPY app.R /app/app.R

WORKDIR /app

EXPOSE 8080

CMD ["Rscript", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '8080')))"]
