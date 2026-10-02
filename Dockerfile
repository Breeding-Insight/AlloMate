# AlloMate for Google Cloud Run. See docs/cloud-run.md.
# The desktop app and shinyapps.io do not use this file.
#
# rocker/r-ver installs CRAN packages as prebuilt binaries from Posit Package
# Manager, pinned to the CRAN snapshot of this R release.
FROM rocker/r-ver:4.6.0

# Runtime libraries for httr2/curl, openssl and xml2.
RUN apt-get update \
    && apt-get install -y --no-install-recommends libcurl4-openssl-dev libssl-dev libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Dependencies first, so code changes do not reinstall them. Installs Imports
# plus the Suggests the app needs at runtime: kinship2 and AGHmatrix (kinship
# and Matrix Builder) and httr2/jsonlite/openssl (ORCID sign-in, Firestore).
COPY DESCRIPTION /tmp/DESCRIPTION
RUN Rscript -e ' \
      imports <- read.dcf("/tmp/DESCRIPTION", fields = "Imports")[1, 1]; \
      pkgs <- trimws(sub("\\(.*$", "", strsplit(imports, ",")[[1]])); \
      pkgs <- c(pkgs[nzchar(pkgs)], "kinship2", "AGHmatrix", "httr2", "jsonlite", "openssl"); \
      install.packages(pkgs, Ncpus = 4); \
      missing <- setdiff(pkgs, rownames(installed.packages())); \
      if (length(missing)) stop("Failed to install: ", paste(missing, collapse = ", "))'

COPY . /tmp/AlloMate
RUN R CMD INSTALL --no-docs --no-multiarch --no-test-load /tmp/AlloMate \
    && rm -rf /tmp/AlloMate /tmp/DESCRIPTION

RUN useradd --create-home --uid 10001 allomate
USER allomate
WORKDIR /home/allomate

ENV R_CONFIG_ACTIVE=cloudrun \
    PORT=8080
EXPOSE 8080

CMD ["Rscript", "-e", "shiny::runApp(AlloMate::run_app(), host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '8080')), launch.browser = FALSE)"]
