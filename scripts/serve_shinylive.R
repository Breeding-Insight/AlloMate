#!/usr/bin/env Rscript

# Simple static file server for Shinylive exports
# Usage:
#   Rscript scripts/serve_shinylive.R [dir]
# - dir: directory to serve (default: "docs" if exists, otherwise "shinylive_export")
# - Set PORT env var to override the port (default: 8080)

args <- commandArgs(trailingOnly = TRUE)

dir_to_serve <- if (length(args) >= 1) args[[1]] else if (dir.exists("docs")) "docs" else "shinylive_export"

if (!dir.exists(dir_to_serve)) {
  stop(sprintf("Directory not found: %s", dir_to_serve))
}

port <- suppressWarnings(as.integer(Sys.getenv("PORT", unset = "8080")))
if (is.na(port) || port <= 0) port <- 8080

host <- "127.0.0.1"

if (!requireNamespace("httpuv", quietly = TRUE)) {
  stop("Package 'httpuv' is required. Install with install.packages('httpuv').")
}

cat(sprintf("Serving %s at http://%s:%d\n", dir_to_serve, host, port))

# Start server
srv <- httpuv::runStaticServer(dir_to_serve, port = port, host = host)

url <- sprintf("http://%s:%d", host, port)
try(utils::browseURL(url), silent = TRUE)

# Keep the process alive
repeat Sys.sleep(3600)


