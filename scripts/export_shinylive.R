#!/usr/bin/env Rscript

# Deterministic Shinylive export to docs/
# Usage:
#   Rscript scripts/export_shinylive.R [dest]
# - dest: output directory (default: "docs")

args <- commandArgs(trailingOnly = TRUE)
dest <- if (length(args) >= 1) args[[1]] else "docs"

if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop("Package 'shinylive' is required. Install with install.packages('shinylive').")
}

# Avoid attempting to download wasm packages; we use WebR-safe fallback paths
Sys.setenv(SHINYLIVE_WASM_PACKAGES = 0)

# Create dest if missing and add .nojekyll to disable Jekyll processing on GitHub Pages
if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
nojekyll <- file.path(dest, ".nojekyll")
if (!file.exists(nojekyll)) file.create(nojekyll)

# subdir ensures correct base path when hosted at /<repo>/
repo_name <- basename(getwd())

cat(sprintf("Exporting Shinylive app to %s (subdir: %s)\n", dest, repo_name))

shinylive::export(
  appdir = "app",
  destdir = dest,
  quiet = FALSE,
  subdir = repo_name
)

cat("\nNext steps:\n")
cat("- Preview locally: Rscript scripts/serve_shinylive.R docs\n")
cat("- Commit and push 'docs/' to GitHub, then enable Pages from /docs\n")


