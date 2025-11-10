# Global.R - Simple package setup
# This file runs once when the Shiny app starts

# Check if we're in the app directory or project root
if (dir.exists("R") && dir.exists("www")) {
  app_dir <<- TRUE  # Make it global so other functions can access it
} else if (dir.exists("app") && dir.exists("scripts")) {
  app_dir <<- FALSE  # Make it global so other functions can access it
} else {
  app_dir <<- FALSE  # Make it global so other functions can access it
}

# Check if we're in a Shiny server environment (temporary directory)
is_shiny_server <- grepl("^/home/web_user/", getwd()) || grepl("^/tmp/", getwd()) || grepl("^/var/folders/", getwd())

# Load required packages (skip if not available)
# Load non-tidyverse packages first
required_packages <- c("shiny", "shinyjs", "DT", "openxlsx")

openxlsx_available <- FALSE

# Try to detect quadprog (optional - we have fallback)
quadprog_available <- requireNamespace("quadprog", quietly = TRUE)
if (quadprog_available) {
  message("✅ quadprog available")
} else {
  warning("Package quadprog not available (fallback optimization will be unavailable).")
}

# Try to load kinship2 (optional - we have fallback)
kinship2_available <- FALSE
tryCatch({
  if (!require(kinship2, quietly = TRUE)) {
    install.packages("kinship2", repos = "https://cran.rstudio.com/", quiet = TRUE)
  }
  library(kinship2, quietly = TRUE)
  kinship2_available <- TRUE
}, error = function(e) {
  warning(paste("Package kinship2 not available (will use fallback):", e$message))
})

# Try to load lpSolve for transportation-based mating
tryCatch({
  if (!require(lpSolve, quietly = TRUE)) {
    install.packages("lpSolve", repos = "https://cran.rstudio.com/", quiet = TRUE)
  }
  library(lpSolve, quietly = TRUE)
  message("✅ lpSolve loaded successfully")
}, error = function(e) {
  warning(paste("Package lpSolve not available (transportation mating will be unavailable):", e$message))
})

# Load non-tidyverse packages first
for (pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    message(paste("✅ Loaded package:", pkg))
    if (identical(pkg, "openxlsx")) {
      openxlsx_available <<- TRUE
    }
  }, error = function(e) {
    warning(paste("Package", pkg, "not available:", e$message))
    if (identical(pkg, "openxlsx")) {
      openxlsx_available <<- FALSE
    }
  })
}

# Load tidyverse last to avoid masking issues
tryCatch({
  library(tidyverse)
  message("✅ Loaded tidyverse package")
}, error = function(e) {
  warning(paste("Package tidyverse not available:", e$message))
})

# Detect WebR environment
is_webr <- (exists("webr") && !is.null(webr)) ||
  grepl("emscripten|wasm", tolower(R.version$platform))

# Configure defaults for webR runtime quirks
if (is_webr) {
  options(allomate.force_greedy_mating = TRUE)
  options(allomate.force_qp_greedy = FALSE)
  message("🌐 webR environment detected; enabling greedy mating fallback.")
} else {
  options(allomate.force_greedy_mating = FALSE)
  options(allomate.force_qp_greedy = FALSE)
}

# Try to install and load optiSel - with custom fallback
optisel_available <- FALSE
custom_ocs_available <- FALSE

functions_path <- if (app_dir) "R/load_functions.R" else "app/R/load_functions.R"
if (!file.exists(functions_path)) {
  stop(paste("Functions file not found at:", functions_path))
}

source(functions_path)

tryCatch({
  if (!require(optiSel, quietly = TRUE)) {
    install.packages("optiSel", repos = "https://cran.rstudio.com/", quiet = TRUE)
  }
  library(optiSel)
  optisel_available <- TRUE
  message("✅ optiSel loaded successfully")
}, error = function(e) {
  message("⚠️ optiSel not available - using custom OCS fallback")
  if (exists("custom_ocs_available") && custom_ocs_available) {
    message("✅ Custom OCS fallback loaded successfully")
    message("📦 OCS functionality enabled via custom fallback")
  } else {
    message("⚠️ Custom OCS fallback not available after loading functions")
  }
})

# Set global flags for app behavior
ocs_available <- optisel_available || custom_ocs_available

message("🚀 AlloMate app startup complete!")
