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

# Try to load quadprog (optional - we have fallback)
quadprog_available <- FALSE
tryCatch({
  if (!require(quadprog, quietly = TRUE)) {
    install.packages("quadprog", repos = "https://cran.rstudio.com/", quiet = TRUE)
  }
  library(quadprog, quietly = TRUE)
  quadprog_available <- TRUE
}, error = function(e) {
  warning(paste("Package quadprog not available (will use fallback optimization):", e$message))
})

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

# Load non-tidyverse packages first
for (pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    message(paste("✅ Loaded package:", pkg))
  }, error = function(e) {
    warning(paste("Package", pkg, "not available:", e$message))
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
is_webr <- exists("webr") && !is.null(webr)

# Try to install and load optiSel - with custom fallback
optisel_available <- FALSE
custom_ocs_available <- FALSE

tryCatch({
  if (!require(optiSel, quietly = TRUE)) {
    install.packages("optiSel", repos = "https://cran.rstudio.com/", quiet = TRUE)
  }
  library(optiSel)
  optisel_available <- TRUE
  message("✅ optiSel loaded successfully")
}, error = function(e) {
  message("⚠️ optiSel not available - loading custom OCS fallback")
  
  # Load all organized functions (which includes the fallback)
  tryCatch({
    # Determine correct path based on current directory
    if (app_dir) {
      functions_path <- "R/load_functions.R"
    } else {
      functions_path <- "app/R/load_functions.R"
    }
    
    # Check if the functions file exists before sourcing
    if (!file.exists(functions_path)) {
      stop(paste("Functions file not found at:", functions_path))
    }
    
    source(functions_path)
    
    # Check if the fallback functions were loaded and flag was set
    if (exists("custom_ocs_available") && custom_ocs_available) {
      message("✅ Custom OCS fallback loaded successfully")
      message("📦 OCS functionality enabled via custom fallback")
    } else {
      message("⚠️ Custom OCS fallback not available after loading functions")
    }
  }, error = function(func_error) {
    message("❌ Could not load organized functions:", func_error$message)
  })
})

# Set global flags for app behavior
ocs_available <- optisel_available || custom_ocs_available

message("🚀 AlloMate app startup complete!")
