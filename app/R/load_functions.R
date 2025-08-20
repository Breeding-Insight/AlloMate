# Load All Functions
# This file sources all function files in the correct order

# Use the global app_dir variable set in global.R, or determine it if not available
if (exists("app_dir")) {
  # Use existing app_dir variable
} else {
  # Check if we're in the app directory or project root
  if (dir.exists("R") && dir.exists("www")) {
    app_dir <- TRUE
  } else if (dir.exists("app") && dir.exists("scripts")) {
    app_dir <- FALSE
  } else {
    app_dir <- FALSE
  }
}

# Check if we're in a Shiny server environment
is_shiny_server <- grepl("^/home/web_user/", getwd()) || grepl("^/tmp/", getwd()) || grepl("^/var/folders/", getwd())

# Load utility functions (data processing, file handling)
if (app_dir) {
  source("R/utils.R")
} else {
  source("app/R/utils.R")
}

# Load OCS helper functions
if (app_dir) {
  source("R/ocs_helpers.R")
} else {
  source("app/R/ocs_helpers.R")
}

# Load UI helper functions
if (app_dir) {
  source("R/ui_helpers.R")
} else {
  source("app/R/ui_helpers.R")
}

# Load custom OCS fallback (if not already loaded)
if (!exists("custom_candes")) {
  if (is_shiny_server) {
    # In Shiny server environment, look for fallback in R directory
    fallback_path <- "R/optsel_fallback.R"
  } else {
    # Normal environment
    if (app_dir) {
      fallback_path <- "R/optsel_fallback.R"
    } else {
      fallback_path <- "scripts/optsel_fallback.R"
    }
  }
  
  if (file.exists(fallback_path)) {
    source(fallback_path)
    
    # Verify that the fallback functions were loaded
    if (exists("custom_candes") && exists("custom_opticont") && exists("custom_noffspring") && exists("custom_matings")) {
      # Set the flag to indicate fallback is available
      custom_ocs_available <<- TRUE
    }
  }
} else {
  # If functions already exist, make sure the flag is set
  if (exists("custom_candes") && exists("custom_opticont") && exists("custom_noffspring") && exists("custom_matings")) {
    custom_ocs_available <<- TRUE
  }
}


