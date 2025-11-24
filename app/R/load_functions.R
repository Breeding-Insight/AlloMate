# Load All Functions
# This file sources all function files in the correct order

# Initialize variables if they don't exist (for standalone usage)
if (!exists("optisel_available")) {
  optisel_available <<- FALSE
}
if (!exists("kinship2_available")) {
  kinship2_available <<- FALSE
}

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

# Load pure XLSX writer utilities
if (app_dir) {
  source("R/pure_xlsx_writer.R")
} else {
  source("app/R/pure_xlsx_writer.R")
}

#' Check if the optiSel package is loaded and available
#' @return TRUE if optiSel namespace is loaded and candes exists, FALSE otherwise

#' Safely assign a function alias in the global environment
#' @param alias_name Name of the alias to assign
#' @param custom_function Function object to assign
#' @return TRUE if assignment succeeded, FALSE if skipped due to locked binding or existing optiSel functions
is_optisel_loaded <- function() {
  tryCatch({
    # Check if optiSel namespace exists
    ns <- asNamespace("optiSel")
    # Check if candes exists in the namespace
    return(exists("candes", envir = ns, inherits = FALSE))
  }, error = function(e) {
    # Namespace doesn't exist - optiSel not loaded
    return(FALSE)
  })
}

# Helper function to safely assign function aliases
# Checks if binding is locked before attempting assignment
safe_assign_alias <- function(alias_name, custom_function) {
  # First check if optiSel is loaded - if so, don't try to overwrite
  if (is_optisel_loaded()) {
    # Check if this function exists in optiSel namespace
    tryCatch({
      ns <- asNamespace("optiSel")
      if (exists(alias_name, envir = ns, inherits = FALSE)) {
        # optiSel is loaded and has this function - don't overwrite
        return(FALSE)
      }
    }, error = function(e) {
      # Namespace check failed - proceed with assignment
    })
  }
  
  # Check if the alias already exists in global environment
  if (exists(alias_name, envir = .GlobalEnv)) {
    # Try to assign - will fail if locked
    tryCatch({
      assign(alias_name, custom_function, envir = .GlobalEnv)
      return(TRUE)
    }, error = function(e) {
      # Binding is locked - can't overwrite
      # This is expected if optiSel is loaded, so we'll silently skip
      return(FALSE)
    })
  } else {
    # Function doesn't exist - safe to assign
    assign(alias_name, custom_function, envir = .GlobalEnv)
    return(TRUE)
  }
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
      
      # Only create function aliases if optiSel is not actually loaded
      # (optiSel functions are locked bindings and cannot be overwritten)
      if ((!exists("optisel_available") || !optisel_available) && !is_optisel_loaded()) {
        # Use safe assignment to avoid locked binding errors
        safe_assign_alias("candes", custom_candes)
        safe_assign_alias("opticont", custom_opticont)
        safe_assign_alias("noffspring", custom_noffspring)
        safe_assign_alias("matings", custom_matings)
      }
    }
  }
} else {
  # If functions already exist, make sure the flag is set and create aliases
  if (exists("custom_candes") && exists("custom_opticont") && exists("custom_noffspring") && exists("custom_matings")) {
    custom_ocs_available <<- TRUE
    
    # Only create function aliases if optiSel is not actually loaded
    # (optiSel functions are locked bindings and cannot be overwritten)
    if ((!exists("optisel_available") || !optisel_available) && !is_optisel_loaded()) {
      # Use safe assignment to avoid locked binding errors
      safe_assign_alias("candes", custom_candes)
      safe_assign_alias("opticont", custom_opticont)
      safe_assign_alias("noffspring", custom_noffspring)
      safe_assign_alias("matings", custom_matings)
    }
  }
}


