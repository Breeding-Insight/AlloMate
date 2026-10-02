#' Is AlloMate running in its Cloud Run configuration?
#'
#' True only when the active golem config profile (`R_CONFIG_ACTIVE`) is
#' `cloudrun`. Desktop, shinyapps.io and install_github runs use `default`,
#' so none of the cloud code paths run there.
#'
#' @noRd
cloud_mode <- function() {
  isTRUE(tryCatch(get_golem_config("cloud_run"), error = function(e) FALSE))
}

#' Read a Cloud Run setting from the golem config, with a fallback
#' @noRd
cloud_setting <- function(name, default = NULL) {
  value <- tryCatch(get_golem_config(name), error = function(e) NULL)
  if (is.null(value)) default else value
}

#' Read an environment variable, treating "" as unset
#' @noRd
cloud_env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

#' True when the process is running on Cloud Run (Cloud Run sets K_SERVICE)
#' @noRd
on_cloud_run <- function() {
  nzchar(Sys.getenv("K_SERVICE", unset = ""))
}

#' Check the Cloud Run configuration before the app starts
#'
#' Fails with one clear message listing everything that is missing, rather
#' than failing on the first sign-in.
#'
#' @noRd
cloud_preflight <- function() {
  missing_pkgs <- c("httr2", "jsonlite", "openssl")[
    !vapply(c("httr2", "jsonlite", "openssl"), requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_pkgs) > 0) {
    stop("Cloud Run mode needs these packages: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }
  if (auth_bypass_enabled()) {
    message("AlloMate: ORCID sign-in is bypassed (ALLOMATE_AUTH_BYPASS, local use only).")
    return(invisible(TRUE))
  }
  required <- c("SECRET_KEY", "ORCID_CLIENT_ID", "ORCID_CLIENT_SECRET", "FIRESTORE_DATABASE")
  missing_env <- required[!nzchar(Sys.getenv(required))]
  if (length(missing_env) > 0) {
    stop("Cloud Run mode needs these environment variables: ",
         paste(missing_env, collapse = ", "), call. = FALSE)
  }
  if (nchar(Sys.getenv("SECRET_KEY")) < 32) {
    stop("SECRET_KEY must be at least 32 characters.", call. = FALSE)
  }
  invisible(TRUE)
}
