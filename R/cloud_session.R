#' Per-session setup for Cloud Run
#'
#' Runs at the start of `app_server()` in Cloud Run mode. Returns FALSE (after
#' closing the session) when the connection has no valid sign-in, so the app
#' modules are never started for it.
#'
#' @noRd
cloud_session_start <- function(input, session) {
  # The page itself is gated in cloud_route_request(), but the websocket is a
  # separate request, so check the sign-in cookie again here.
  if (is.null(auth_session_user(session))) {
    session$close()
    return(FALSE)
  }
  cloud_idle_disconnect(input, session)
  cloud_cleanup_uploads(input, session)
  TRUE
}

#' Close sessions nobody is using
#'
#' Request-based billing charges for an instance while any session is
#' connected, and an open session keeps the service from scaling to zero.
#' `cloud.js` reports user activity (at most every 30 s) as
#' `input$cloud_activity`; this warns after `idle_warn_minutes` and closes the
#' session after `idle_close_minutes`.
#'
#' @noRd
cloud_idle_disconnect <- function(input, session) {
  warn_after  <- as.numeric(cloud_setting("idle_warn_minutes", 8)) * 60
  close_after <- as.numeric(cloud_setting("idle_close_minutes", 10)) * 60
  last_activity <- Sys.time()
  warned <- FALSE

  shiny::observeEvent(input$cloud_activity, {
    last_activity <<- Sys.time()
    if (warned) {
      shiny::removeModal(session)
      warned <<- FALSE
    }
  }, ignoreInit = TRUE)

  shiny::observe({
    shiny::invalidateLater(15000, session)
    idle <- as.numeric(difftime(Sys.time(), last_activity, units = "secs"))
    if (idle >= close_after) {
      session$close()
    } else if (idle >= warn_after && !warned) {
      warned <<- TRUE
      minutes_left <- max(1, round((close_after - idle) / 60))
      shiny::showModal(shiny::modalDialog(
        title = "Are you still there?",
        shiny::p(sprintf(
          "AlloMate will close this session in %d minute%s because there has been no activity.",
          minutes_left, if (minutes_left == 1) "" else "s"
        )),
        shiny::p("Download any results you still need before then. Closed sessions cannot be recovered."),
        footer = shiny::modalButton("I'm still here"),
        easyClose = TRUE
      ), session = session)
    }
  })
}

#' Delete this session's uploaded files when it ends
#'
#' Shiny keeps uploads in the R temp folder until the process exits. Cloud
#' Run's disk is held in memory, so remove them with the session.
#'
#' @noRd
cloud_cleanup_uploads <- function(input, session) {
  upload_dirs <- character()
  shiny::observe({
    values <- shiny::reactiveValuesToList(input)
    for (value in values) {
      if (is.data.frame(value) && "datapath" %in% names(value)) {
        upload_dirs <<- union(upload_dirs, unique(dirname(value$datapath)))
      }
    }
  })
  session$onSessionEnded(function() {
    temp_root <- normalizePath(tempdir(), mustWork = FALSE)
    for (dir in upload_dirs) {
      # Only ever delete folders inside R's own temp folder.
      if (startsWith(normalizePath(dir, mustWork = FALSE), temp_root)) {
        unlink(dir, recursive = TRUE)
      }
    }
  })
}

#' Disconnect message that explains the idle and connection time limits
#' @noRd
cloud_disconnect_message <- function() {
  shinydisconnect::disconnectMessage(
    text = sprintf(paste(
      "Your AlloMate session has ended. Sessions close after %s minutes without",
      "activity, and connections are limited to 60 minutes. Reload to start a new session."
    ), cloud_setting("idle_close_minutes", 10)),
    refresh = "Reload"
  )
}
