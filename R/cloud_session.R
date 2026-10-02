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
  TRUE
}
