#' HTTP routes served before the app page (Cloud Run only)
#'
#' Shiny calls `app_ui(request)` for every path that is not a static file, and
#' serves an `httpResponse` returned from it as-is. This answers the health
#' check, robots.txt and the sign-in routes, and sends signed-out visitors to
#' the landing page. Returns NULL when the app page should be shown.
#'
#' @noRd
cloud_route_request <- function(req) {
  path <- req$PATH_INFO %||% "/"

  # Quick and unauthenticated, for the landing page's wake-up request.
  # Not /healthz: Cloud Run reserves paths ending in "z".
  if (identical(path, "/health")) {
    return(shiny::httpResponse(204, content_type = "text/plain", content = "",
                               headers = list(`Cache-Control` = "no-store")))
  }
  # The public landing page lives on GitHub Pages; keep crawlers off the service.
  if (identical(path, "/robots.txt")) {
    return(shiny::httpResponse(200, content_type = "text/plain",
                               content = "User-agent: *\nDisallow: /\n"))
  }
  if (identical(path, "/auth/login"))    return(auth_login(req))
  if (identical(path, "/auth/callback")) return(auth_callback(req))
  if (identical(path, "/auth/logout"))   return(auth_logout(req))

  if (is.null(auth_user_from_cookies(req$HTTP_COOKIE))) {
    return(http_redirect(cloud_env("ALLOMATE_LANDING_URL", "/auth/login")))
  }
  NULL
}
