#' Add external resources to the Shiny application
#'
#' This function is automatically called in `app_ui()` to add external
#' resources such as CSS, JavaScript, favicon, and others.
#'
#' @importFrom golem add_resource_path
#' @importFrom shiny tags
#' @return Shiny tags for inclusion in the UI
golem_add_external_resources <- function() {

  # Map the 'www' URL in the app to the package folder
  add_resource_path(
    "www", app_sys("app/www")
  )

  # Cache-busting token from a file's mtime so edits to CSS/JS always reload
  # (Shiny ignores the query string and still serves the underlying file).
  www_mtime <- function(file) {
    f <- app_sys("app/www", file)
    if (file.exists(f)) as.integer(file.info(f)$mtime) else "0"
  }

  # Include resources in the UI
  tags$head(
    # Browser tab title
    tags$title("AlloMate"),

    # Favicon
    tags$link(rel = "shortcut icon", href = "www/allomate.png"),

    # Custom CSS
    tags$link(
      rel = "stylesheet", type = "text/css",
      href = sprintf("www/custom.css?v=%s", www_mtime("custom.css"))
    ),

    # Custom JavaScript
    tags$script(src = sprintf("www/custom.js?v=%s", www_mtime("custom.js"))),
    
    # Any other head tags
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  )
}