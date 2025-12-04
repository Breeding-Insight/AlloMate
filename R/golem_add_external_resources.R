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
  
  # Include resources in the UI
  tags$head(
    # Favicon
    tags$link(rel = "shortcut icon", href = "www/favicon.ico"),
    
    # Custom CSS
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),
    
    # Custom JavaScript
    tags$script(src = "www/custom.js"),
    
    # Any other head tags
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  )
}