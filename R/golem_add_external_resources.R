#' Add external resources to the Shiny application
#'
#' This function is automatically called in `app_ui()` to add external
#' resources such as CSS, JavaScript, favicon, and others.
#'
#' @importFrom golem add_resource_path
#' @importFrom shiny tags
#' @return Shiny tags for inclusion in the UI
golem_add_external_resources <- function() {
  
  # Add a folder to the resource path (www folder inside your package)
  add_resource_path(
    "www", system.file("inst/app/www", package = "AlloMate")
  )
  
  # Include resources in the UI
  tags$head(
    # Favicon
    tags$link(rel = "shortcut icon", href = "inst/app/www/favicon.ico"),
    
    # Custom CSS
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),
    
    # Custom JavaScript
    tags$script(src = "www/custom.js"),
    
    # Any other head tags can be added here
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  )
}