#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'   DO NOT REMOVE.
#'
#' @noRd
app_server <- function(input, output, session) {
  mod_home_server("home_1", parent_session = session)
  mod_allomate_server("allomate_1", parent_session = session)
  mod_help_server("help_1", parent_session = session)
}

