#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'   DO NOT REMOVE.
#'
#' @noRd
app_server <- function(input, output, session) {
  if (cloud_mode() && !cloud_session_start(input, session)) return(invisible())
   shiny::callModule(mod_Home_server, "Home_1", parent_session = session)
  mod_allomate_server("allomate_1",             parent_session = session)
  mod_matrix_builder_server("matrix_builder_1", parent_session = session)
  mod_help_server("help_1",                     parent_session = session)
}