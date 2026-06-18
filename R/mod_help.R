# mod_help.R

#' Help module UI
#'
#' @param id Module id
#'
#' @noRd
mod_help_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::div(
          style = "padding: 20px;",
          shiny::div(
            style = "text-align: center; margin-bottom: 25px; padding-bottom: 15px; border-bottom: 2px solid #17a2b8;",
            shiny::tags$h2("Help Documentation", style = "color: #17a2b8; margin-bottom: 10px;"),
            shiny::tags$p("Reference material for the application workflows.",
                          style = "color: #666; font-size: 16px;")
          ),
          shiny::uiOutput(ns("help_content"))
        )
      )
    )
  )
}

#' Help module server
#'
#' @param id Module id
#' @param parent_session Parent (app) session
#'
#' @noRd
mod_help_server <- function(id, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    make_help_section <- function(icon_name, label, body_content) {
      shiny::tags$div(
        style = "margin-bottom: 20px;",
        shiny::tags$div(
          style = "background-color: #17a2b8; border-radius: 6px 6px 0 0; color: white; font-size: 15px; font-weight: 600; padding: 14px 18px;",
          shiny::tagList(
            shiny::icon(icon_name),
            shiny::tags$span(label, style = "margin-left: 8px;")
          )
        ),
        shiny::tags$div(
          style = "border: 1px solid #17a2b8; border-top: none; border-radius: 0 0 6px 6px; padding: 16px;",
          body_content
        )
      )
    }

    output$help_content <- shiny::renderUI({
      shiny::tagList(
        make_help_section(
          icon_name = "bullseye",
          label = "Mate Allocation",
          body_content = help_content_allomate(collapse_fn = make_collapse_panel, id_prefix = "page")
        ),
        make_help_section(
          icon_name = "diagram-project",
          label = "Pedigree Cleaner",
          body_content = help_content_ped_cleaner(collapse_fn = make_collapse_panel, id_prefix = "page")
        )
      )
    })
  })
}
