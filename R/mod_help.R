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
            shiny::tags$p("Click a module to expand its help section.",
                          style = "color: #666; font-size: 16px;")
          ),
          shiny::uiOutput(ns("help_accordion"))
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
    
    # ── Top-level accordion panel builder ──────────────────────────
    make_top_panel <- function(panel_id, icon_name, label, body_content) {
      shiny::tags$div(
        style = "margin-bottom: 8px;",
        shiny::tags$div(
          style = "background-color: #17a2b8; border-radius: 6px; overflow: hidden;",
          shiny::tags$button(
            class           = "btn w-100 text-left d-flex align-items-center justify-content-between",
            style           = "color: white; font-size: 15px; font-weight: 600; padding: 14px 18px; background: none; border: none;",
            `data-toggle`   = "collapse",
            `data-target`   = paste0("#top_", panel_id),
            `aria-expanded` = "false",
            shiny::tags$span(
              shiny::tagList(
                shiny::icon(icon_name),
                shiny::tags$span(label, style = "margin-left: 8px;")
              )
            ),
            shiny::tags$span("+", style = "font-size: 20px; font-weight: bold;")
          )
        ),
        shiny::tags$div(
          id    = paste0("top_", panel_id),
          class = "collapse",
          shiny::tags$div(
            style = "border: 1px solid #17a2b8; border-top: none; border-radius: 0 0 6px 6px; padding: 16px;",
            body_content
          )
        )
      )
    }
    
    output$help_accordion <- shiny::renderUI({
      shiny::tagList(
        make_top_panel(
          panel_id     = "allomate",
          icon_name    = "bullseye",
          label        = "Mate Allocation",
          # ↓ single source of truth
          body_content = help_content_allomate()
        ),
        make_top_panel(
          panel_id     = "ped_cleaner",
          icon_name    = "diagram-project",
          label        = "Pedigree Cleaner",
          # ↓ single source of truth
          body_content = help_content_ped_cleaner()
        )
      )
    })
  })
}