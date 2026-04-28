#' Home page module
#'
#' @param id Module id
#'
#' @noRd
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::div(
          style = "display:flex; align-items:center; justify-content:space-between; margin: 10px 0 20px 0;",
          shiny::tags$img(src = "www/allomate.png", height = "120", style = "margin-left: 10px;"),
          shiny::tags$img(src = "www/logos2.png", style = "width: 70%; height: auto; margin-right: 10px;")
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 8,
        bs4Dash::box(
          title       = "Welcome to AlloMate",
          width       = 12,
          status      = "info",
          solidHeader = TRUE,
          collapsible = FALSE,
          shiny::tags$p(
            "AlloMate helps breeding programs allocate mates and optimize selection decisions by combining pedigree-based kinship, weighted EBVs, and Optimum Contribution Selection (OCS)."
          ),
          shiny::tags$p(
            "Use the Mate Allocation tab to upload your data, configure parameters, run OCS, and export results."
          ),
          shiny::actionButton(
            ns("go_allomate"),
            "Start Analysis",
            class = "btn-primary"
          )
        ),
        bs4Dash::box(
          title       = "Quick Start",
          width       = 12,
          status      = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          shiny::tags$ol(
            shiny::tags$li(
              shiny::tags$strong("Prepare Data"),
              shiny::tags$ul(
                shiny::tags$li("Upload your pedigree file to the Pedigree Cleaning tab"),
                shiny::tags$li("Review detected issues and download the corrected pedigree")
              )
            ),
            shiny::tags$li(
              shiny::tags$strong("Mate Allocation"),
              shiny::tags$ul(
                shiny::tags$li("Upload your candidate list"),
                shiny::tags$li("Upload the corrected pedigree file"),
                shiny::tags$li("Set kinship threshold (optional)"),
                shiny::tags$li("Add EBV trait files and weights"),
                shiny::tags$li("Run OCS and export results")
              )
            )
          )  # closes tags$ol
        )   # closes Quick Start box
      ),
      shiny::column(
        width = 4,
        bs4Dash::box(
          title       = "Links & Citation",
          width       = 12,
          status      = "secondary",
          solidHeader = TRUE,
          collapsible = FALSE,
          shiny::tags$p(
            shiny::tags$a(
              href   = "https://github.com/Breeding-Insight/AlloMate",
              target = "_blank",
              "AlloMate on GitHub"
            )
          ),
          shiny::tags$hr(),
          shiny::tags$p("If you use AlloMate in research, please cite:"),
          shiny::tags$p(
            shiny::tags$strong("Chinchilla-Vargas, J., Ackerman, A. J., Taniguti, C. H., & Sandercock, A. M."),
            shiny::tags$br(),
            "AlloMate: Genetic Mate Allocation and Breeding Optimization App.",
            shiny::tags$br(),
            "RRID: SCR_027115"
          )
        )
      )
    )
  )
}

#' @param id Module id
#' @param parent_session Parent (app) session, used for sidebar navigation
#' @noRd
mod_home_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$go_allomate, {
      bs4Dash::updatebs4TabItems(
        session = parent_session,
        inputId = "MainMenu",
        selected = "allomate"
      )
    })
  })
}