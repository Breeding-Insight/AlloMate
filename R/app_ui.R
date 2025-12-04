#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @importFrom shiny tagList fluidPage sidebarLayout sidebarPanel mainPanel
#' @importFrom shiny downloadButton fileInput numericInput actionButton uiOutput 
#'   conditionalPanel verbatimTextOutput fluidRow column HTML
#' @importFrom shiny tags
#' @importFrom shinyjs useShinyjs
#' @importFrom DT DTOutput renderDT
#' @noRd
app_ui <- function(request) {

  # Override: Chromium download attribute bug fix
  downloadButton <- function(...) {
    tag <- shiny::downloadButton(...)
    tag$attribs$download <- NULL
    tag
  }

  tagList(
    golem_add_external_resources(),

    fluidPage(
      useShinyjs(),

      # Custom CSS for better help content styling
      tags$head(
        tags$style(HTML("
        /* ... your full CSS here ... */
        "))
      ),

      # JavaScript for smooth scrolling and TOC functionality
      tags$script(HTML("
      /* ... your JS here ... */
      ")),

      ## ─── Banner ───────────────────────────
      div(
        style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 15px;",
        tags$img(src = "www/allomate.png", height = "120px", style = "margin-left: 20px;"),
        tags$img(src = "www/logos2.png", style = "width: 67%; height: auto;")
      ),

      ## ─── Sidebar + Main Panel ───────────────────────────
      sidebarLayout(
        sidebarPanel(
          # Dynamic startup guide and feedback
          div(
            id = "startup_guide",
            style = "background-color: #ffffff; border: 1px solid #444444; padding: 10px; margin-bottom: 15px; border-radius: 5px;",
            h4("🚀 Getting Started"),
            htmlOutput("dynamic_guide"),
            div(
              style = "text-align: center; margin-top: 15px; padding-top: 10px; border-top: 1px solid #dee2e6;",
              actionButton("help_btn", "❓ Help",
                           style = "background-color: #007bff; color: white; border: none; padding: 8px 16px; border-radius: 5px;")
            )
          ),

          wellPanel(
            style = "background-color: #ffffff; border: 2px solid #444444; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
            h4("🧬 Core Data Inputs", style = "color: #1565c0; margin-bottom: 15px; border-bottom: 1px solid #2196f3; padding-bottom: 8px;"),
            p("These inputs are used by both Index Generation and OCS calculations:", style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"),
            h5("Estimate progeny genetic merit"),
            fileInput("candidate_file", "Upload list of candidates",
                      accept = c(".csv", ".txt")),
            h5("Calculate kinship matrix"),
            fileInput("pedigree_file", "Upload pedigree file", accept = ".txt"),
            uiOutput("pedigree_status_display"),
            h5("Set kinship threshold"),
            numericInput("thresh", "Max kinship allowed between mates:",
                         value = 1, min = 0, max = 1, step = 0.1)
          ),

          wellPanel(
            style = "background-color: #ffffff; border: 2px solid #444444; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
            h4("⚖️ Weighted EBVs", style = "color: #c62828; margin-bottom: 15px; border-bottom: 1px solid #f44336; padding-bottom: 8px;"),
            p("Define traits and their relative importance for breeding decisions:", style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"),
            h5("Traits (EBVs and weights)"),
            uiOutput("trait_inputs"),
            fluidRow(
              column(6, actionButton("add_trait", "➕ Add trait")),
              column(6, actionButton("remove_trait", "➖ Remove trait"))
            ),
            p("💡 Note: Adding or removing traits will require re-uploading files.",
              style = "color: #6c757d; font-size: 11px; font-style: italic; margin-top: 8px;")
          ),

          wellPanel(
            style = "background-color: #ffffff; border: 2px solid #444444; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
            h4("🎯 Optimum Contribution Selection", style = "color: #856404; margin-bottom: 15px; border-bottom: 1px solid #ffeaa7; padding-bottom: 8px;"),
            p("Configure breeding objectives and constraints:", style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"),
            verbatimTextOutput("package_status_text"),
            h5("Breeding Objectives"),
            numericInput("inbreeding_rate", "Desired Inbreeding Rate",
                         value = 0.05, min = 0.01, max = 0.2, step = 0.01),
            numericInput("num_offspring", "Number of Offspring",
                         value = 100, min = 10, step = 1),
            conditionalPanel(
              condition = "output.ocs_checkbox_mode == '1'",
              checkboxInput("enforce_pair_kinship", "Enforce per-pair kinship threshold in mating plan", value = TRUE),
              checkboxInput("force_greedy_mating", "Use greedy mating (browser-safe)",
                            value = isTRUE(getOption("allomate.force_greedy_mating", FALSE))),
              checkboxInput("force_qp_greedy", "Bypass quadprog (heuristic contributions)",
                            value = isTRUE(getOption("allomate.force_qp_greedy", FALSE)))
            ),
            actionButton("run_ocs_btn", "Run OCS",
                         style = "margin-top: 15px; width: 100%; background-color: #856404; color: white; border: none; padding: 10px; border-radius: 5px;")
          ),

          wellPanel(
            style = "background-color: #ffffff; border: 2px solid #444444; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
            h4("📊 Export Results", style = "color: #155724; margin-bottom: 15px; border-bottom: 1px solid #c3e6cb; padding-bottom: 8px;"),
            p("Download all results in a single Excel file with multiple tabs:", style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"),
            downloadButton("download_all_results", "📥 Export All Results",
                           style = "width: 100%; background-color: #28a745; color: white; border: none; padding: 10px; border-radius: 5px; margin-bottom: 10px;"),
            div(
              style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 12px; margin-bottom: 10px; border-radius: 5px;",
              h5("📋 File Status", style = "color: #495057; margin-top: 0; margin-bottom: 10px; font-size: 14px;"),
              htmlOutput("file_status_display")
            ),
            actionButton("view_r_code_btn", "📝 View R Code",
                         style = "width: 100%; background-color: #17a2b8; color: white; border: none; padding: 10px; border-radius: 5px;")
          )
        ),

        mainPanel(
          tabsetPanel(
            id = "main_tabs",
            tabPanel("Kinship and EBV",
                     verbatimTextOutput("message1"),
                     uiOutput("candidate_ebv_status"),
                     uiOutput("ebv_upload_prompt"),
                     uiOutput("message2"),
                     DTOutput("quadrants_table"),
                     DTOutput("matrix")
            ),
            tabPanel("Optimum Contribution Selection",
                     div(
                       id = "ocs_container",
                       style = "position: relative;",
                       div(
                         id = "ocs_loading",
                         style = "display: none; position: absolute; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(255,255,255,0.8); z-index: 9999;",
                         div(
                           style = "position: absolute; top: 30%; left: 50%; transform: translate(-50%, -50%);",
                           div(class = "ocs-spinner")
                         )
                       ),
                       DTOutput("ocs_candidate_table"),
                       uiOutput("ocs_solver_note"),
                       br(),
                       DTOutput("ocs_mating_table")
                     )
            ),
            tabPanel("Help",
                     div(
                       style = "padding: 20px; background-color: #f8f9fa; border-radius: 8px; margin: 10px 0; max-height: 80vh; overflow-y: auto; position: relative;",
                       div(
                         style = "background-color: white; padding: 25px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                         div(
                           style = "text-align: center; margin-bottom: 25px; padding-bottom: 15px; border-bottom: 2px solid #007bff;",
                           h2("📚 AlloMate Documentation", style = "color: #007bff; margin-bottom: 10px;"),
                           p("Complete user guide and technical documentation", style = "color: #666; font-size: 16px;")
                         ),
                         div(
                           style = "line-height: 1.6; font-size: 14px;",
                           htmlOutput("help_content")
                         ),
                         div(
                           style = "text-align: center; margin-top: 30px; padding-top: 20px; border-top: 1px solid #dee2e6;",
                           actionButton("back_to_top", "⬆️ Back to Top",
                                        style = "background-color: #6c757d; color: white; border: none; padding: 8px 16px; border-radius: 5px;")
                         )
                       )
                     )
            )
          )
        )
      )
    )
  )
}
