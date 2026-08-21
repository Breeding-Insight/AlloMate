#' Home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash renderValueBox valueBox
#' @importFrom utils capture.output sessionInfo read.csv read.table
#' @importFrom grDevices jpeg png tiff svg dev.off
#' @importFrom stats dbinom
#'
#'
mod_Home_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidPage(
      shiny::fluidRow(
        # Left section: top two boxes + news_box stacked, independent of right column height
        shiny::column(width = 8,
                      shiny::fluidRow(
                        shiny::column(width = 6,
                                      bs4Dash::box(
                                        title = "About AlloMate", status = "info", solidHeader = FALSE, width = 12, collapsible = FALSE,
                                        shiny::HTML(
                                          "<div style='text-align: center; margin-top: 10px; margin-bottom: 10px;'>
                                <img src='www/allomate.png' alt='AlloMate' style='width: 120px; height: 120px;'>
                              </div>",
                                          paste0(
                                            "<p>AlloMate helps breeding programs allocate mates and optimize selection decisions by combining pedigree-based kinship, weighted EBVs, and Optimum Contribution Selection (OCS).</p>",
                                            "<ul>",
                                            "<li>Pedigree-based kinship calculation</li>",
                                            "<li>Matrix Builder for pedigree (A), genomic (G), and combined single-step (H) relationship matrices</li>",
                                            "<li>Multi-trait EBV weighting</li>",
                                            "<li>Optimum Contribution Selection (OCS) and mate allocation</li>",
                                            "</ul>"
                                          )
                                        ),
                                        style = "overflow-y: auto; height: 500px"
                                      )
                        ),
                        shiny::column(width = 6,
                                      bs4Dash::box(
                                        title = "About Breeding Insight", status = "success", solidHeader = FALSE, width = 12, collapsible = FALSE,
                                        shiny::HTML(
                                          "We provide scientific consultation and data management software to the specialty crop and animal breeding communities.
            <ul>
              <li>Genomics</li>
              <li>Phenomics</li>
              <li>Data Management</li>
              <li>Software Tools</li>
              <li>Analysis</li>
            </ul>
            Breeding Insight is funded by the U.S. Department of Agriculture (USDA) Agricultural Research Service (ARS) through University of Florida (UF) - Institute of Food and Agricultural Sciences (IFAS).
            <div style='text-align: center; margin-top: 20px;'>
              <img src='www/BreedingInsight.png' alt='Breeding Insight' style='width: 85px; height: 85px;'>
            </div>"
                                        ),
                                        style = "overflow-y: auto; height: 500px"
                                      )
                        )
                      ),
                      shiny::fluidRow(
                        shiny::column(width = 12,
                                      shiny::uiOutput(ns("news_box"))
                        )
                      )
        ),
        # Right section: links + Try the Breedverse box
        shiny::column(width = 4,
                      shiny::tags$a(
                        href = "https://www.breedinginsight.org",
                        target = "_blank",
                        bs4Dash::valueBox(
                          value = NULL,
                          subtitle = "Learn More About Breeding Insight",
                          icon = shiny::icon("link"),
                          color = "purple",
                          gradient = TRUE,
                          width = 11
                        ),
                        style = "text-decoration: none; color: inherit;"
                      ),
                      shiny::tags$a(
                        href = "https://breedinginsight.org/contact-us/",
                        target = "_blank",
                        bs4Dash::valueBox(
                          value = NULL,
                          subtitle = "Contact Us",
                          icon = shiny::icon("envelope"),
                          color = "danger",
                          gradient = TRUE,
                          width = 11
                        ),
                        style = "text-decoration: none; color: inherit;"
                      ),
                      shiny::tags$a(
                      href = "https://scribehow.com/viewer/...",
                      target = "_blank",
                      bs4Dash::valueBox(
                      value = NULL,
                      subtitle = "AlloMate Tutorial",
                      icon = shiny::icon("compass"),
                      color = "info",
                      gradient = TRUE,
                      width = 11
                      ),
                      style = "text-decoration: none; color: inherit;"
                      ),
                      bs4Dash::box(
                        title = "Try the Breedverse!", status = "warning", solidHeader = TRUE, width = 11, collapsible = FALSE,
                        shiny::HTML(
                          "We developed an R shiny interface where you can use ALL of our Breeding Insight applications in a single location. This
                   includes applications like BIGapp, Qploidy, and GenoBrew, PLUS all of our newly released applications.
                   Learn more and see install instructions here
                    <div style='text-align: center; margin-top: 20px;'>
                      <img src='www/breedverse_logo.png' alt='Breedingverse' style='width: 120px; height: 140px;'>
                    </div>"
                        ),
                        style = "overflow-y: auto; height: 300px"
                      )
        )
      )
    )
  )
}

#' Home Server Functions
#'
#'
#' @noRd
mod_Home_server <- function(input, output, session, parent_session){
  ns <- session$ns
  output$news_box <- shiny::renderUI({
    # Locate NEWS.md via golem helper, then dev working directory fallbacks
    news_file <- app_sys("NEWS.md")
    if (!nzchar(news_file) || !file.exists(news_file)) {
      candidates <- c(
        file.path(getwd(), "NEWS.md"),
        file.path(getwd(), "..", "NEWS.md"),
        file.path(getwd(), "..", "..", "NEWS.md")
      )
      news_file <- Filter(file.exists, candidates)[1]
      if (is.null(news_file) || is.na(news_file)) return(NULL)
    }
    lines <- readLines(news_file, warn = FALSE)
    version_starts <- grep("^# ", lines)
    if (length(version_starts) == 0) return(NULL)
    # Parse up to the two most recent versions
    n_versions <- min(2, length(version_starts))
    parse_section <- function(content) {
      html_parts <- character(0)
      in_list    <- FALSE
      for (line in content) {
        if (grepl("^## ", line)) {
          if (in_list) { html_parts <- c(html_parts, "</ul>"); in_list <- FALSE }
          heading    <- sub("^## ", "", line)
          html_parts <- c(html_parts, paste0("<h5><b>", heading, "</b></h5>"))
        } else if (grepl("^\\* ", line)) {
          if (!in_list) { html_parts <- c(html_parts, "<ul>"); in_list <- TRUE }
          item       <- sub("^\\* ", "", line)
          item       <- gsub("\\*\\*(.+?)\\*\\*", "<b>\\1</b>", item)
          html_parts <- c(html_parts, paste0("<li>", item, "</li>"))
        } else if (nzchar(trimws(line))) {
          if (in_list) { html_parts <- c(html_parts, "</ul>"); in_list <- FALSE }
          text       <- gsub("\\*\\*(.+?)\\*\\*", "<b>\\1</b>", line)
          html_parts <- c(html_parts, paste0("<p>", text, "</p>"))
        }
      }
      if (in_list) html_parts <- c(html_parts, "</ul>")
      html_parts
    }
    all_html <- character(0)
    for (i in seq_len(n_versions)) {
      v_start <- version_starts[i]
      v_title <- sub("^# ", "", lines[v_start])
      v_end   <- if (i < length(version_starts)) version_starts[i + 1] - 1 else length(lines)
      content <- lines[(v_start + 1):v_end]
      section_html <- parse_section(content)
      if (i > 1) all_html <- c(all_html, "<hr/>")
      all_html <- c(all_html,
                    paste0("<h4><b>", v_title, "</b></h4>"),
                    section_html)
    }
    bs4Dash::box(
      title       = "What's New",
      status      = "info",
      solidHeader = FALSE,
      width       = 12,
      collapsible = TRUE,
      shiny::HTML(paste(all_html, collapse = "\n"))
    )
  })
}

# Suppress global variable and function notes for CRAN checks
utils::globalVariables(c(
  ".__hl__", "Xb", "Yb"
))
## To be copied in the UI
# mod_Home_ui("Home_1")
## To be copied in the server
# mod_Home_server("Home_1")