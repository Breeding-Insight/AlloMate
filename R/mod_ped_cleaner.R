#' Pedigree Cleaner module UI
#'
#' @param id Module id
#'
#' @noRd
mod_ped_cleaner_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::div(
          style = "background-color: #ffffff; border: 2px solid #444444; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
          shiny::h4(
            shiny::tagList(shiny::icon("gear"), "Getting Started"),
            style = "color: #000000; margin-bottom: 15px; border-bottom: 1px solid black; padding-bottom: 8px;"
          ),
          shiny::htmlOutput(ns("dynamic_guide")),
          shiny::div(
            style = "text-align: center; margin-top: 15px; padding-top: 10px; border-top: 1px solid #dee2e6;",
            shiny::actionButton(
              ns("help_btn"),
              shiny::tagList(shiny::icon("circle-question"), "Help"),
              style = "background-color: #FFD700; color: #000000; border:none; padding: 8px 16px; border-radius: 5px;"
            )
          )
        ),
        shiny::wellPanel(
          style = "background-color: #ffffff; border: 2px solid #444444; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
          shiny::h4(
            shiny::tagList(shiny::icon("sitemap"), "Pedigree Input"),
            style = "color: #000000; margin-bottom: 15px; border-bottom: 1px solid #000000; padding-bottom: 8px;"
          ),
          shiny::p(
            "Upload a tab-separated pedigree file with columns: id, male_parent, female_parent (or id, sire, dam).",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"
          ),
          shiny::fileInput(
            ns("ped_file"),
            "Upload pedigree file",
            accept = c(".txt", ".csv")
          ),
          shiny::actionButton(
            ns("run_check"),
            "Run Pedigree Check",
            style = "margin-top: 10px; width: 100%; background-color: #28a745; color: white; border: none; padding: 10px; border-radius: 5px;"
          )
        ),
        shiny::wellPanel(
          style = "background-color: #ffffff; border: 2px solid #444444; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
          shiny::h4(
            shiny::tagList(shiny::icon("download"), "Export"),
            style = "color: #000000; margin-bottom: 15px; border-bottom: 1px solid #000000; padding-bottom: 8px;"
          ),
          shiny::p(
            "Download the corrected pedigree and issue report as a zip file:",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"
          ),
          shinyjs::disabled(
            shiny::downloadButton(
              ns("download_results"),
              "Export Corrected Pedigree + Report",
              style = "width: 100%; background-color: #28a745; color: white; border: none; padding: 10px; border-radius: 5px;"
            )
          )
        )
      ),
      shiny::mainPanel(
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bs4Dash::box(
              title       = "Status",
              width       = 12,
              collapsible = TRUE,
              status      = "info",
              shinyWidgets::progressBar(
                id          = ns("pb_ped"),
                value       = 0,
                status      = "info",
                display_pct = TRUE,
                striped     = TRUE,
                title       = " "
              )
            )
          )
        ),
        shiny::br(),
        shiny::uiOutput(ns("summary_banner")),
        shiny::br(),
        shiny::uiOutput(ns("results_ui"))
      )
    )
  )
}

#' Pedigree Cleaner module server
#'
#' @param id Module id
#' @param parent_session Parent (app) session, used for sidebar navigation
#'
#' @noRd
mod_ped_cleaner_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    check_results <- shiny::reactiveVal(NULL)
    error_message <- shiny::reactiveVal("")
    
    make_collapse_panel <- function(panel_id, icon_name, label, body_content) {
      shiny::tags$div(
        class = "card mb-1",
        style = "border: 1px solid #dee2e6; border-radius: 4px;",
        shiny::tags$div(
          class = "card-header p-0",
          style = "background-color: #f8f9fa;",
          shiny::tags$button(
            class           = "btn btn-link btn-sm w-100 text-left d-flex align-items-center",
            style           = "color: #343a40; text-decoration: none; font-size: 13px; padding: 8px 12px; gap: 6px;",
            `data-toggle`   = "collapse",
            `data-target`   = paste0("#", panel_id),
            `aria-expanded` = "false",
            shiny::icon(icon_name),
            shiny::tags$span(label)
          )
        ),
        shiny::tags$div(
          id    = panel_id,
          class = "collapse",
          shiny::tags$div(
            class = "card-body",
            style = "padding: 12px 14px; font-size: 13px;",
            body_content
          )
        )
      )
    }
    
    ped_table <- function(rows) {
      shiny::tags$table(
        class = "table table-bordered table-sm",
        style = "width: auto; font-size: 12px; margin-bottom: 6px;",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("id"),
            shiny::tags$th("sire"),
            shiny::tags$th("dam")
          )
        ),
        shiny::tags$tbody(rows)
      )
    }
    
    ped_row <- function(id, sire, dam, bg = NULL) {
      style <- if (!is.null(bg)) paste0("background-color:", bg, ";") else ""
      shiny::tags$tr(
        style = style,
        shiny::tags$td(id),
        shiny::tags$td(sire),
        shiny::tags$td(dam)
      )
    }
    
    ####  Startup ####
    output$dynamic_guide <- shiny::renderUI({
      current_error <- error_message()
      has_file      <- !is.null(input$ped_file)
      has_results   <- !is.null(check_results())
      
      if (current_error != "") {
        return(shiny::HTML(paste0(
          "<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px; border-radius: 4px; margin-bottom: 10px;'>",
          "<p style='color: #721c24; margin: 0;'><strong>Error occurred:</strong></p>",
          "<p style='color: #721c24; margin: 5px 0; font-family: monospace; font-size: 11px;'>", current_error, "</p>",
          "</div>"
        )))
      }
      
      get_icon <- function(done) {
        if (done) {
          "<span style='color: #28a745; font-weight: bold;'>[Done]</span>"
        } else {
          "<span style='color: #6c757d;'>[Pending]</span>"
        }
      }
      
      steps <- c(
        sprintf("<p>%s <strong>Step 1:</strong> Upload your pedigree file (.txt or .csv)</p>",
                get_icon(has_file)),
        sprintf("<p>%s <strong>Step 2:</strong> Click 'Run Pedigree Check' to detect and fix issues</p>",
                get_icon(has_results)),
        sprintf("<p>%s <strong>Step 3:</strong> Review results and export the corrected pedigree</p>",
                get_icon(has_results))
      )
      
      if (has_results) {
        steps <- c(steps,
                   "<p><strong>Check Complete!</strong></p>",
                   "<p style='color: #28a745; font-weight: bold;'>Your corrected pedigree is ready to download.</p>"
        )
      }
      shiny::HTML(paste(steps, collapse = ""))
    })
    
    #### Help button ####
    shiny::observeEvent(input$help_btn, {
      shiny::showModal(
        shiny::modalDialog(
          title     = shiny::tagList(shiny::icon("circle-question"), " Pedigree Cleaner — Help"),
          size      = "l",
          easyClose = TRUE,
          footer    = shiny::modalButton("Close"),
          # ↓ single source of truth
          help_content_ped_cleaner(collapse_fn = make_collapse_panel)
        )
      )
    })
    
    #### Run check ####
    shiny::observeEvent(input$run_check, {
      shiny::req(input$ped_file)
      error_message("")
      check_results(NULL)
      shinyjs::disable("download_results")
      
      tryCatch({
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 10, status = "info",
          title = "Reading pedigree file..."
        )
        
        tmp_path <- input$ped_file$datapath
        ped_raw  <- utils::read.table(tmp_path, header = TRUE, sep = "\t",
                                      stringsAsFactors = FALSE, check.names = FALSE)
        
        if (all(c("sire", "dam") %in% colnames(ped_raw)) &&
            !all(c("male_parent", "female_parent") %in% colnames(ped_raw))) {
          colnames(ped_raw)[colnames(ped_raw) == "sire"] <- "male_parent"
          colnames(ped_raw)[colnames(ped_raw) == "dam"]  <- "female_parent"
          tmp_path <- tempfile(fileext = ".txt")
          utils::write.table(ped_raw, tmp_path, sep = "\t",
                             row.names = FALSE, quote = FALSE)
        }
        
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 30, status = "info",
          title = "Checking for duplicate records..."
        )
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 50, status = "info",
          title = "Checking for conflicting IDs and inconsistent parent sex roles..."
        )
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 70, status = "info",
          title = "Checking for missing parents..."
        )
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 85, status = "info",
          title = "Detecting cycles and dependencies..."
        )
        
        report <- BIGr::check_ped(
          ped.file = tmp_path,
          verbose  = FALSE,
          save_zip = FALSE
        )
        
        file_base_actual <- tools::file_path_sans_ext(basename(tmp_path))
        corrected_name   <- paste0(file_base_actual, "_corrected")
        corrected_ped    <- tryCatch(
          get(corrected_name, envir = .GlobalEnv, inherits = FALSE),
          error = function(e) NULL
        )
        
        check_results(list(
          report        = report,
          corrected_ped = corrected_ped
        ))
        
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 100, status = "success",
          title = "Finished"
        )
        shinyjs::enable("download_results")
        
      }, error = function(e) {
        error_message(paste0("Error running pedigree check: ", e$message))
        check_results(NULL)
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_ped",
          value = 100, status = "danger",
          title = "Failed"
        )
      })
    })
    
    #### Summary banner ####
    output$summary_banner <- shiny::renderUI({
      shiny::req(check_results())
      report <- check_results()$report
      
      get_count <- function(df) if (is.null(df) || !is.data.frame(df)) 0L else nrow(df)
      
      n_dupes    <- get_count(report$exact_duplicates)
      n_conflict <- get_count(report$repeated_ids_diff)
      # FIX #1: use correct key name from check_ped() return value [2]
      n_messy    <- get_count(report$inconsistent_sex_roles)
      n_missing  <- get_count(report$missing_parents)
      n_cycles   <- get_count(report$dependencies)
      total      <- n_dupes + n_conflict + n_messy + n_missing + n_cycles
      
      banner_color <- if (total == 0) "#d4edda" else "#fff3cd"
      border_color <- if (total == 0) "#c3e6cb" else "#ffeeba"
      text_color   <- if (total == 0) "#155724"  else "#856404"
      headline     <- if (total == 0) "No issues found. Pedigree looks clean!" else
        paste0(total, " issue(s) detected and corrected. Review the summary below.")
      
      shiny::HTML(paste0(
        "<div style='background-color:", banner_color, "; border: 1px solid ", border_color,
        "; padding: 12px; border-radius: 6px;'>",
        "<p style='color:", text_color, "; margin: 0; font-weight: bold; font-size: 14px;'>",
        headline, "</p>",
        "<p style='color:", text_color, "; margin: 6px 0 0 0; font-size: 12px;'>",
        "- Exact duplicates removed: <strong>", n_dupes, "</strong> &nbsp;",
        "- Conflicting IDs resolved: <strong>", n_conflict, "</strong> &nbsp;",
        "- Inconsistent parent sex roles flagged: <strong>", n_messy, "</strong> &nbsp;",
        "- Missing parents added: <strong>", n_missing, "</strong> &nbsp;",
        "- Cycles detected: <strong>", n_cycles, "</strong>",
        "</p></div>"
      ))
    })
    
    #### Results UI — collapsible detail tables ####
    output$results_ui <- shiny::renderUI({
      shiny::req(check_results())
      report <- check_results()$report
      
      get_count <- function(df) if (is.null(df) || !is.data.frame(df)) 0L else nrow(df)
      
      make_section <- function(title, icon_name, df, color_hex) {
        n <- get_count(df)
        bs4Dash::box(
          title = shiny::tagList(
            shiny::icon(icon_name),
            shiny::strong(paste0(" ", title, ": ")),
            shiny::span(
              if (n == 0) "None found" else paste0(n, " record(s) found"),
              style = paste0("color: ", if (n == 0) "#28a745" else color_hex, ";")
            )
          ),
          width       = 12,
          collapsible = TRUE,
          collapsed   = (n == 0),
          status      = if (n == 0) "success" else "warning",
          style       = paste0("border-left: 4px solid ", color_hex, ";"),
          if (n > 0) {
            DT::DTOutput(session$ns(paste0("tbl_", gsub(" ", "_", tolower(title)))))
          } else {
            shiny::p("No records found.", style = "color: #28a745; margin: 0;")
          }
        )
      }
      
      sections <- shiny::tagList(
        shiny::h5(
          shiny::tagList(shiny::icon("list-check"), " Check Results"),
          style = "font-weight: bold; margin-bottom: 10px;"
        ),
        make_section("Exact Duplicates Removed",       "copy",        report$exact_duplicates,       "#6c757d"),
        make_section("Conflicting IDs Resolved",       "exclamation", report$repeated_ids_diff,      "#856404"),
        make_section("Inconsistent Parent Sex Roles",  "shuffle",     report$inconsistent_sex_roles, "#856404"),
        make_section("Missing Parents Added",          "user-plus",   report$missing_parents,        "#0c5460"),
        make_section("Cycles / Dependencies Detected", "rotate",      report$dependencies,           "#721c24")
      )
      
      # Render DT tables for non-empty sections
      render_if <- function(output_id, df) {
        if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
          output[[output_id]] <<- DT::renderDT({
            DT::datatable(df,
                          rownames = FALSE,
                          options  = list(pageLength = 10, scrollX = TRUE),
                          class    = "table-bordered table-sm")
          })
        }
      }
      
      render_if("tbl_exact_duplicates_removed",         report$exact_duplicates)
      render_if("tbl_conflicting_ids_resolved",         report$repeated_ids_diff)
      render_if("tbl_inconsistent_parent_sex_roles",    report$inconsistent_sex_roles)
      render_if("tbl_missing_parents_added",            report$missing_parents)
      render_if("tbl_cycles_/_dependencies_detected",   report$dependencies)
      
      sections
    })
    
    #### Download ####
    output$download_results <- shiny::downloadHandler(
      filename = function() {
        paste0("pedigree_check_", Sys.Date(), ".zip")
      },
      content = function(file) {
        shiny::req(check_results())
        results <- check_results()
        report  <- results$report
        
        tmp_dir <- tempfile("ped_export")
        dir.create(tmp_dir)
        
        if (!is.null(results$corrected_ped)) {
          write.table(results$corrected_ped,
                      file.path(tmp_dir, "corrected_pedigree.txt"),
                      sep = "\t", row.names = FALSE, quote = FALSE)
        }
        
                sections <- list(
          exact_duplicates       = report$exact_duplicates,
          conflicting_ids        = report$repeated_ids_diff,
          inconsistent_sex_roles = report$inconsistent_sex_roles,
          missing_parents        = report$missing_parents,
          dependencies           = report$dependencies
        )
        
        for (nm in names(sections)) {
          df <- sections[[nm]]
          if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
            write.table(df,
                        file.path(tmp_dir, paste0(nm, ".txt")),
                        sep = "\t", row.names = FALSE, quote = FALSE)
          }
        }
        
        tsv_files <- list.files(tmp_dir)
        zip::zip(zipfile = file, files = tsv_files, root = tmp_dir)
        unlink(tmp_dir, recursive = TRUE)
      },
      contentType = "application/zip"
    )
  })
}