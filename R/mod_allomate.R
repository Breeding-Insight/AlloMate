#' AlloMate analysis module
#'
#' @param id Module id
#'
#' @noRd
mod_allomate_ui <- function(id) {
  ns <- shiny::NS(id)

  # Override: Chromium download attribute bug fix
  downloadButton <- function(...) {
    tag <- shiny::downloadButton(...)
    tag$attribs$download <- NULL
    tag
  }

  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::div(
          id = "startup_guide",
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
            shiny::tagList(shiny::icon("sitemap"), "Core Data Inputs"),
            style = "color: #000000; margin-bottom: 15px; border-bottom: 1px solid #000000; padding-bottom: 8px;"
          ),
          shiny::p(
            "These inputs are used by both Index Generation and OCS calculations:",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"
          ),
          shiny::h5("Estimate progeny genetic merit"),
          shiny::fileInput(ns("candidate_file"), "Upload list of candidates", accept = c(".csv", ".txt")),
          shiny::h5("Calculate kinship matrix"),
          shiny::fileInput(ns("pedigree_file"), "Upload pedigree file", accept = ".txt"),
          shiny::uiOutput(ns("pedigree_status_display")),
          shiny::h5("Set kinship threshold"),
          shiny::numericInput(
            ns("thresh"),
            "Max kinship allowed between mates:",
            value = 1,
            min = 0,
            max = 1,
            step = 0.1
          )
        ),

        shiny::wellPanel(
          style = "background-color: #ffffff; border: 2px solid #444444; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
          shiny::h4(
            shiny::tagList(shiny::icon("balance-scale"), " Weighted EBVs"),
            style = "color: #000000; margin-bottom: 15px; border-bottom: 1px solid #000000; padding-bottom: 8px;"
          ),
          shiny::p(
            "Define traits and their relative importance for breeding decisions:",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"
          ),
          shiny::h5("Traits (EBVs and weights)"),
          shiny::uiOutput(ns("trait_inputs")),
          shiny::fluidRow(
            shiny::column(6, shiny::actionButton(ns("add_trait"), "➕ Add trait")),
            shiny::column(6, shiny::actionButton(ns("remove_trait"), "➖ Remove trait"))
          ),
          shiny::p(
            "Note: Adding or removing traits will require re-uploading files.",
            style = "color: #6c757d; font-size: 11px; font-style: italic; margin-top: 8px;"
          )
        ),

        shiny::wellPanel(
          style = "background-color: #ffffff; border: 2px solid #444444; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
          shiny::h4(
            shiny::tagList(shiny::icon("bullseye"), " Optimum Contribution Selection"),
            style = "color: #000000; margin-bottom: 15px; border-bottom: 1px solid #000000; padding-bottom: 8px;"
          ),
          shiny::p(
            "Configure breeding objectives and constraints:",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"
          ),
          shiny::verbatimTextOutput(ns("package_status_text")),
          # Hidden output used for conditionalPanel logic
          shiny::div(style = "display:none;", shiny::textOutput(ns("ocs_checkbox_mode"))),
          shiny::h5("Breeding Objectives"),
          shiny::numericInput(
            ns("inbreeding_rate"),
            "Desired Inbreeding Rate",
            value = 0.05,
            min = 0.01,
            max = 0.2,
            step = 0.01
          ),
          shiny::numericInput(
            ns("num_offspring"),
            "Number of Offspring",
            value = 100,
            min = 10,
            step = 1
          ),
          shiny::conditionalPanel(
            condition = sprintf("output['%s'] == '1'", ns("ocs_checkbox_mode")),
            shiny::checkboxInput(
              ns("enforce_pair_kinship"),
              "Enforce per-pair kinship threshold in mating plan",
              value = TRUE
            ),
            shiny::checkboxInput(
              ns("force_greedy_mating"),
              "Use greedy mating (browser-safe)",
              value = isTRUE(getOption("allomate.force_greedy_mating", FALSE))
            ),
            shiny::checkboxInput(
              ns("force_qp_greedy"),
              "Bypass quadprog (heuristic contributions)",
              value = isTRUE(getOption("allomate.force_qp_greedy", FALSE))
            )
          ),
          shiny::actionButton(
            ns("run_ocs_btn"),
            "Run OCS",
            style = "margin-top: 15px; width: 100%; background-color: #28a745; color: white; border: none; padding: 10px; border-radius: 5px;"
          )
        ),

        shiny::wellPanel(
          style = "background-color: #ffffff; border: 2px solid #444444; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
          shiny::h4(
            shiny::tagList(shiny::icon("bar-chart"), "Export Results"),
            style = "color: #000000; margin-bottom: 15px; border-bottom: 1px solid #c3e6cb; padding-bottom: 8px;"
          ),
          shiny::p(
            "Download all results in a single zip file:",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"
          ),
          downloadButton(
            ns("download_all_results"),
            "Export All Results",
            style = "width: 100%; background-color: #28a745; color: white; border: none; padding: 10px; border-radius: 5px; margin-bottom: 10px;"
          ),
          shiny::div(
            style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 12px; margin-bottom: 10px; border-radius: 5px;",
            shiny::h5(
              shiny::tagList(shiny::icon("file"), "File Status"),
              style = "color: #495057; margin-top: 0; margin-bottom: 10px; font-size: 14px;"
            ),
            shiny::htmlOutput(ns("file_status_display"))
          ),
          shiny::tags$a(
            href = "https://github.com/Breeding-Insight/AlloMate",
            target = "_blank",
            class = "btn btn-info",
            style = "width: 100%; background-color: #17a2b8; color: white; border: none; padding: 10px; border-radius: 5px; text-align: center;",
            shiny::tagList(shiny::icon("github"), "Visit GitHub repository")
          )
        )
      ),

      shiny::mainPanel(
        bs4Dash::tabsetPanel(
          id = ns("main_tabs"),
          type = "tabs",
          shiny::tabPanel(
            "Kinship and EBV",
            shiny::verbatimTextOutput(ns("message1")),
            shiny::uiOutput(ns("candidate_ebv_status")),
            shiny::uiOutput(ns("ebv_upload_prompt")),
            shiny::uiOutput(ns("message2")),
            DT::DTOutput(ns("quadrants_table")),
            DT::DTOutput(ns("matrix"))
          ),
          shiny::tabPanel(
            "Optimum Contribution Selection",
            shiny::div(
              id = "ocs_container",
              style = "position: relative;",
              shiny::div(
                id = "ocs_loading",
                style = "display: none; position: absolute; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(255,255,255,0.8); z-index: 9999;",
                shiny::div(
                  style = "position: absolute; top: 30%; left: 50%; transform: translate(-50%, -50%);",
                  shiny::div(class = "ocs-spinner")
                )
              ),
              DT::DTOutput(ns("ocs_candidate_table")),
              shiny::uiOutput(ns("ocs_solver_note")),
              shiny::br(),
              DT::DTOutput(ns("ocs_mating_table"))
            )
          )
        )
      )
    )
  )
}

#' AlloMate analysis module server
#'
#' @param id Module id
#' @param parent_session Parent (app) session, used for sidebar navigation
#'
#' @noRd
mod_allomate_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns
      missing_id_data <- reactiveValues(
        candidates = character(),
        ebvs = character()
      )

      # Package status output
      output$package_status_text <- renderText({
        generate_package_status()
      })


      ocs_checkboxes_enabled <- !requireNamespace("optiSel", quietly = TRUE)

      output$ocs_checkbox_mode <- renderText({
        if (ocs_checkboxes_enabled) {
          "1"
        } else {
          ""
        }
      })
      outputOptions(output, "ocs_checkbox_mode", suspendWhenHidden = FALSE)

      if (!ocs_checkboxes_enabled) {
        options(
          allomate.force_greedy_mating = FALSE,
          allomate.force_qp_greedy = FALSE
        )
      }

      # Help button functionality
      observeEvent(input$help_btn, {
        bs4Dash::updatebs4TabItems(
          session = parent_session,
          inputId = "MainMenu",
          selected = "help"
        )
      })

      trait_counter <- reactiveVal(1)
      candidate_status <- reactiveVal(list(ok = FALSE, error = NULL))
      ocs_results_reactive <- reactiveVal()  # For OCS results
      ebv_results_reactive <- reactiveVal()  # For EBV results
      error_message <- reactiveVal("")  # For tracking errors
      pedigree_validation_stats <- reactiveVal(NULL)
      ebv_data <- reactive({
        process_ebvs(trait_counter(), input)
      })

      # Export: Prebuild workbook so Chrome downloads reliably
      # Reactive cache for export
      export_cache <- reactiveVal(NULL)
  
      # Function to generate ZIP of TSVs
      generate_export_zip <- function(dest_zip) {
        safe_char <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
    
        tmp_dir <- tempfile("export_tsvs")
        dir.create(tmp_dir)
    
        # README
        readme_text <- c(
          "📊 AlloMate Complete Results Report",
          "",
          "This TSV collection contains all results from your AlloMate analysis:",
          "",
          "📋 Files included:",
          "1. README - This overview and explanation",
          "2. Filtered Results - Crosses meeting criteria (positive EBVs, kinship below threshold)",
          "3. EBV Matrix - Complete matrix view with masked values",
          "4. OCS Candidates - Selected candidates from Optimum Contribution Selection",
          "5. Mating Plan - Recommended mating pairs from OCS",
          "6. Parameters - Analysis parameters used",
          "",
          "🔍 Data Details:",
          "- Filtered Results: Only crosses with positive EBVs and kinship below threshold",
          "- EBV Matrix: All possible crosses with values masked for failed criteria",
          "- OCS Results: Available only after running Optimum Contribution Selection",
          "",
          "📅 Generated on:", as.character(Sys.Date())
        )
        write.table(data.frame(Text = readme_text, stringsAsFactors = FALSE),
                    file.path(tmp_dir, "README.tsv"), sep="\t", row.names = FALSE, quote = FALSE)
    
        # EBV results
        ebv_results <- ebv_results_reactive()
        if (!is.null(ebv_results)) {
          # Filtered Results
          filtered_results_df <- as.data.frame(ebv_results$filt_results_table)
          write.table(filtered_results_df, file.path(tmp_dir, "Filtered_Results.tsv"),
                      sep="\t", row.names = F, quote = FALSE)
      
          # EBV Matrix
          m_ids <- unique(ebv_results$full_results$Male)
          f_ids <- unique(ebv_results$full_results$Female)
          mat_for_csv <- matrix(NA_real_, nrow = length(m_ids), ncol = length(f_ids),
                                dimnames = list(m_ids, f_ids))
      
          if (nrow(ebv_results$filt_results_matrix) > 0) {
            for (i in seq_len(nrow(ebv_results$filt_results_matrix))) {
              m <- ebv_results$filt_results_matrix$Male[i]
              f <- ebv_results$filt_results_matrix$Female[i]
              val <- ebv_results$filt_results_matrix$EBV[i]
              if (!is.na(m) && !is.na(f) &&
                  m %in% rownames(mat_for_csv) && f %in% colnames(mat_for_csv)) {
                mat_for_csv[m, f] <- val
              }
            }
          }
      
          row_labels <- rownames(mat_for_csv)
          if (is.null(row_labels)) row_labels <- seq_len(nrow(mat_for_csv))
          ebv_matrix_df <- data.frame(
            Male = row_labels,
            mat_for_csv,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          write.table(ebv_matrix_df, file.path(tmp_dir, "EBV_Matrix.tsv"),
                      sep="\t", row.names = FALSE, quote = FALSE, na = "")
        }
    
        # OCS results
        formatted_results <- NULL
        if (!is.null(ocs_results_reactive())) {
          formatted_results <- tryCatch(
            format_ocs_results(ocs_results_reactive()),
            error = function(e) {
              message("format_ocs_results error: ", e$message)
              NULL
            }
          )
          if (!is.null(formatted_results)) {
            write.table(as.data.frame(formatted_results$candidate_table),
                        file.path(tmp_dir, "OCS_Candidates.tsv"),
                        sep="\t", row.names = FALSE, quote = FALSE)
            write.table(as.data.frame(formatted_results$mating_table),
                        file.path(tmp_dir, "Mating_Plan.tsv"),
                        sep="\t", row.names = FALSE, quote = FALSE)
          }
        }
    
        # Parameters
        params_data <- data.frame(
          Parameter = c("Analysis Date", "Kinship Threshold", "Desired Inbreeding Rate", "Number of Offspring"),
          Value = c(
            as.character(Sys.Date()),
            safe_char(input$thresh),
            safe_char(input$inbreeding_rate),
            safe_char(input$num_offspring)
          ),
          stringsAsFactors = FALSE
        )
        write.table(params_data, file.path(tmp_dir, "Parameters.tsv"),
                    sep="\t", row.names = FALSE, quote = FALSE)
    
        # Create ZIP using relative paths
        tsv_files <- list.files(tmp_dir)  # relative file names
        zip::zip(zipfile = dest_zip, files = tsv_files, root = tmp_dir)
    
        unlink(tmp_dir, recursive = TRUE)
        TRUE
      }
  
      # Observer to automatically export ZIP
      observeEvent({
        list(
          ebv_results_reactive(),
          ocs_results_reactive(),
          input$thresh,
          input$inbreeding_rate,
          input$num_offspring
        )
      }, {
        tmp_zip <- tempfile(pattern = "allomate_export_", fileext = ".zip")
    
        if (generate_export_zip(tmp_zip) && file.exists(tmp_zip)) {
          old <- export_cache()
          export_cache(tmp_zip)
          if (!is.null(old) && file.exists(old)) unlink(old, force = TRUE)
        }
      }, ignoreNULL = FALSE)

      session$onSessionEnded(function() {
        cache <- shiny::isolate(export_cache())
        if (!is.null(cache) && file.exists(cache)) unlink(cache, force = TRUE)
      })

      output$download_all_results <- downloadHandler(
        filename = function() {
          paste0("AlloMate_results-", Sys.Date(), ".zip")
        },
        content = function(file) {
          cache <- shiny::isolate(export_cache())
          if (!is.null(cache) && file.exists(cache)) {
            file.copy(cache, file, overwrite = TRUE)
          } else {
            generate_export_xlsx(file)
          }
        },
        contentType = "application/zip"
      )
      outputOptions(output, "download_all_results", suspendWhenHidden = FALSE)

      # Dynamic startup guide
      output$dynamic_guide <- renderUI({
        # Check for errors first
        current_error <- error_message()
        if (current_error != "") {
          return(HTML(paste0(
            "<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px; border-radius: 4px; margin-bottom: 10px;'>",
            "<p style='color: #721c24; margin: 0;'><strong>❌ Error occurred:</strong></p>",
            "<p style='color: #721c24; margin: 5px 0; font-family: monospace; font-size: 11px;'>", current_error, "</p>",
            "<p style='color: #721c24; margin: 5px 0 0 0; font-size: 12px;'>💡 Need help? Check the Help tab for detailed documentation.</p>",
            "</div>"
          )))
        }

        # Check current state
        has_candidates <- !is.null(input$candidate_file)
        has_pedigree <- !is.null(input$pedigree_file)
        has_kinship_threshold <- !is.null(input$thresh) && input$thresh < 1
        has_traits <- trait_counter() > 0 && any(sapply(1:trait_counter(), function(i) !is.null(input[[paste0("trait_file_", i)]])))
        has_ocs_results <- !is.null(ocs_results_reactive())

        # Determine error type from error message, if not working properly, update the grep logic
        pedigree_error <- current_error != "" && grepl("error|pedigree|kinship", current_error, ignore.case = TRUE)
        trait_error <- current_error != "" && grepl("error|ebv|trait|weight", current_error, ignore.case = TRUE)
        ocs_error <- current_error != "" && grepl("error|ocs", current_error, ignore.case = TRUE)

        cs <- candidate_status()
        candidate_ready <- isTRUE(cs$ok)
        candidate_error_flag <- !is.null(cs$error)

        # Helper to decide icon per step
        get_step_icon <- function(has_item, error_flag) {
          if (error_flag) {
            "❌"
          } else if (has_item) {
            "✅"
          } else {
            "⬜"
          }
        }

        # Build step-by-step guide
        steps <- c()

        # Step 1: Candidates
        step1_icon <- get_step_icon(candidate_ready, candidate_error_flag)
        steps <- c(steps, sprintf("<p><strong>%s Step 1:</strong> Upload your candidate list to begin the analysis</p>", step1_icon))

        # Step 2: Pedigree
        step2_icon <- get_step_icon(has_pedigree, pedigree_error)
        steps <- c(steps, sprintf("<p><strong>%s Step 2:</strong> Upload your pedigree file for kinship calculations</p>", step2_icon))

        # Step 3: Kinship threshold
        if (has_pedigree) {
          steps <- c(steps, "<p><strong>⚙️ Step 3:</strong> Set your kinship threshold (optional)</p>")
        } else {
          steps <- c(steps, "<p><strong>Step 3:</strong> Set your kinship threshold (optional)</p>")
        }

        # Step 4: Traits
        step4_icon <- get_step_icon(has_traits, trait_error)
        steps <- c(steps, sprintf("<p><strong>%s Step 4:</strong> Add trait files and weights for breeding value analysis</p>", step4_icon))

        # Step 5: OCS
        step5_icon <- get_step_icon(has_ocs_results, ocs_error)
        steps <- c(steps, sprintf("<p><strong>%s Step 5:</strong> Configure OCS parameters and run analysis</p>", step5_icon))

        # Add completion message if all steps are done
        if (has_ocs_results) {
          steps <- c(steps,
                     "<p><strong>🎉 Analysis Complete!</strong></p>",
                     "<p><strong>Final Step:</strong> Export your results using the download button below</p>",
                     "<p style='color: #28a745; font-weight: bold;'>Great job! Results are ready for review and export</p>"
          )
        }

        HTML(paste(steps, collapse = ""))
      })

      # File status display
      output$file_status_display <- renderUI({
        # Check what files/data are ready
        has_candidates <- !is.null(input$candidate_file)
        has_pedigree <- !is.null(input$pedigree_file)
        has_ebv <- !is.null(ebv_results_reactive())
        has_ocs <- !is.null(ocs_results_reactive())

        # Check for errors
        current_error <- error_message()
        has_error <- current_error != ""

        cs <- candidate_status()
        candidate_ready <- isTRUE(cs$ok)
        candidate_error_flag <- !is.null(cs$error)

        # Determine error type from error message
        pedigree_error <- has_error && grepl("pedigree|kinship", current_error, ignore.case = TRUE)
        ebv_error <- has_error && grepl("ebv|trait|weight", current_error, ignore.case = TRUE)
        ocs_error <- has_error && grepl("ocs", current_error, ignore.case = TRUE)

        # Helper function to determine status icon and text
        get_status <- function(has_data, uploaded, has_specific_error, ready_text = "Ready", pending_text = "Not uploaded", error_text = "Error") {
          if (has_specific_error) {
            list(icon = "❌", text = paste0("<span style='color: #dc3545;'>", error_text, "</span>"))
          } else if (has_data) {
            list(icon = "✅", text = paste0("<span style='color: #28a745;'>", ready_text, "</span>"))
          } else if (uploaded) {
            list(icon = "⬜", text = paste0("<span style='color: #ffc107;'>Processing...</span>"))
          } else {
            list(icon = "⬜", text = paste0("<span style='color: #6c757d;'>", pending_text, "</span>"))
          }
        }

        # Candidate status with explicit tracking
        candidate_status_ui <- if (candidate_error_flag) {
          list(icon = "❌", text = "<span style='color: #dc3545;'>Error</span>")
        } else if (candidate_ready) {
          list(icon = "✅", text = "<span style='color: #28a745;'>Ready</span>")
        } else if (has_candidates) {
          list(icon = "⬜", text = "<span style='color: #ffc107;'>Processing...</span>")
        } else {
          list(icon = "⬜", text = "<span style='color: #6c757d;'>Not uploaded</span>")
        }

        # Pedigree status
        pedigree_status <- get_status(has_pedigree, has_pedigree, pedigree_error,
                                      "Ready", "Not uploaded", "Error")

        # EBV status - can error if uploaded but processing failed
        ebv_status <- get_status(has_ebv,
                                 has_candidates && has_pedigree,
                                 ebv_error,
                                 "Ready", "Pending", "Error")

        # OCS status
        ocs_status <- get_status(has_ocs, FALSE, ocs_error,
                                 "Ready", "Not run", "Error")

        # Create status lines with emojis
        status_lines <- c(
          paste0(
            candidate_status_ui$icon,
            " <strong>Candidate List:</strong> ",
            candidate_status_ui$text
          ),
          paste0(
            pedigree_status$icon,
            " <strong>Pedigree Data:</strong> ",
            pedigree_status$text
          ),
          paste0(
            ebv_status$icon,
            " <strong>EBV Matrix:</strong> ",
            ebv_status$text
          ),
          paste0(
            ocs_status$icon,
            " <strong>OCS Results:</strong> ",
            ocs_status$text
          )
        )

        # Check if all files are ready for download
        all_ready <- candidate_ready && has_pedigree && has_ebv

        download_status <- if (has_error) {
          "<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px; margin-top: 10px; border-radius: 3px;'>
            <p style='color: #721c24; margin: 0; font-size: 12px;'><strong>⚠️ Error detected.</strong> Check the startup guide above for details.</p>
          </div>"
        } else if (all_ready) {
          "<div style='background-color: #d4edda; border: 1px solid #c3e6cb; padding: 8px; margin-top: 10px; border-radius: 3px;'>
            <p style='color: #155724; margin: 0; font-size: 12px;'><strong>✅ Download ready!</strong> All core data is available.</p>
          </div>"
        } else {
          "<div style='background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 8px; margin-top: 10px; border-radius: 3px;'>
            <p style='color: #6c757d; margin: 0; font-size: 12px;'>Upload required files to enable download.</p>
          </div>"
        }

        HTML(paste0(
          "<div style='font-size: 12px; line-height: 1.8;'>",
          paste(status_lines, collapse = "<br>"),
          "</div>",
          download_status
        ))
      })

      output$pedigree_status_display <- renderUI({
        stats <- pedigree_validation_stats()
        if (is.null(stats)) {
          return(NULL)
        }

        get_count <- function(val) {
          if (is.null(val) || is.na(val)) {
            0L
          } else {
            as.integer(val)
          }
        }

        format_count <- function(val) {
          format(get_count(val), big.mark = ",", scientific = FALSE)
        }

        records <- format_count(stats$records_loaded)
        unknown_count <- get_count(stats$unknown_parent_count)
        circular_count <- get_count(stats$circular_reference_count)
        missing_count <- get_count(stats$missing_candidates)
        duplicates <- get_count(stats$duplicates_removed)

        green_box <- paste0(
          "<div style='background-color: #d4edda; border: 1px solid #c3e6cb; padding: 8px; border-radius: 3px; margin-top: 10px; font-size: 12px;'>",
          "✅ ", records, " records loaded",
          "</div>"
        )

        # Build yellow box warnings only for non-zero counts
        yellow_warnings <- c()
        if (unknown_count > 0) {
          yellow_warnings <- c(yellow_warnings,
                               paste0("<p style='margin: ", if(length(yellow_warnings) == 0) "0" else "4px 0 0", ";'>⚠️ ",
                                      format_count(stats$unknown_parent_count),
                                      " individuals with unknown parent(s) (treated as founders)</p>"))
        }
        if (circular_count > 0) {
          yellow_warnings <- c(yellow_warnings,
                               paste0("<p style='margin: ", if(length(yellow_warnings) == 0) "0" else "4px 0 0", ";'>⚠️ ",
                                      format_count(stats$circular_reference_count),
                                      " circular references detected and broken at earliest generation</p>"))
        }
        if (missing_count > 0) {
          yellow_warnings <- c(yellow_warnings,
                               paste0("<p style='margin: ", if(length(yellow_warnings) == 0) "0" else "4px 0 0", ";'>⚠️ ",
                                      format_count(stats$missing_candidates),
                                      " selection candidates missing from pedigree</p>"))
        }

        yellow_box <- if (length(yellow_warnings) > 0) {
          paste0(
            "<div style='background-color: #fff3cd; border: 1px solid #ffeeba; padding: 8px; border-radius: 3px; margin-top: 6px; font-size: 12px;'>",
            paste(yellow_warnings, collapse = ""),
            "</div>"
          )
        } else {
          ""
        }

        red_box <- if (duplicates > 0) {
          paste0(
            "<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px; border-radius: 3px; margin-top: 6px; font-size: 12px;'>",
            "❌ ", format(duplicates, big.mark = ",", scientific = FALSE), " duplicates removed",
            "</div>"
          )
        } else {
          ""
        }

        HTML(paste0(green_box, yellow_box, red_box))
      })

      observeEvent(input$add_trait, {
        trait_counter(trait_counter() + 1)
      })

      observeEvent(input$remove_trait, {
        if (trait_counter() > 1) {
          trait_counter(trait_counter() - 1)
        }
      })

      output$trait_inputs <- renderUI({
        create_trait_inputs(trait_counter(), ns = ns)
      })

      # OCS trait inputs
      output$ocs_trait_inputs <- renderUI({
        req(input$ocs_trait_counter)
        create_ocs_trait_inputs(input$ocs_trait_counter, ns = ns)
      })

      candidates_data <- reactive({
        req(input$candidate_file)

        tryCatch({
          res <- read_candidates(input$candidate_file)
          candidate_status(list(ok = TRUE, error = NULL))

          current_err <- error_message()
          if (current_err != "" && grepl("^Error processing candidates", current_err)) {
            error_message("")
          }

          res
        }, error = function(e) {
          candidate_status(list(ok = FALSE, error = e$message))
          error_message(paste0("Error processing candidates: ", e$message))
          validate(need(FALSE, e$message))
          NULL
        })
      })

      pedigree_data <- reactiveVal(NULL)

      observeEvent(input$pedigree_file, {
        req(candidates_data())
        males <- candidates_data()$males
        females <- candidates_data()$females

        tryCatch({
          raw_ped <- readr::read_table(input$pedigree_file$datapath)
          cleaned_ped <- clean_pedigree(raw_ped, return_stats = TRUE)
          final_ped <- cleaned_ped$pedigree

          # Check for candidates missing from pedigree
          candidate_ids <- candidates_data()$candidates$id
          pedigree_ids <- as.character(raw_ped$id)
          missing_candidate_ids <- setdiff(candidate_ids, pedigree_ids)
          missing_candidates <- length(missing_candidate_ids)
          missing_male_ids <- intersect(missing_candidate_ids, males)
          missing_female_ids <- intersect(missing_candidate_ids, females)
          remaining_missing_ids <- setdiff(missing_candidate_ids, c(missing_male_ids, missing_female_ids))

          # Add missing candidates counts to stats
          cleaned_ped$stats$missing_candidates <- missing_candidates
          cleaned_ped$stats$missing_candidate_ids <- missing_candidate_ids

          kinship_res <- compute_kinship_matrix(final_ped, males, females)

          output$quadrants_table <- DT::renderDT({
            DT::datatable(kinship_res$quads, options = list(ordering = FALSE, dom = "t"), rownames = TRUE) %>%
              DT::formatStyle(colnames(kinship_res$quads), styleEqual(
                kinship_res$quads[1, ], c("lightgreen", "yellow", "orange", "coral")
              ))
          })

          pedigree_data(list(results = kinship_res$results, quads = kinship_res$quads))
          pedigree_validation_stats(cleaned_ped$stats)

          error_message("")  # Clear any previous errors

          format_missing_msg <- function(ids, label) {
            if (length(ids) == 0) {
              return(NULL)
            }
            ids_str <- format_id_list(ids)
            plural <- if (length(ids) == 1) "" else "s"
            if (ids_str != "") {
              paste0(length(ids), " ", label, plural,
                     " missing from pedigree and not visualized (IDs: ", ids_str, ").")
            } else {
              paste0(length(ids), " ", label, plural,
                     " missing from pedigree and not visualized.")
            }
          }

          kinship_mismatch_msgs <- Filter(Negate(is.null), list(
            format_missing_msg(missing_male_ids, "male candidate"),
            format_missing_msg(missing_female_ids, "female candidate"),
            format_missing_msg(remaining_missing_ids, "candidate")
          ))
          kinship_mismatch_msgs <- unlist(kinship_mismatch_msgs)

          kinship_status <- if (length(kinship_mismatch_msgs) == 0) {
            "✅ Kinship matrix generated successfully."
          } else {
            paste(
              "⚠️ Kinship matrix generated with warnings.",
              paste(kinship_mismatch_msgs, collapse = " ")
            )
          }

          output$message1 <- renderText(kinship_status)
        }, error = function(e) {
          error_message(paste0("Error processing pedigree: ", e$message))
          pedigree_validation_stats(NULL)
          output$message1 <- renderText(
            paste0("❌ Error processing pedigree: Make sure your pedigree is clean and valid.\n",
                   "Original error: ", e$message)
          )
        })
      })

      observe({
        if (is.null(ebv_data())) {
          output$message2 <- renderUI(NULL)
          output$candidate_ebv_status <- renderUI(NULL)
        }
      })

      output$ebv_upload_prompt <- renderUI({
        if (!is.null(pedigree_data()) && is.null(ebv_data())) {
          tags$pre(
            class = "shiny-text-output",
            "⬜️ Please upload trait EBVs."
          )
        } else {
          NULL
        }
      })

      observe({
        req(candidates_data())
        ebv_res <- ebv_data()
        req(ebv_res)

        joint_ebvs <- ebv_res$joint_ebvs
        rel_weights <- ebv_res$rel_weights
        weight_total <- ebv_res$weight_total

        ebv_cols <- paste0("EBV.", seq_along(rel_weights))
        # Ensure EBV columns are numeric for matrix multiplication
        joint_ebvs[ebv_cols] <- lapply(joint_ebvs[ebv_cols], as.numeric)
        joint_ebvs$index_val <- as.vector(as.matrix(joint_ebvs[ebv_cols]) %*% rel_weights)

        cands <- candidates_data()$candidates
        males <- candidates_data()$males
        females <- candidates_data()$females

        ebv_ids <- unique(joint_ebvs$ID)
        ebv_only_ids <- setdiff(ebv_ids, cands$id)
        joint_ebvs_filtered <- joint_ebvs %>%
          filter(ID %in% cands$id)

        cand_ebv <- left_join(cands, joint_ebvs_filtered, by = c("id" = "ID")) %>%
          select(id, sex, index_val)

        candidate_missing_ids <- cand_ebv %>%
          filter(is.na(index_val)) %>%
          pull(id)

        missing_id_data$candidates <- candidate_missing_ids
        missing_id_data$ebvs <- ebv_only_ids

        m_ebv <- cand_ebv %>%
          filter(id %in% males, !is.na(index_val)) %>%
          select(id, index_val)
        f_ebv <- cand_ebv %>%
          filter(id %in% females, !is.na(index_val)) %>%
          select(id, index_val)

        valid_pairs <- nrow(m_ebv) > 0 && nrow(f_ebv) > 0

        if (valid_pairs) {
          ebv_matrix <- outer(m_ebv$index_val, f_ebv$index_val, function(x, y) round((x + y) / 2, 2))
          rownames(ebv_matrix) <- m_ebv$id
          colnames(ebv_matrix) <- f_ebv$id

          ebv_quads <- tibble(
            Data = "EBV",
            Q25 = quantile(ebv_matrix, 0.25, na.rm = TRUE),
            Q50 = quantile(ebv_matrix, 0.50, na.rm = TRUE),
            Q75 = quantile(ebv_matrix, 0.75, na.rm = TRUE),
            Q100 = quantile(ebv_matrix, 1.00, na.rm = TRUE)
          ) %>% column_to_rownames("Data")

          ebv_results <- as_tibble(ebv_matrix, rownames = "Male") %>%
            pivot_longer(-Male, names_to = "Female", values_to = "EBV")

          full_results <- if (!is.null(pedigree_data())) {
            left_join(pedigree_data()$results, ebv_results, by = c("Female", "Male"))
          } else {
            relocate(mutate(ebv_results, Kinship = NA), Kinship, .after = EBV)
          }
        } else {
          ebv_quads <- tibble(
            Data = "EBV",
            Q25 = NA_real_,
            Q50 = NA_real_,
            Q75 = NA_real_,
            Q100 = NA_real_
          ) %>% column_to_rownames("Data")

          if (!is.null(pedigree_data())) {
            full_results <- pedigree_data()$results %>%
              mutate(EBV = NA_real_)
          } else {
            full_results <- tibble(
              Male = character(),
              Female = character(),
              Kinship = numeric(),
              EBV = numeric()
            )
          }

          ebv_results <- tibble(
            Male = character(),
            Female = character(),
            EBV = numeric()
          )
        }

        quads_combined <- if (!is.null(pedigree_data())) {
          dplyr::bind_rows(pedigree_data()$quads, ebv_quads)
        } else {
          ebv_quads
        }

        output$quadrants_table <- DT::renderDT({
          DT::datatable(quads_combined, options = list(ordering = FALSE, dom = "t"), rownames = TRUE) %>%
            DT::formatStyle("Q25", backgroundColor = "coral") %>%
            DT::formatStyle("Q50", backgroundColor = "orange") %>%
            DT::formatStyle("Q75", backgroundColor = "yellow") %>%
            DT::formatStyle("Q100", backgroundColor = "lightgreen")
        })

        # Filter out crosses with EBV <= 0 or kinship >= threshold
        filt_results_table <- full_results %>%
          filter(EBV > 0, (is.na(Kinship) | Kinship < input$thresh))

        # For Excel: mask EBVs for invalid crosses as NA (blank in Excel)
        filt_results_matrix <- full_results %>%
          mutate(EBV = ifelse(EBV <= 0 | (!is.na(Kinship) & Kinship >= input$thresh), NA, EBV))

        # Store results for download handler
        ebv_results_reactive(list(
          filt_results_table = filt_results_table,
          filt_results_matrix = filt_results_matrix,
          full_results = full_results,
          ebv_quads = ebv_quads
        ))

        output$matrix <- DT::renderDT({
          dt <- DT::datatable(filt_results_table, rownames = TRUE)
          if (valid_pairs && nrow(filt_results_table) > 0) {
            dt <- dt %>%
              DT::formatStyle("EBV", styleInterval(
                unlist(ebv_quads[1, c("Q25", "Q50", "Q75")]),
                c("coral", "orange", "yellow", "lightgreen")
              ))
          }
          dt
        })

        status_components <- list()

        total_candidates <- dplyr::n_distinct(cands$id)
        total_ebvs <- length(ebv_ids)
        candidate_match_ui <- NULL
        if (total_candidates > 0 &&
            length(candidate_missing_ids) == 0 &&
            total_ebvs >= total_candidates) {
          candidate_match_ui <- tags$pre(
            class = "shiny-text-output",
            "✅ All selection candidates have corresponding EBVs"
          )
        }

        output$candidate_ebv_status <- renderUI(candidate_match_ui)

        if (length(candidate_missing_ids) > 0) {
          display_ids <- head(candidate_missing_ids, 3)
          extra_count <- length(candidate_missing_ids) - length(display_ids)
          display_str <- paste(display_ids, collapse = ", ")
          candidate_text <- paste0("Candidates lack corresponding EBVs: ", display_str)
          if (extra_count > 0) {
            candidate_text <- paste0(candidate_text, " and ", extra_count, " more")
          }
          candidate_text <- paste0(candidate_text, " will be excluded from further analysis, ")

          status_components <- append(status_components, list(
            tags$p(
              "❗",
              candidate_text,
              downloadLink(ns("download_missing_candidates"), "view here"),
              "."
            )
          ))
        }

        if (length(ebv_only_ids) > 0) {
          status_components <- append(status_components, list(
            tags$p(
              sprintf("🗂️ EBVs filtered for matching candidates: %d EBVs removed.", length(ebv_only_ids))
            )
          ))
        }

        if (!valid_pairs) {
          status_components <- append(status_components, list(
            tags$p("❌ EBV matrix not visualized because no overlapping candidates with EBVs remain.")
          ))
        }

        if (abs(weight_total - 1) > 1e-6) {
          status_components <- append(status_components, list(
            tags$p(
              paste("⚠️ Warning: Trait weights do not sum to 1 (total =", round(weight_total, 4), ")")
            )
          ))
        }

        status_body <- if (length(status_components) == 0) {
          tags$p("✅ EBV matrix generated successfully.")
        } else {
          do.call(tagList, status_components)
        }

        status_ui <- tags$div(
          style = "background-color: #f0f0f0; border: 1px solid #d0d0d0; padding: 12px; border-radius: 6px; margin-top: 10px;",
          tags$h5("ℹ️ Candidate-EBV Mapping Summary", style = "margin-top: 0; font-weight: 600;"),
          status_body
        )

        output$message2 <- renderUI(status_ui)

        # download handler defined at top-level above
      })

      output$download_missing_candidates <- downloadHandler(
        filename = function() {
          paste0("candidates_missing_ebvs_", Sys.Date(), ".txt")
        },
        content = function(file) {
          ids <- missing_id_data$candidates
          lines <- if (length(ids) == 0) {
            c("Candidates lacking EBVs:", "None.")
          } else {
            c("Candidates lacking EBVs:", ids)
          }
          writeLines(lines, con = file)
        }
      )

      #### OCS Server Logic ####

      observeEvent(input$force_greedy_mating, {
        if (!ocs_checkboxes_enabled) {
          return(NULL)
        }
        options(allomate.force_greedy_mating = isTRUE(input$force_greedy_mating))
      }, ignoreNULL = FALSE)

      observeEvent(input$force_qp_greedy, {
        if (!ocs_checkboxes_enabled) {
          return(NULL)
        }
        options(allomate.force_qp_greedy = isTRUE(input$force_qp_greedy))
      }, ignoreNULL = FALSE)

      observeEvent(input$run_ocs_btn, {
        req(input$pedigree_file, input$candidate_file)

        if (ocs_checkboxes_enabled) {
          options(allomate.force_greedy_mating = isTRUE(input$force_greedy_mating))
          options(allomate.force_qp_greedy = isTRUE(input$force_qp_greedy))
        } else {
          options(allomate.force_greedy_mating = FALSE)
          options(allomate.force_qp_greedy = FALSE)
        }

        # Show OCS loading spinner for the duration of the analysis
        shinyjs::show("ocs_loading")
        on.exit(shinyjs::hide("ocs_loading"), add = TRUE)

        tryCatch({
          ped_data <- read.table(input$pedigree_file$datapath, header = TRUE, stringsAsFactors = FALSE)
          candidates <- read.table(input$candidate_file$datapath, header = TRUE, stringsAsFactors = FALSE)

          final_ped <- clean_pedigree(ped_data)
          kinship_matrix <- if (requireNamespace("kinship2", quietly = TRUE)) {
            kinship2::kinship(final_ped)
          } else {
            fallback_kinship(final_ped)
          }

          ebv_result <- process_ebvs(trait_counter(), input)
          if (is.null(ebv_result) || abs(ebv_result$weight_total - 1) > 1e-6) {
            showModal(modalDialog(
              title = "Invalid Weights",
              "Weights must sum to 1.",
              easyClose = TRUE
            ))
            return(NULL)
          }

          joint_ebvs <- calculate_index(ebv_result$joint_ebvs, ebv_result$rel_weights)
          joint_ebvs_ids <- unique(joint_ebvs$ID)
          ebv_only_ids <- setdiff(joint_ebvs_ids, candidates$id)
          joint_ebvs_filtered <- joint_ebvs %>% filter(ID %in% candidates$id)

          candidates_joined <- left_join(candidates, joint_ebvs_filtered, by = c("id" = "ID"))
          missing_candidate_ids <- candidates_joined %>%
            filter(is.na(index_val)) %>%
            pull(id)

          if (length(missing_candidate_ids) > 0) {
            display_ids <- head(missing_candidate_ids, 3)
            extra_count <- length(missing_candidate_ids) - length(display_ids)
            warning_msg <- paste0(
              "OCS: Removed ", length(missing_candidate_ids),
              " candidate(s) lacking EBVs (IDs: ",
              paste(display_ids, collapse = ", ")
            )
            if (extra_count > 0) {
              warning_msg <- paste0(warning_msg, " and ", extra_count, " more")
            }
            warning_msg <- paste0(warning_msg, ").")
            showNotification(warning_msg, type = "warning", duration = 8)
          }

          if (length(ebv_only_ids) > 0) {
            showNotification("OCS: EBV records without candidates were ignored.", type = "warning", duration = 8)
          }

          candidates_filtered <- candidates_joined %>% filter(!is.na(index_val))

          if (nrow(candidates_filtered) == 0) {
            stop("❌ No candidates with EBVs available for OCS. Please ensure at least one candidate of each sex has an EBV.")
          }

          results <- run_ocs(
            candidates_df = candidates_filtered,
            kinship_matrix = kinship_matrix,
            ebv_index = candidates_filtered$index_val,
            desired_inbreeding_rate = input$inbreeding_rate,
            num_offspring = input$num_offspring,
            per_pair_kinship_limit = if (ocs_checkboxes_enabled && isTRUE(input$enforce_pair_kinship)) input$inbreeding_rate else NULL
          )

          mating_info <- attr(results$Mating, "info")
          if (!is.null(mating_info) && is.character(mating_info) &&
              grepl("Greedy allocation", mating_info, fixed = TRUE)) {
            showNotification(
              paste0("OCS fallback: ", mating_info, ". lpSolve solution could not be used, so a greedy mating plan was generated."),
              type = "warning",
              duration = 10
            )
          }

          ocs_results_reactive(results)

          error_message("")  # Clear any previous errors

          # Format results for display
          formatted_results <- format_ocs_results(results)

          output$ocs_candidate_table <- DT::renderDT({
            DT::datatable(
              formatted_results$candidate_table,
              options = list(pageLength = 10, autoWidth = TRUE),
              rownames = FALSE
            )
          })

          # Switch to OCS tab to show results
          updateTabsetPanel(session, "main_tabs", selected = "Optimum Contribution Selection")

        }, error = function(e) {
          error_message(paste0("Error running OCS: ", e$message))
          showModal(modalDialog(
            title = "Error",
            paste("❌ Error running OCS:", e$message),
            easyClose = TRUE
          ))
        })
      })

      output$ocs_mating_table <- DT::renderDT({
        req(ocs_results_reactive())
        results <- ocs_results_reactive()
        formatted_results <- format_ocs_results(results)
        mating_tbl <- formatted_results$mating_table

        # Optionally enforce per-pair kinship threshold for display
        if (ocs_checkboxes_enabled && isTRUE(input$enforce_pair_kinship)) {
          mating_tbl <- mating_tbl %>%
            dplyr::filter(is.na(Kinship) | Kinship < input$inbreeding_rate)
        }

        mating_tbl %>%
          DT::datatable(
            options = list(pageLength = 10, autoWidth = TRUE),
            rownames = FALSE
          )
      })

      output$ocs_solver_note <- renderUI({
        req(ocs_results_reactive())
        formatted_results <- format_ocs_results(ocs_results_reactive())
        info <- formatted_results$summary_stats$mating_info
        if (is.null(info) || is.na(info) || info == "") {
          return(NULL)
        }
        div(
          style = "margin: 10px 0; padding: 10px; background-color: #fff8e1; border-left: 4px solid #ffb300; font-size: 13px;",
          tags$strong("Solver note: "), info
        )
      })

      #### R Code Display and Download ####

      # HTML escape function for code display
      htmlEscape <- function(text) {
        text <- gsub("&", "&amp;", text)
        text <- gsub("<", "&lt;", text)
        text <- gsub(">", "&gt;", text)
        text <- gsub('"', "&quot;", text)
        text <- gsub("'", "&#39;", text)
        return(text)
      }
  })
}
