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
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      
      # Column 1: Inputs
      shiny::column(
        width = 3,
        bs4Dash::box(
          title       = "Inputs",
          width       = 12,
          collapsible = TRUE,
          collapsed   = FALSE,
          status      = "info",
          solidHeader = TRUE,
          shiny::p(
            "Upload required files and configure parameters below.",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"
          ),
          
          # --- Core Data ---
          shiny::h5(
            shiny::tagList(shiny::icon("sitemap"), " Core Data Inputs"),
            style = "border-bottom: 1px solid #dee2e6; padding-bottom: 6px; margin-bottom: 10px;"
          ),
          shiny::p(
            "Used by both Index Generation and OCS calculations:",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 10px;"
          ),
          shiny::h6("Estimate progeny genetic merit"),
          shiny::fileInput(ns("candidate_file"), "Upload list of candidates", accept = c(".csv", ".txt")),
          shiny::h6("Calculate kinship matrix"),
          shiny::selectInput(
            ns("matrix_source"), "Relationship matrix source",
            choices = c(
              "Compute A matrix from pedigree"    = "pedigree",
              "Upload precomputed matrix (A/G/H)" = "upload"
            ),
            selected = "pedigree"
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'pedigree'", ns("matrix_source")),
            shiny::fileInput(ns("pedigree_file"), "Upload pedigree file", accept = ".txt"),
            shiny::uiOutput(ns("pedigree_status_display"))
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'upload'", ns("matrix_source")),
            shiny::fileInput(
              ns("relationship_matrix_file"),
              "Upload relationship matrix (.csv, IDs as row & column names)",
              accept = c(".csv", ".txt")
            ),
            shiny::checkboxInput(
              ns("matrix_is_kinship"),
              "Values are already kinship coefficients (½ scale)",
              value = FALSE
            ),
            shiny::p(
              shiny::HTML(
                "Leave unchecked for a standard A/G/H relationship matrix
                (diagonal ≈ 1+F), e.g. from the <strong>Matrix Builder</strong> tab.
                Check this box if the diagonal is already on the kinship (½) scale."
              ),
              style = "color: #6c757d; font-size: 11px; margin-top: 6px;"
            )
          ),
          shiny::h6("Set kinship threshold"),
          shiny::numericInput(
            ns("thresh"),
            "Max kinship allowed between mates:",
            value = 1, min = 0, max = 1, step = 0.1
          ),
          shiny::hr(),
          
          # --- Weighted EBVs ---
          shiny::h5(
            shiny::tagList(shiny::icon("balance-scale"), " Weighted EBVs"),
            style = "border-bottom: 1px solid #dee2e6; padding-bottom: 6px; margin-bottom: 10px;"
          ),
          shiny::p(
            "Define traits and their relative importance for breeding decisions:",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 10px;"
          ),
          shiny::h6("Traits (EBVs and weights)"),
          shiny::uiOutput(ns("trait_inputs")),
          shiny::fluidRow(
            shiny::column(6, shiny::actionButton(ns("add_trait"),    "+ Add trait")),
            shiny::column(6, shiny::actionButton(ns("remove_trait"), "- Remove trait"))
          ),
          shiny::p(
            "Note: Adding or removing traits will require re-uploading files.",
            style = "color: #6c757d; font-size: 11px; font-style: italic; margin-top: 8px;"
          ),
          shiny::hr(),
          
          # --- OCS ---
          shiny::h5(
            shiny::tagList(shiny::icon("bullseye"), " Optimum Contribution Selection"),
            style = "border-bottom: 1px solid #dee2e6; padding-bottom: 6px; margin-bottom: 10px;"
          ),
          shiny::p(
            "Configure breeding objectives and constraints:",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 10px;"
          ),
          shiny::verbatimTextOutput(ns("package_status_text")),
          shiny::div(style = "display:none;", shiny::textOutput(ns("ocs_checkbox_mode"))),
          shiny::h6("Breeding Objectives"),
          shiny::numericInput(
            ns("inbreeding_rate"), "Desired Inbreeding Rate",
            value = 0.05, min = 0.01, max = 0.2, step = 0.01
          ),
          shiny::numericInput(
            ns("num_offspring"), "Number of Offspring",
            value = 100, min = 10, step = 1
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
          shiny::actionButton(ns("run_ocs_btn"), "Run OCS"),
          shiny::hr(),
          
          # --- Export ---
          shiny::h5(
            shiny::tagList(shiny::icon("bar-chart"), " Export Results"),
            style = "border-bottom: 1px solid #dee2e6; padding-bottom: 6px; margin-bottom: 10px;"
          ),
          shiny::p(
            "Download the mate allocation plan (with kinship and OCS tabs) as an Excel file:",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 10px;"
          ),
          shinyjs::disabled(
            downloadButton(ns("download_mate_allocation"), "Download Mate Allocation")
          ),
          shiny::p(
            "Or download every results table (kinship, EBV matrix, OCS, mating plan, parameters) as a zip of TSV files:",
            style = "color: #6c757d; font-size: 12px; margin: 10px 0;"
          ),
          shinyjs::disabled(
            downloadButton(ns("download_all_tsv"), "Download All Tables (.zip)")
          ),
          shiny::hr(),
          
          # Help button
          shiny::div(
            style = "text-align: center; margin-top: 5px;",
            shiny::actionButton(
              ns("help_btn"),
              shiny::tagList(shiny::icon("circle-question"), "Help"),
              style = "background-color: #FFD700; color: #000000; border: none; padding: 8px 16px; border-radius: 5px;"
            )
          )
        ) # closes box
      ), # closes column(width = 3)
      
      # Column 2: Results
      shiny::column(
        width = 6,
        bs4Dash::box(
          title       = "AlloMate Results",
          id          = ns("results_box"),
          status      = "info",
          solidHeader = TRUE,
          width       = 12,
          height      = 750,
          maximizable = TRUE,
          collapsible = TRUE,
          collapsed   = TRUE,
          bs4Dash::tabsetPanel(
            id   = ns("main_tabs"),
            type = "tabs",
            shiny::tabPanel(
              "Kinship and EBV",
              shiny::br(),
              shiny::div(
                class = "kinship-extra",
                shiny::verbatimTextOutput(ns("message1")),
                shiny::uiOutput(ns("candidate_ebv_status")),
                shiny::uiOutput(ns("ebv_upload_prompt")),
                shiny::uiOutput(ns("message2"))
              ),
              shiny::div(
                class = "kinship-quantiles",
                shiny::h5("Kinship & EBV Quantiles"),
                DT::DTOutput(ns("quadrants_table"))
              ),
              shiny::div(
                class = "kinship-extra",
                DT::DTOutput(ns("matrix"))
              ),
              style = "overflow-y: auto; height: 640px;"
            ),
            shiny::tabPanel(
              "OCS",
              shiny::br(),
              shiny::p(
                class = "ocs-maximize-tip",
                shiny::icon("expand"),
                " Tip: maximize this panel (top-right icon) to view OCS and Mate Allocation side by side.",
                style = "color: #6c757d; font-size: 12px; margin: 0 0 8px;"
              ),
              shiny::div(
                id    = "ocs_container",
                style = "position: relative;",
                shiny::div(
                  id    = "ocs_loading",
                  style = "display: none; position: absolute; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(255,255,255,0.8); z-index: 9999;",
                  shiny::div(
                    style = "position: absolute; top: 30%; left: 50%; transform: translate(-50%, -50%);",
                    shiny::div(class = "ocs-spinner")
                  )
                ),
                shiny::h5("Optimal Contributions"),
                DT::DTOutput(ns("ocs_candidate_table"))
              ),
              style = "overflow-y: auto; height: 640px;"
            ),
            shiny::tabPanel(
              "Mate Allocation",
              shiny::br(),
              shiny::h5("Mating Plan"),
              DT::DTOutput(ns("ocs_mating_table")),
              shiny::uiOutput(ns("ocs_solver_note")),
              style = "overflow-y: auto; height: 640px;"
            )
          )
        ),
        bs4Dash::box(
          title       = "Instructions",
          id          = ns("instructions_box"),
          status      = "info",
          solidHeader = FALSE,
          width       = 12,
          collapsible = TRUE,
          collapsed   = FALSE,
          shiny::HTML('
            <ul>
              <li>This tool performs mate selection and optimum contribution selection (OCS) for breeding programs.</li>
              <li><strong>Step 1:</strong> Upload your <strong>candidate list</strong> (.csv or .txt) with columns: <code>id</code>, <code>sex</code>.</li>
              <li><strong>Step 2:</strong> Upload your <strong>pedigree file</strong> (.txt) with columns: <code>id</code>, <code>male_parent</code>, <code>female_parent</code> — or switch the relationship matrix source to upload a precomputed A, G, or H matrix (e.g. built in the <strong>Matrix Builder</strong> tab).</li>
              <li><strong>Step 3:</strong> Optionally adjust the <strong>kinship threshold</strong> to restrict inbred crosses.</li>
              <li><strong>Step 4:</strong> Upload <strong>trait EBV files</strong> and assign weights. Weights must sum to 1.</li>
              <li><strong>Step 5:</strong> Configure OCS parameters and click <strong>Run OCS</strong>.</li>
              <li>Results are shown in the <strong>Kinship and EBV</strong>, <strong>OCS</strong>, and <strong>Mate Allocation</strong> tabs. Maximize the results panel for a dashboard view: the Kinship/EBV quantile summary as a full-width header with the OCS and Mate Allocation tables side by side beneath it.</li>
              <li>Use <strong>Download Results</strong> to download a zip of all output tables.</li>
            </ul>
          ')
        )
      ), # closes column(width = 6)
      
      # Column 3: Status + File Status + Guide
      shiny::column(
        width = 3,
        
        # Status / progress
        bs4Dash::box(
          title       = "Status",
          width       = 12,
          collapsible = TRUE,
          status      = "info",
          shinyWidgets::progressBar(
            id          = ns("pb_allomate"),
            value       = 0,
            status      = "info",
            display_pct = TRUE,
            striped     = TRUE,
            title       = " "
          )
        ),
        
        # Getting started / step guide
        bs4Dash::box(
          title       = "Getting Started",
          width       = 12,
          collapsible = TRUE,
          collapsed   = FALSE,
          status      = "info",
          solidHeader = FALSE,
          shiny::uiOutput(ns("dynamic_guide"))
        ),
        
        # File status
        bs4Dash::box(
          title       = "File Status",
          width       = 12,
          collapsible = TRUE,
          collapsed   = FALSE,
          status      = "info",
          solidHeader = FALSE,
          shiny::htmlOutput(ns("file_status_display"))
        )
      ) # closes column(width = 3)
    ) # closes fluidRow
  )   # closes tagList
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
    missing_id_data <- shiny::reactiveValues(
      candidates = character(),
      ebvs       = character()
    )
    
    #### Reactive values ####
    trait_counter             <- shiny::reactiveVal(1)
    candidate_status          <- shiny::reactiveVal(list(ok = FALSE, error = NULL))
    ocs_results_reactive      <- shiny::reactiveVal()
    ebv_results_reactive      <- shiny::reactiveVal()
    error_message             <- shiny::reactiveVal("")
    pedigree_validation_stats <- shiny::reactiveVal(NULL)
    ebv_data <- shiny::reactive({ process_ebvs(trait_counter(), input) })

    #### Package status ####
    output$package_status_text <- shiny::renderText({ generate_package_status() })
    optisel_available      <- requireNamespace("optiSel", quietly = TRUE)
    ocs_checkboxes_enabled <- optisel_available
    output$ocs_checkbox_mode <- shiny::renderText({
      if (ocs_checkboxes_enabled) "1" else ""
    })
    shiny::outputOptions(output, "ocs_checkbox_mode", suspendWhenHidden = FALSE)
    if (optisel_available) {
      options(allomate.force_greedy_mating = FALSE, allomate.force_qp_greedy = FALSE)
    }
    
    #### Candidates ####
    candidates_data <- shiny::reactive({
      shiny::req(input$candidate_file)
      tryCatch({
        res <- read_candidates(input$candidate_file)
        candidate_status(list(ok = TRUE, error = NULL))
        current_err <- error_message()
        if (current_err != "" && grepl("^Error processing candidates", current_err)) error_message("")
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_allomate",
          value = 20, status = "info", title = "Candidates loaded..."
        )
        res
      }, error = function(e) {
        candidate_status(list(ok = FALSE, error = e$message))
        error_message(paste0("Error processing candidates: ", e$message))
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_allomate",
          value = 20, status = "danger", title = "Failed to load candidates"
        )
        shiny::validate(shiny::need(FALSE, e$message))
        NULL
      })
    })
    
    #### Pedigree / relationship matrix ####
    pedigree_data <- shiny::reactiveVal(NULL)
    kinship_matrix_reactive <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      list(input$matrix_source, input$pedigree_file, input$relationship_matrix_file, input$matrix_is_kinship),
      {
        shiny::req(candidates_data())
        source_type <- input$matrix_source %||% "pedigree"
        if (identical(source_type, "pedigree")) {
          shiny::req(input$pedigree_file)
        } else {
          shiny::req(input$relationship_matrix_file)
        }
        males   <- candidates_data()$males
        females <- candidates_data()$females
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_allomate",
          value = 40, status = "info",
          title = if (identical(source_type, "pedigree")) "Processing pedigree..." else "Processing uploaded matrix..."
        )
        tryCatch({
          resolved <- resolve_kinship_input(
            source            = source_type,
            pedigree_file     = input$pedigree_file,
            matrix_file       = input$relationship_matrix_file,
            matrix_is_kinship = input$matrix_is_kinship
          )
          kinship_matrix_reactive(resolved$kinship_matrix)

          candidate_ids         <- candidates_data()$candidates$id
          available_ids         <- rownames(resolved$kinship_matrix)
          missing_candidate_ids <- setdiff(candidate_ids, available_ids)
          missing_candidates    <- length(missing_candidate_ids)
          missing_male_ids      <- intersect(missing_candidate_ids, males)
          missing_female_ids    <- intersect(missing_candidate_ids, females)
          remaining_missing_ids <- setdiff(missing_candidate_ids, c(missing_male_ids, missing_female_ids))

          stats <- resolved$stats
          if (!is.null(stats)) {
            stats$missing_candidates    <- missing_candidates
            stats$missing_candidate_ids <- missing_candidate_ids
          }

          kinship_res <- summarize_kinship_matrix(resolved$kinship_matrix, males, females)
          output$quadrants_table <- DT::renderDT({
            DT::datatable(kinship_res$quads,
                          options = list(ordering = FALSE, dom = "t"), rownames = TRUE) %>%
              DT::formatStyle(colnames(kinship_res$quads),
                              styleEqual(kinship_res$quads[1, ],
                                         c("lightgreen", "yellow", "orange", "coral")))
          })
          # Always compute so it can populate the maximized dashboard header even
          # if the Kinship and EBV tab has not been opened.
          shiny::outputOptions(output, "quadrants_table", suspendWhenHidden = FALSE)
          pedigree_data(list(results = kinship_res$results, quads = kinship_res$quads))
          pedigree_validation_stats(stats)
          error_message("")
          format_missing_msg <- function(ids, label) {
            if (length(ids) == 0) return(NULL)
            ids_str <- format_id_list(ids)
            plural  <- if (length(ids) == 1) "" else "s"
            noun    <- if (identical(source_type, "pedigree")) "pedigree" else "uploaded matrix"
            if (ids_str != "") {
              paste0(length(ids), " ", label, plural,
                     " missing from ", noun, " and not visualized (IDs: ", ids_str, ").")
            } else {
              paste0(length(ids), " ", label, plural,
                     " missing from ", noun, " and not visualized.")
            }
          }
          kinship_mismatch_msgs <- unlist(Filter(Negate(is.null), list(
            format_missing_msg(missing_male_ids,   "male candidate"),
            format_missing_msg(missing_female_ids, "female candidate"),
            format_missing_msg(remaining_missing_ids, "candidate")
          )))
          kinship_status <- if (length(kinship_mismatch_msgs) == 0) {
            "Kinship matrix generated successfully."
          } else {
            paste("Kinship matrix generated with warnings.",
                  paste(kinship_mismatch_msgs, collapse = " "))
          }
          output$message1 <- shiny::renderText(kinship_status)
          shinyWidgets::updateProgressBar(
            session = session, id = "pb_allomate",
            value = 60, status = "info", title = "Kinship matrix ready. Waiting for EBVs..."
          )
        }, error = function(e) {
          error_message(paste0("Error processing pedigree: ", e$message))
          pedigree_validation_stats(NULL)
          kinship_matrix_reactive(NULL)
          fallback_msg <- if (identical(source_type, "pedigree")) {
            "Make sure your pedigree file has columns id, male_parent, female_parent and is clean and valid."
          } else {
            "Make sure your matrix file is a square CSV with individual IDs as both row and column names."
          }
          output$message1 <- shiny::renderText(
            paste0("Error processing pedigree: ", fallback_msg, "\n",
                   "Original error: ", e$message)
          )
          shinyWidgets::updateProgressBar(
            session = session, id = "pb_allomate",
            value = 40, status = "danger", title = "Failed to process pedigree"
          )
        })
      },
      ignoreInit = TRUE
    )
    
    shiny::observe({
      if (is.null(ebv_data())) {
        output$message2             <- shiny::renderUI(NULL)
        output$candidate_ebv_status <- shiny::renderUI(NULL)
      }
    })
    
    output$ebv_upload_prompt <- shiny::renderUI({
      if (!is.null(pedigree_data()) && is.null(ebv_data())) {
        shiny::tags$pre(class = "shiny-text-output", "Please upload trait EBVs.")
      } else {
        NULL
      }
    })
    
    #### Trait counter ####
    shiny::observeEvent(input$add_trait,    { trait_counter(trait_counter() + 1) })
    shiny::observeEvent(input$remove_trait, { if (trait_counter() > 1) trait_counter(trait_counter() - 1) })
    output$trait_inputs <- shiny::renderUI({ create_trait_inputs(trait_counter(), ns = ns) })
    output$ocs_trait_inputs <- shiny::renderUI({
      shiny::req(input$ocs_trait_counter)
      create_ocs_trait_inputs(input$ocs_trait_counter, ns = ns)
    })
    
    #### EBV observe ####
    shiny::observe({
      shiny::req(candidates_data())
      ebv_res <- ebv_data()
      shiny::req(ebv_res)
      joint_ebvs   <- ebv_res$joint_ebvs
      rel_weights  <- ebv_res$rel_weights
      weight_total <- ebv_res$weight_total
      ebv_cols     <- paste0("EBV.", seq_along(rel_weights))
      joint_ebvs[ebv_cols] <- lapply(joint_ebvs[ebv_cols], as.numeric)
      joint_ebvs$index_val <- as.vector(as.matrix(joint_ebvs[ebv_cols]) %*% rel_weights)
      cands   <- candidates_data()$candidates
      males   <- candidates_data()$males
      females <- candidates_data()$females
      ebv_ids             <- unique(joint_ebvs$ID)
      ebv_only_ids        <- setdiff(ebv_ids, cands$id)
      joint_ebvs_filtered <- joint_ebvs %>% dplyr::filter(ID %in% cands$id)
      cand_ebv <- dplyr::left_join(cands, joint_ebvs_filtered, by = c("id" = "ID")) %>%
        dplyr::select(id, sex, index_val)
      candidate_missing_ids <- cand_ebv %>%
        dplyr::filter(is.na(index_val)) %>%
        dplyr::pull(id)
      missing_id_data$candidates <- candidate_missing_ids
      missing_id_data$ebvs       <- ebv_only_ids
      m_ebv <- cand_ebv %>% dplyr::filter(id %in% males,   !is.na(index_val)) %>% dplyr::select(id, index_val)
      f_ebv <- cand_ebv %>% dplyr::filter(id %in% females, !is.na(index_val)) %>% dplyr::select(id, index_val)
      valid_pairs <- nrow(m_ebv) > 0 && nrow(f_ebv) > 0
      if (valid_pairs) {
        ebv_matrix <- outer(m_ebv$index_val, f_ebv$index_val,
                            function(x, y) round((x + y) / 2, 2))
        rownames(ebv_matrix) <- m_ebv$id
        colnames(ebv_matrix) <- f_ebv$id
        ebv_quads <- tibble::tibble(
          Data = "EBV",
          Q25  = stats::quantile(ebv_matrix, 0.25, na.rm = TRUE),
          Q50  = stats::quantile(ebv_matrix, 0.50, na.rm = TRUE),
          Q75  = stats::quantile(ebv_matrix, 0.75, na.rm = TRUE),
          Q100 = stats::quantile(ebv_matrix, 1.00, na.rm = TRUE)
        ) %>% tibble::column_to_rownames("Data")
        ebv_results <- tibble::as_tibble(ebv_matrix, rownames = "Male") %>%
          tidyr::pivot_longer(-Male, names_to = "Female", values_to = "EBV")
        full_results <- if (!is.null(pedigree_data())) {
          dplyr::left_join(pedigree_data()$results, ebv_results, by = c("Female", "Male"))
        } else {
          dplyr::relocate(dplyr::mutate(ebv_results, Kinship = NA), Kinship, .after = EBV)
        }
      } else {
        ebv_quads <- tibble::tibble(
          Data = "EBV", Q25 = NA_real_, Q50 = NA_real_, Q75 = NA_real_, Q100 = NA_real_
        ) %>% tibble::column_to_rownames("Data")
        full_results <- if (!is.null(pedigree_data())) {
          dplyr::mutate(pedigree_data()$results, EBV = NA_real_)
        } else {
          tibble::tibble(Male = character(), Female = character(),
                         Kinship = numeric(), EBV = numeric())
        }
        ebv_results <- tibble::tibble(Male = character(), Female = character(), EBV = numeric())
      }
      quads_combined <- if (!is.null(pedigree_data())) {
        dplyr::bind_rows(pedigree_data()$quads, ebv_quads)
      } else {
        ebv_quads
      }
      output$quadrants_table <- DT::renderDT({
        DT::datatable(quads_combined, options = list(ordering = FALSE, dom = "t"), rownames = TRUE) %>%
          DT::formatStyle("Q25",  backgroundColor = "coral") %>%
          DT::formatStyle("Q50",  backgroundColor = "orange") %>%
          DT::formatStyle("Q75",  backgroundColor = "yellow") %>%
          DT::formatStyle("Q100", backgroundColor = "lightgreen")
      })
      shiny::outputOptions(output, "quadrants_table", suspendWhenHidden = FALSE)
      filt_results_table <- full_results %>%
        dplyr::filter(EBV > 0, (is.na(Kinship) | Kinship < input$thresh))
      filt_results_matrix <- full_results %>%
        dplyr::mutate(EBV = ifelse(EBV <= 0 | (!is.na(Kinship) & Kinship >= input$thresh), NA, EBV))
      ebv_results_reactive(list(
        filt_results_table  = filt_results_table,
        filt_results_matrix = filt_results_matrix,
        full_results        = full_results,
        ebv_quads           = ebv_quads
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
      shinyWidgets::updateProgressBar(
        session = session, id = "pb_allomate",
        value = 80, status = "info", title = "EBV matrix ready. Run OCS to complete analysis."
      )
      
      #### Candidate-EBV status UI ####
      total_candidates <- dplyr::n_distinct(cands$id)
      total_ebvs       <- length(ebv_ids)
      candidate_match_ui <- if (total_candidates > 0 &&
                                length(candidate_missing_ids) == 0 &&
                                total_ebvs >= total_candidates) {
        shiny::tags$pre(class = "shiny-text-output",
                        "All selection candidates have corresponding EBVs.")
      } else NULL
      output$candidate_ebv_status <- shiny::renderUI(candidate_match_ui)
      status_components <- list()
      if (length(candidate_missing_ids) > 0) {
        display_ids    <- head(candidate_missing_ids, 3)
        extra_count    <- length(candidate_missing_ids) - length(display_ids)
        display_str    <- paste(display_ids, collapse = ", ")
        candidate_text <- paste0("Candidates lack corresponding EBVs: ", display_str)
        if (extra_count > 0) candidate_text <- paste0(candidate_text, " and ", extra_count, " more")
        candidate_text <- paste0(candidate_text, " will be excluded from further analysis, ")
        status_components <- append(status_components, list(
          shiny::tags$p(candidate_text,
                        shiny::downloadLink(ns("download_missing_candidates"), "view here"), ".")
        ))
      }
      if (length(ebv_only_ids) > 0) {
        status_components <- append(status_components, list(
          shiny::tags$p(sprintf("EBVs filtered for matching candidates: %d EBVs removed.",
                                length(ebv_only_ids)))
        ))
      }
      if (!valid_pairs) {
        status_components <- append(status_components, list(
          shiny::tags$p("EBV matrix not visualized because no overlapping candidates with EBVs remain.")
        ))
      }
      if (abs(weight_total - 1) > 1e-6) {
        status_components <- append(status_components, list(
          shiny::tags$p(paste("Warning: Trait weights do not sum to 1 (total =",
                              round(weight_total, 4), ")"))
        ))
      }
      status_body <- if (length(status_components) == 0) {
        shiny::tags$p("EBV matrix generated successfully.")
      } else {
        do.call(shiny::tagList, status_components)
      }
      output$message2 <- shiny::renderUI(
        shiny::tags$div(
          style = "background-color: #f0f0f0; border: 1px solid #d0d0d0; padding: 12px; border-radius: 6px; margin-top: 10px;",
          shiny::tags$h5("Candidate-EBV Mapping Summary", style = "margin-top: 0; font-weight: 600;"),
          status_body
        )
      )
    })
    
    output$download_missing_candidates <- shiny::downloadHandler(
      filename = function() paste0("candidates_missing_ebvs_", Sys.Date(), ".txt"),
      content  = function(file) {
        ids   <- missing_id_data$candidates
        lines <- if (length(ids) == 0) c("Candidates lacking EBVs:", "None.") else c("Candidates lacking EBVs:", ids)
        writeLines(lines, con = file)
      }
    )

    #### Download mate allocation (single xlsx: Mate Allocation / Kinship / OCS) ####
    output$download_mate_allocation <- shiny::downloadHandler(
      filename = function() paste0("AlloMate_mate_allocation-", Sys.Date(), ".xlsx"),
      content  = function(file) {
        shiny::req(ebv_results_reactive(), ocs_results_reactive())

        fmt <- format_ocs_results(ocs_results_reactive())

        sheets <- list(
          "Mate Allocation" = as.data.frame(fmt$mating_table),
          "Kinship"         = as.data.frame(ebv_results_reactive()$filt_results_table),
          "OCS"             = as.data.frame(fmt$candidate_table)
        )

        save_xlsx_with_fallback(file, sheets, active_sheet = "Mate Allocation")
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    #### Download all tables (zip of TSVs) ####
    output$download_all_tsv <- shiny::downloadHandler(
      filename = function() paste0("AlloMate_results-", Sys.Date(), ".zip"),
      content  = function(file) {
        shiny::req(ebv_results_reactive())
        tmp_dir <- tempfile("allomate_export")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

        # README
        readme_text <- c(
          "AlloMate Complete Results Report", "",
          "This TSV collection contains all results from your AlloMate analysis:", "",
          "Files included:",
          "1. README - This overview and explanation",
          "2. Filtered Results - Crosses meeting criteria (positive EBVs, kinship below threshold)",
          "3. EBV Matrix - Complete matrix view with masked values",
          "4. OCS Candidates - Selected candidates from Optimum Contribution Selection",
          "5. Mating Plan - Recommended mating pairs from OCS",
          "6. Parameters - Analysis parameters used", "",
          "Generated on:", as.character(Sys.Date())
        )
        write.table(data.frame(Text = readme_text, stringsAsFactors = FALSE),
                    file.path(tmp_dir, "README.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)

        # Filtered results and EBV matrix
        ebv_results <- ebv_results_reactive()
        if (!is.null(ebv_results)) {
          write.table(as.data.frame(ebv_results$filt_results_table),
                      file.path(tmp_dir, "Filtered_Results.tsv"),
                      sep = "\t", row.names = FALSE, quote = FALSE)

          m_ids       <- unique(ebv_results$full_results$Male)
          f_ids       <- unique(ebv_results$full_results$Female)
          mat_for_csv <- matrix(NA_real_, nrow = length(m_ids), ncol = length(f_ids),
                                dimnames = list(m_ids, f_ids))
          for (i in seq_len(nrow(ebv_results$filt_results_matrix))) {
            m <- ebv_results$filt_results_matrix$Male[i]
            f <- ebv_results$filt_results_matrix$Female[i]
            v <- ebv_results$filt_results_matrix$EBV[i]
            if (!is.na(m) && !is.na(f) &&
                m %in% rownames(mat_for_csv) && f %in% colnames(mat_for_csv))
              mat_for_csv[m, f] <- v
          }
          ebv_matrix_df <- data.frame(Male = rownames(mat_for_csv), mat_for_csv,
                                      check.names = FALSE, stringsAsFactors = FALSE)
          write.table(ebv_matrix_df, file.path(tmp_dir, "EBV_Matrix.tsv"),
                      sep = "\t", row.names = FALSE, quote = FALSE, na = "")
        }

        # OCS results
        if (!is.null(ocs_results_reactive())) {
          formatted_results <- tryCatch(format_ocs_results(ocs_results_reactive()), error = function(e) NULL)
          if (!is.null(formatted_results)) {
            write.table(as.data.frame(formatted_results$candidate_table),
                        file.path(tmp_dir, "OCS_Candidates.tsv"),
                        sep = "\t", row.names = FALSE, quote = FALSE)
            write.table(as.data.frame(formatted_results$mating_table),
                        file.path(tmp_dir, "Mating_Plan.tsv"),
                        sep = "\t", row.names = FALSE, quote = FALSE)
          }
        }

        # Parameters
        write.table(
          data.frame(
            Parameter = c("Analysis Date", "Kinship Threshold", "Desired Inbreeding Rate", "Number of Offspring"),
            Value     = c(as.character(Sys.Date()), input$thresh, input$inbreeding_rate, input$num_offspring)
          ),
          file.path(tmp_dir, "Parameters.tsv"), sep = "\t", row.names = FALSE, quote = FALSE
        )

        zip_files <- list.files(tmp_dir)
        zip::zip(zipfile = file, files = zip_files, root = tmp_dir)
      },
      contentType = "application/zip"
    )

    #### Pedigree status display ####
    output$pedigree_status_display <- shiny::renderUI({
      render_pedigree_status_html(pedigree_validation_stats())
    })
    
    #### OCS server logic ####
    shiny::observeEvent(input$force_greedy_mating, {
      if (!ocs_checkboxes_enabled) return(NULL)
      options(allomate.force_greedy_mating = isTRUE(input$force_greedy_mating))
    }, ignoreNULL = FALSE)
    
    shiny::observeEvent(input$force_qp_greedy, {
      if (!ocs_checkboxes_enabled) return(NULL)
      options(allomate.force_qp_greedy = isTRUE(input$force_qp_greedy))
    }, ignoreNULL = FALSE)
    
    shiny::observeEvent(input$run_ocs_btn, {
      shiny::req(input$candidate_file)
      matrix_source_now <- input$matrix_source %||% "pedigree"
      if (identical(matrix_source_now, "pedigree")) {
        shiny::req(input$pedigree_file)
      } else {
        shiny::req(input$relationship_matrix_file)
      }
      # Switch to the results view: expand the Results box, collapse Instructions.
      if (isTRUE(input$results_box$collapsed))
        bs4Dash::updateBox("results_box", action = "toggle", session = session)
      if (isFALSE(input$instructions_box$collapsed))
        bs4Dash::updateBox("instructions_box", action = "toggle", session = session)
      shinyjs::disable("download_mate_allocation")
      shinyjs::disable("download_all_tsv")
      if (ocs_checkboxes_enabled) {
        options(allomate.force_greedy_mating = isTRUE(input$force_greedy_mating))
        options(allomate.force_qp_greedy     = isTRUE(input$force_qp_greedy))
      } else {
        options(allomate.force_greedy_mating = FALSE, allomate.force_qp_greedy = FALSE)
      }
      shinyjs::show("ocs_loading")
      on.exit(shinyjs::hide("ocs_loading"), add = TRUE)
      shinyWidgets::updateProgressBar(
        session = session, id = "pb_allomate",
        value = 82, status = "info", title = "Reading input files..."
      )
      tryCatch({
        candidates <- candidates_data()$candidates
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_allomate",
          value = 87, status = "info",
          title = if (identical(matrix_source_now, "pedigree")) "Computing kinship matrix..." else "Reading uploaded relationship matrix..."
        )
        resolved <- resolve_kinship_input(
          source            = matrix_source_now,
          pedigree_file     = input$pedigree_file,
          matrix_file       = input$relationship_matrix_file,
          matrix_is_kinship = input$matrix_is_kinship
        )
        kinship_matrix <- resolved$kinship_matrix
        ebv_result <- process_ebvs(trait_counter(), input)
        if (is.null(ebv_result) || abs(ebv_result$weight_total - 1) > 1e-6) {
          shiny::showModal(shiny::modalDialog(
            title = "Invalid Weights", "Weights must sum to 1.", easyClose = TRUE
          ))
          return(NULL)
        }
        joint_ebvs           <- calculate_index(ebv_result$joint_ebvs, ebv_result$rel_weights)
        joint_ebvs_ids       <- unique(joint_ebvs$ID)
        ebv_only_ids         <- setdiff(joint_ebvs_ids, candidates$id)
        joint_ebvs_filtered  <- joint_ebvs %>% dplyr::filter(ID %in% candidates$id)
        candidates_joined    <- dplyr::left_join(candidates, joint_ebvs_filtered, by = c("id" = "ID"))
        missing_candidate_ids <- candidates_joined %>% dplyr::filter(is.na(index_val)) %>% dplyr::pull(id)
        if (length(missing_candidate_ids) > 0) {
          display_ids <- head(missing_candidate_ids, 3)
          extra_count <- length(missing_candidate_ids) - length(display_ids)
          warning_msg <- paste0("OCS: Removed ", length(missing_candidate_ids),
                                " candidate(s) lacking EBVs (IDs: ",
                                paste(display_ids, collapse = ", "))
          if (extra_count > 0) warning_msg <- paste0(warning_msg, " and ", extra_count, " more")
          shiny::showNotification(paste0(warning_msg, ")."), type = "warning", duration = 8)
        }
        if (length(ebv_only_ids) > 0) {
          shiny::showNotification("OCS: IDs with EBVs but not in the candidate list were ignored.",
                                  type = "warning", duration = 8)
        }
        candidates_filtered <- candidates_joined %>% dplyr::filter(!is.na(index_val))
        if (nrow(candidates_filtered) == 0) {
          stop("No candidates with EBVs available for OCS. Please ensure at least one candidate of each sex has an EBV.")
        }
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_allomate",
          value = 93, status = "info", title = "Running OCS optimisation..."
        )
        results <- run_ocs(
          candidates_df           = candidates_filtered,
          kinship_matrix          = kinship_matrix,
          ebv_index               = candidates_filtered$index_val,
          desired_inbreeding_rate = input$inbreeding_rate,
          num_offspring           = input$num_offspring,
          per_pair_kinship_limit  = if (ocs_checkboxes_enabled && isTRUE(input$enforce_pair_kinship))
            input$inbreeding_rate else NULL
        )
        mating_info <- attr(results$Mating, "info")
        if (!is.null(mating_info) && is.character(mating_info) &&
            grepl("Greedy allocation", mating_info, fixed = TRUE)) {
          shiny::showNotification(
            paste0("OCS fallback: ", mating_info,
                   ". lpSolve solution could not be used, so a greedy mating plan was generated."),
            type = "warning", duration = 10
          )
        }
        ocs_results_reactive(results)
        error_message("")
        shinyjs::enable("download_mate_allocation")
        shinyjs::enable("download_all_tsv")
        formatted_results <- format_ocs_results(results)
        output$ocs_candidate_table <- DT::renderDT({
          DT::datatable(formatted_results$candidate_table,
                        options = list(pageLength = 20, autoWidth = TRUE), rownames = FALSE)
        })
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_allomate",
          value = 100, status = "success", title = "Finished"
        )
        shiny::updateTabsetPanel(session, "main_tabs", selected = "Kinship and EBV")
      }, error = function(e) {
        error_message(paste0("Error running OCS: ", e$message))
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          paste("Error running OCS:", e$message),
          easyClose = TRUE
        ))
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_allomate",
          value = 100, status = "danger", title = "Failed"
        )
      })
    })
    
    output$ocs_mating_table <- DT::renderDT({
      shiny::req(ocs_results_reactive())
      formatted_results <- format_ocs_results(ocs_results_reactive())
      mating_tbl <- formatted_results$mating_table
      if (ocs_checkboxes_enabled && isTRUE(input$enforce_pair_kinship)) {
        mating_tbl <- mating_tbl %>%
          dplyr::filter(is.na(Kinship) | Kinship < input$inbreeding_rate)
      }
      DT::datatable(mating_tbl, options = list(pageLength = 20, autoWidth = TRUE), rownames = FALSE)
    })
    
    output$ocs_solver_note <- shiny::renderUI({
      shiny::req(ocs_results_reactive())
      info <- format_ocs_results(ocs_results_reactive())$summary_stats$mating_info
      if (is.null(info) || is.na(info) || info == "") return(NULL)
      shiny::div(
        style = "margin: 10px 0; padding: 10px; background-color: #fff8e1; border-left: 4px solid #ffb300; font-size: 13px;",
        shiny::tags$strong("Solver note: "), info
      )
    })

    #### Getting Started (dynamic step guide) ####
    output$dynamic_guide <- shiny::renderUI({
      current_error <- error_message()
      if (current_error != "") {
        return(shiny::HTML(paste0(
          "<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px; border-radius: 4px; margin-bottom: 10px;'>",
          "<p style='color: #721c24; margin: 0;'><strong>Error occurred:</strong></p>",
          "<p style='color: #721c24; margin: 5px 0; font-family: monospace; font-size: 11px;'>", current_error, "</p>",
          "<p style='color: #721c24; margin: 5px 0 0 0; font-size: 12px;'>Need help? Click the Help button for detailed documentation.</p>",
          "</div>"
        )))
      }
      has_candidates  <- !is.null(input$candidate_file)
      has_pedigree    <- if (identical(input$matrix_source %||% "pedigree", "upload")) {
        !is.null(input$relationship_matrix_file)
      } else {
        !is.null(input$pedigree_file)
      }
      has_traits      <- trait_counter() > 0 &&
        any(sapply(seq_len(trait_counter()), function(i) !is.null(input[[paste0("trait_file_", i)]])))
      has_ocs_results <- !is.null(ocs_results_reactive())
      pedigree_error       <- current_error != "" && grepl("error|pedigree|kinship", current_error, ignore.case = TRUE)
      trait_error          <- current_error != "" && grepl("error|ebv|trait|weight",  current_error, ignore.case = TRUE)
      ocs_error            <- current_error != "" && grepl("error|ocs",               current_error, ignore.case = TRUE)
      cs                   <- candidate_status()
      candidate_ready      <- isTRUE(cs$ok)
      candidate_error_flag <- !is.null(cs$error)
      get_step_label <- function(has_item, error_flag) {
        if (error_flag) {
          "<span style='color: #dc3545; font-weight: bold;'>[Error]</span>"
        } else if (has_item) {
          "<span style='color: #28a745; font-weight: bold;'>[Done]</span>"
        } else {
          "<span style='color: #6c757d;'>[Pending]</span>"
        }
      }
      steps <- c(
        sprintf("<p>%s <strong>Step 1:</strong> Upload your candidate list to begin the analysis</p>",
                get_step_label(candidate_ready, candidate_error_flag)),
        sprintf("<p>%s <strong>Step 2:</strong> Upload your pedigree file (or a precomputed A/G/H matrix) for kinship calculations</p>",
                get_step_label(has_pedigree, pedigree_error)),
        sprintf("<p>%s <strong>Step 3:</strong> Set your kinship threshold (optional)</p>",
                get_step_label(has_pedigree, FALSE)),
        sprintf("<p>%s <strong>Step 4:</strong> Add trait files and weights for breeding value analysis</p>",
                get_step_label(has_traits, trait_error)),
        sprintf("<p>%s <strong>Step 5:</strong> Configure OCS parameters and run analysis</p>",
                get_step_label(has_ocs_results, ocs_error))
      )
      if (has_ocs_results) {
        steps <- c(
          steps,
          "<p><strong>Analysis Complete!</strong></p>",
          "<p style='color: #28a745; font-weight: bold;'>Results are ready for review and export.</p>"
        )
      }
      shiny::HTML(paste(steps, collapse = ""))
    })

    #### File status display ####
    output$file_status_display <- shiny::renderUI({
      has_candidates <- !is.null(input$candidate_file)
      has_pedigree   <- if (identical(input$matrix_source %||% "pedigree", "upload")) {
        !is.null(input$relationship_matrix_file)
      } else {
        !is.null(input$pedigree_file)
      }
      has_ebv        <- !is.null(ebv_results_reactive())
      has_ocs        <- !is.null(ocs_results_reactive())
      current_error  <- error_message()
      has_error      <- current_error != ""
      cs             <- candidate_status()
      candidate_ready      <- isTRUE(cs$ok)
      candidate_error_flag <- !is.null(cs$error)
      pedigree_error <- has_error && grepl("pedigree|kinship", current_error, ignore.case = TRUE)
      ebv_error      <- has_error && grepl("ebv|trait|weight",  current_error, ignore.case = TRUE)
      ocs_error      <- has_error && grepl("ocs",               current_error, ignore.case = TRUE)
      get_status <- function(has_data, uploaded, has_specific_error,
                             ready_text = "Ready", pending_text = "Not uploaded", error_text = "Error") {
        if (has_specific_error) {
          list(label = "[Error]",   color = "#dc3545")
        } else if (has_data) {
          list(label = "[Ready]",   color = "#28a745")
        } else if (uploaded) {
          list(label = "[Processing...]", color = "#ffc107")
        } else {
          list(label = paste0("[", pending_text, "]"), color = "#6c757d")
        }
      }
      cand_ui <- if (candidate_error_flag) {
        list(label = "[Error]", color = "#dc3545")
      } else if (candidate_ready) {
        list(label = "[Ready]", color = "#28a745")
      } else if (has_candidates) {
        list(label = "[Processing...]", color = "#ffc107")
      } else {
        list(label = "[Not uploaded]", color = "#6c757d")
      }
      ped_ui <- get_status(has_pedigree, has_pedigree, pedigree_error, "Ready", "Not uploaded")
      ebv_ui <- get_status(has_ebv, has_candidates && has_pedigree, ebv_error, "Ready", "Pending")
      ocs_ui <- get_status(has_ocs, FALSE, ocs_error, "Ready", "Not run")
      make_line <- function(label, s) {
        paste0(
          "<strong>", label, ":</strong> ",
          "<span style='color:", s$color, "; font-weight: bold;'>", s$label, "</span>"
        )
      }
      status_lines <- c(
        make_line("Candidate List", cand_ui),
        make_line("Pedigree Data",  ped_ui),
        make_line("EBV Matrix",     ebv_ui),
        make_line("OCS Results",    ocs_ui)
      )
      all_ready <- candidate_ready && has_pedigree && has_ebv
      download_status <- if (has_error) {
        "<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px; margin-top: 10px; border-radius: 3px;'>
        <p style='color: #721c24; margin: 0; font-size: 12px;'><strong>Error detected.</strong> Check the startup guide above for details.</p>
        </div>"
      } else if (all_ready) {
        "<div style='background-color: #d4edda; border: 1px solid #c3e6cb; padding: 8px; margin-top: 10px; border-radius: 3px;'>
        <p style='color: #155724; margin: 0; font-size: 12px;'><strong>Download ready.</strong> All core data is available.</p>
        </div>"
      } else {
        "<div style='background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 8px; margin-top: 10px; border-radius: 3px;'>
        <p style='color: #6c757d; margin: 0; font-size: 12px;'>Upload required files to enable download.</p>
        </div>"
      }
      shiny::HTML(paste0(
        "<div style='font-size: 12px; line-height: 1.8;'>",
        paste(status_lines, collapse = "<br>"),
        "</div>",
        download_status
      ))
    })

    #### Help button ####
    shiny::observeEvent(input$help_btn, {
      shiny::showModal(
        shiny::modalDialog(
          title     = shiny::tagList(shiny::icon("circle-question"), " AlloMate — Help"),
          size      = "l",
          easyClose = TRUE,
          footer    = shiny::modalButton("Close"),
          help_content_allomate(id_prefix = "modal")
        )
      )
    })
  })
}
