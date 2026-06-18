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
          shiny::fileInput(ns("pedigree_file"), "Upload pedigree file", accept = ".txt"),
          shiny::uiOutput(ns("pedigree_status_display")),
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
            "Download all results in a single zip file:",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 10px;"
          ),
          shinyjs::disabled(
            downloadButton(ns("download_all_results"), "Download Results")
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
          status      = "info",
          solidHeader = FALSE,
          width       = 12,
          height      = 750,
          maximizable = TRUE,
          bs4Dash::tabsetPanel(
            id   = ns("main_tabs"),
            type = "tabs",
            shiny::tabPanel(
              "Instructions",
              shiny::fluidRow(
                shiny::column(12, shiny::wellPanel(shiny::HTML('
                  <ul>
                    <li>This tool performs mate selection and optimum contribution selection (OCS) for breeding programs.</li>
                    <li><strong>Step 1:</strong> Upload your <strong>candidate list</strong> (.csv or .txt) with columns: <code>id</code>, <code>sex</code>.</li>
                    <li><strong>Step 2:</strong> Upload your <strong>pedigree file</strong> (.txt) with columns: <code>id</code>, <code>male_parent</code>, <code>female_parent</code>.</li>
                    <li><strong>Step 3:</strong> Optionally adjust the <strong>kinship threshold</strong> to restrict inbred crosses.</li>
                    <li><strong>Step 4:</strong> Upload <strong>trait EBV files</strong> and assign weights. Weights must sum to 1.</li>
                    <li><strong>Step 5:</strong> Configure OCS parameters and click <strong>Run OCS</strong>.</li>
                    <li>Results are shown in the <strong>Kinship and EBV</strong> and <strong>Optimum Contribution Selection</strong> tabs.</li>
                    <li>Use <strong>Download Results</strong> to download a zip of all output tables.</li>
                  </ul>
                ')))
              ),
              style = "overflow-y: auto; height: 640px;"
            ),
            shiny::tabPanel(
              "Kinship and EBV",
              shiny::br(),
              shiny::verbatimTextOutput(ns("message1")),
              shiny::uiOutput(ns("candidate_ebv_status")),
              shiny::uiOutput(ns("ebv_upload_prompt")),
              shiny::uiOutput(ns("message2")),
              DT::DTOutput(ns("quadrants_table")),
              DT::DTOutput(ns("matrix")),
              style = "overflow-y: auto; height: 640px;"
            ),
            shiny::tabPanel(
              "Optimum Contribution Selection",
              shiny::br(),
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
                DT::DTOutput(ns("ocs_candidate_table")),
                shiny::uiOutput(ns("ocs_solver_note")),
                shiny::br(),
                DT::DTOutput(ns("ocs_mating_table"))
              ),
              style = "overflow-y: auto; height: 640px;"
            )
          )
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
          solidHeader = TRUE,
          shiny::uiOutput(ns("dynamic_guide"))
        ),
        
        # File status
        bs4Dash::box(
          title       = "File Status",
          width       = 12,
          collapsible = TRUE,
          collapsed   = FALSE,
          status      = "info",
          solidHeader = TRUE,
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
    
    #### Pedigree ####
    pedigree_data <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$pedigree_file, {
      shiny::req(candidates_data())
      males   <- candidates_data()$males
      females <- candidates_data()$females
      shinyWidgets::updateProgressBar(
        session = session, id = "pb_allomate",
        value = 40, status = "info", title = "Processing pedigree..."
      )
      tryCatch({
        raw_ped <- read_uploaded_table(input$pedigree_file, file_type = "PEDIGREE")
        names(raw_ped) <- tolower(names(raw_ped))
        required_cols <- c("id", "male_parent", "female_parent")
        missing_cols  <- setdiff(required_cols, colnames(raw_ped))
        if (length(missing_cols) > 0) {
          stop(paste0(
            "Missing required column(s): ",
            paste(missing_cols, collapse = ", "),
            ". File must contain: id, male_parent, female_parent."
          ))
        }
        cleaned_ped <- clean_pedigree(raw_ped, return_stats = TRUE)
        final_ped   <- cleaned_ped$pedigree
        candidate_ids         <- candidates_data()$candidates$id
        pedigree_ids          <- as.character(raw_ped$id)
        missing_candidate_ids <- setdiff(candidate_ids, pedigree_ids)
        missing_candidates    <- length(missing_candidate_ids)
        missing_male_ids      <- intersect(missing_candidate_ids, males)
        missing_female_ids    <- intersect(missing_candidate_ids, females)
        remaining_missing_ids <- setdiff(missing_candidate_ids, c(missing_male_ids, missing_female_ids))
        cleaned_ped$stats$missing_candidates    <- missing_candidates
        cleaned_ped$stats$missing_candidate_ids <- missing_candidate_ids
        kinship_res <- compute_kinship_matrix(final_ped, males, females)
        output$quadrants_table <- DT::renderDT({
          DT::datatable(kinship_res$quads,
                        options = list(ordering = FALSE, dom = "t"), rownames = TRUE) %>%
            DT::formatStyle(colnames(kinship_res$quads),
                            styleEqual(kinship_res$quads[1, ],
                                       c("lightgreen", "yellow", "orange", "coral")))
        })
        pedigree_data(list(results = kinship_res$results, quads = kinship_res$quads))
        pedigree_validation_stats(cleaned_ped$stats)
        error_message("")
        format_missing_msg <- function(ids, label) {
          if (length(ids) == 0) return(NULL)
          ids_str <- format_id_list(ids)
          plural  <- if (length(ids) == 1) "" else "s"
          if (ids_str != "") {
            paste0(length(ids), " ", label, plural,
                   " missing from pedigree and not visualized (IDs: ", ids_str, ").")
          } else {
            paste0(length(ids), " ", label, plural,
                   " missing from pedigree and not visualized.")
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
        output$message1 <- shiny::renderText(
          paste0("Error processing pedigree: Make sure your pedigree file has columns id, male_parent, female_parent and is clean and valid.\n",
                 "Original error: ", e$message)
        )
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_allomate",
          value = 40, status = "danger", title = "Failed to process pedigree"
        )
      })
    })
    
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
    
    #### Pedigree status display ####
    output$pedigree_status_display <- shiny::renderUI({
      stats <- pedigree_validation_stats()
      if (is.null(stats)) return(NULL)
      get_count    <- function(val) if (is.null(val) || is.na(val)) 0L else as.integer(val)
      format_count <- function(val) format(get_count(val), big.mark = ",", scientific = FALSE)
      records        <- format_count(stats$records_loaded)
      unknown_count  <- get_count(stats$unknown_parent_count)
      circular_count <- get_count(stats$circular_reference_count)
      missing_count  <- get_count(stats$missing_candidates)
      duplicates     <- get_count(stats$duplicates_removed)
      green_box <- paste0(
        "<div style='background-color: #d4edda; border: 1px solid #c3e6cb; padding: 8px;",
        " border-radius: 3px; margin-top: 10px; font-size: 12px;'>",
        records, " records loaded</div>"
      )
      yellow_warnings <- c()
      if (unknown_count > 0) yellow_warnings <- c(yellow_warnings,
                                                  paste0("<p style='margin:", if (length(yellow_warnings) == 0) "0" else "4px 0 0", ";'>",
                                                         format_count(stats$unknown_parent_count),
                                                         " individuals with unknown parent(s) (treated as founders)</p>"))
      if (circular_count > 0) yellow_warnings <- c(yellow_warnings,
                                                   paste0("<p style='margin:", if (length(yellow_warnings) == 0) "0" else "4px 0 0", ";'>",
                                                          format_count(stats$circular_reference_count),
                                                          " circular references detected and broken at earliest generation</p>"))
      if (missing_count > 0) yellow_warnings <- c(yellow_warnings,
                                                  paste0("<p style='margin:", if (length(yellow_warnings) == 0) "0" else "4px 0 0", ";'>",
                                                         format_count(stats$missing_candidates),
                                                         " selection candidates missing from pedigree</p>"))
      yellow_box <- if (length(yellow_warnings) > 0) {
        paste0(
          "<div style='background-color: #fff3cd; border: 1px solid #ffeeba; padding: 8px;",
          " border-radius: 3px; margin-top: 6px; font-size: 12px;'>",
          paste(yellow_warnings, collapse = ""), "</div>"
        )
      } else ""
      red_box <- if (duplicates > 0) {
        paste0(
          "<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px;",
          " border-radius: 3px; margin-top: 6px; font-size: 12px;'>",
          format(duplicates, big.mark = ",", scientific = FALSE), " duplicates removed</div>"
        )
      } else ""
      shiny::HTML(paste0(green_box, yellow_box, red_box))
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
      shiny::req(input$pedigree_file, input$candidate_file)
      shinyjs::disable("download_all_results")
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
        ped_data <- read_uploaded_table(input$pedigree_file, file_type = "PEDIGREE")
        names(ped_data) <- tolower(names(ped_data))
        candidates <- candidates_data()$candidates
        required_cols <- c("id", "male_parent", "female_parent")
        missing_cols  <- setdiff(required_cols, colnames(ped_data))
        if (length(missing_cols) > 0) {
          stop(paste0(
            "Missing required column(s): ",
            paste(missing_cols, collapse = ", "),
            ". File must contain: id, male_parent, female_parent."
          ))
        }
        final_ped  <- clean_pedigree(ped_data)
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_allomate",
          value = 87, status = "info", title = "Computing kinship matrix..."
        )
        kinship_matrix <- if (requireNamespace("kinship2", quietly = TRUE)) {
          kinship2::kinship(final_ped)
        } else {
          fallback_kinship(final_ped)
        }
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
        shinyjs::enable("download_all_results")
        formatted_results <- format_ocs_results(results)
        output$ocs_candidate_table <- DT::renderDT({
          DT::datatable(formatted_results$candidate_table,
                        options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
        })
        shinyWidgets::updateProgressBar(
          session = session, id = "pb_allomate",
          value = 100, status = "success", title = "Finished"
        )
        shiny::updateTabsetPanel(session, "main_tabs", selected = "Optimum Contribution Selection")
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
      DT::datatable(mating_tbl, options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
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
  })
}
