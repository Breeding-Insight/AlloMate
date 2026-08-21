#' Matrix Builder module
#'
#' Lets users upload a pedigree and/or genotype (marker dosage) file and
#' build a pedigree-based (A), genomic (G), or combined (H) relationship
#' matrix via AGHmatrix. The resulting matrix can be downloaded as a CSV
#' and re-uploaded in the Mate Allocation module's "Upload precomputed
#' matrix (A/G/H)" option.
#'
#' @param id Module id
#'
#' @noRd
mod_matrix_builder_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      # Column 1: Inputs
      shiny::column(
        width = 4,
        bs4Dash::box(
          title       = "Build a Relationship Matrix",
          width       = 12,
          collapsible = TRUE,
          collapsed   = FALSE,
          status      = "info",
          solidHeader = TRUE,
          shiny::p(
            "Compute a pedigree-based (A), genomic (G), or combined (H) relationship
            matrix with AGHmatrix, preview it, and download it for use in Mate Allocation.",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"
          ),
          shiny::selectInput(
            ns("matrix_type"), "Matrix type",
            choices = c(
              "A — Pedigree-based"                 = "A",
              "G — Genomic"                          = "G",
              "H — Combined (pedigree + genomic)"    = "H"
            ),
            selected = "A"
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'A' || input['%s'] == 'H'", ns("matrix_type"), ns("matrix_type")),
            shiny::h6("Pedigree"),
            shiny::fileInput(ns("pedigree_file"), "Upload pedigree file", accept = c(".txt", ".csv")),
            shiny::uiOutput(ns("pedigree_status_display"))
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'G' || input['%s'] == 'H'", ns("matrix_type"), ns("matrix_type")),
            shiny::h6("Genotypes"),
            shiny::fileInput(ns("genotype_file"), "Upload marker/dosage file", accept = c(".csv", ".txt")),
            shiny::selectInput(
              ns("g_method"), "G matrix method",
              choices  = c("VanRaden", "Yang", "Su", "Vitezica"),
              selected = "VanRaden"
            )
          ),
          shiny::numericInput(ns("ploidy"), "Ploidy", value = 2, min = 2, step = 2),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'H'", ns("matrix_type")),
            # AGHmatrix::Hmatrix implements only "Martini" and "Munoz". Munoz
            # needs a markers argument build_relationship_matrix() does not
            # pass, so Martini is the only method that can actually run.
            shiny::selectInput(
              ns("h_method"), "H matrix method",
              choices  = c("Martini"),
              selected = "Martini"
            )
          ),
          shiny::actionButton(ns("build_btn"), "Build Matrix", icon = shiny::icon("gears")),
          shiny::hr(),
          shiny::uiOutput(ns("build_status")),
          shiny::hr(),
          shinyjs::disabled(
            shiny::downloadButton(ns("download_matrix"), "Download Matrix (.csv)")
          ),
          shiny::p(
            "Downloaded matrices can be uploaded directly in Mate Allocation via
            “Upload precomputed matrix (A/G/H)”.",
            style = "color: #6c757d; font-size: 11px; margin-top: 8px;"
          ),
          shiny::hr(),
          shiny::div(
            style = "text-align: center; margin-top: 5px;",
            shiny::actionButton(
              ns("help_btn"),
              shiny::tagList(shiny::icon("circle-question"), "Help"),
              style = "background-color: #FFD700; color: #000000; border: none; padding: 8px 16px; border-radius: 5px;"
            )
          )
        )
      ),

      # Column 2: Preview
      shiny::column(
        width = 8,
        bs4Dash::box(
          title       = "Matrix Preview",
          width       = 12,
          status      = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          shiny::uiOutput(ns("matrix_summary")),
          shiny::hr(),
          shiny::p(
            "Preview shows up to the first 25 × 25 individuals.",
            style = "color: #6c757d; font-size: 12px;"
          ),
          DT::DTOutput(ns("matrix_preview"))
        )
      )
    )
  )
}

#' Matrix Builder module server
#'
#' @param id Module id
#' @param parent_session Parent (app) session
#'
#' @noRd
mod_matrix_builder_server <- function(id, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    matrix_result             <- shiny::reactiveVal(NULL)
    build_error               <- shiny::reactiveVal(NULL)
    pedigree_validation_stats <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$build_btn, {
      shinyjs::disable("download_matrix")
      build_error(NULL)
      matrix_result(NULL)
      pedigree_validation_stats(NULL)

      tryCatch({
        needs_ped  <- input$matrix_type %in% c("A", "H")
        needs_geno <- input$matrix_type %in% c("G", "H")
        if (needs_ped)  shiny::req(input$pedigree_file)
        if (needs_geno) shiny::req(input$genotype_file)

        pedigree <- NULL
        if (needs_ped) {
          raw_ped <- read_uploaded_table(input$pedigree_file, file_type = "PEDIGREE")
          names(raw_ped) <- tolower(names(raw_ped))
          required_cols <- c("id", "male_parent", "female_parent")
          missing_cols  <- setdiff(required_cols, colnames(raw_ped))
          if (length(missing_cols) > 0) {
            stop(paste0(
              "Missing required column(s): ", paste(missing_cols, collapse = ", "),
              ". File must contain: id, male_parent, female_parent."
            ))
          }
          cleaned_ped <- clean_pedigree(raw_ped, return_stats = TRUE)
          # Use the plain id/male_parent/female_parent table (post dedup and
          # circular-reference cleanup), not the kinship2 pedigree object.
          # kinship2::pedigree()/fixParents() may insert synthetic "hidden
          # spouse" founders for children left with only one known parent
          # (e.g. after the ambiguous-sex fixup below), tracked via internal
          # row-index bookkeeping (findex/mindex) rather than plain ids —
          # reconstructing Sire/Dam strings from that for AGHmatrix is not
          # reliable. AGHmatrix::Amatrix() does its own pedigree validation,
          # so it's safe to hand it the plain cleaned table directly.
          pedigree <- cleaned_ped$cleaned_df
          pedigree_validation_stats(cleaned_ped$stats)
        }

        genotypes <- NULL
        if (needs_geno) {
          genotypes <- read_genotype_matrix(input$genotype_file)
        }

        mat <- build_relationship_matrix(
          matrix_type = input$matrix_type,
          pedigree    = pedigree,
          genotypes   = genotypes,
          ploidy      = input$ploidy,
          g_method    = input$g_method %||% "VanRaden",
          h_method    = input$h_method %||% "Martini"
        )
        validate_relationship_matrix(mat)

        matrix_result(mat)
        shinyjs::enable("download_matrix")
      }, error = function(e) {
        build_error(e$message)
        pedigree_validation_stats(NULL)
      })
    })

    output$pedigree_status_display <- shiny::renderUI({
      render_pedigree_status_html(pedigree_validation_stats())
    })

    output$build_status <- shiny::renderUI({
      err <- build_error()
      mat <- matrix_result()
      if (!is.null(err)) {
        return(shiny::div(
          style = "background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px; border-radius: 4px; font-size: 12px;",
          shiny::tags$strong("Error: "), err
        ))
      }
      if (!is.null(mat)) {
        return(shiny::div(
          style = "background-color: #d4edda; border: 1px solid #c3e6cb; padding: 8px; border-radius: 4px; font-size: 12px;",
          sprintf("%s matrix built successfully for %d individuals.", input$matrix_type, nrow(mat))
        ))
      }
      shiny::p("Upload the required file(s) and click Build Matrix.", style = "color: #6c757d; font-size: 12px;")
    })

    output$matrix_summary <- shiny::renderUI({
      mat <- matrix_result()
      if (is.null(mat)) {
        return(shiny::p("Build a matrix to see a preview and summary.", style = "color: #6c757d;"))
      }
      diag_vals <- diag(mat)
      offdiag   <- mat[upper.tri(mat)]
      shiny::tags$ul(
        style = "font-size: 13px;",
        shiny::tags$li(sprintf("Individuals: %d", nrow(mat))),
        shiny::tags$li(sprintf("Mean diagonal (relationship scale, ≈ 1+F): %.3f", mean(diag_vals, na.rm = TRUE))),
        shiny::tags$li(sprintf("Mean off-diagonal: %.3f", mean(offdiag, na.rm = TRUE))),
        shiny::tags$li(sprintf("Range: %.3f to %.3f", min(mat, na.rm = TRUE), max(mat, na.rm = TRUE)))
      )
    })

    output$matrix_preview <- DT::renderDT({
      mat <- matrix_result()
      shiny::req(mat)
      preview_n <- min(nrow(mat), 25)
      preview   <- round(mat[seq_len(preview_n), seq_len(preview_n), drop = FALSE], 4)
      DT::datatable(preview, options = list(scrollX = TRUE, pageLength = 10), rownames = TRUE)
    })

    output$download_matrix <- shiny::downloadHandler(
      filename = function() paste0("AlloMate_", input$matrix_type, "matrix-", Sys.Date(), ".csv"),
      content  = function(file) {
        mat <- matrix_result()
        shiny::req(mat)
        utils::write.csv(mat, file, row.names = TRUE)
      },
      contentType = "text/csv"
    )

    #### Help button ####
    shiny::observeEvent(input$help_btn, {
      shiny::showModal(
        shiny::modalDialog(
          title     = shiny::tagList(shiny::icon("circle-question"), " Matrix Builder — Help"),
          size      = "l",
          easyClose = TRUE,
          footer    = shiny::modalButton("Close"),
          help_content_matrix_builder(id_prefix = "modal")
        )
      )
    })
  })
}
