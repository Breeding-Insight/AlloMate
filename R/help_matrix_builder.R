# help_content_matrix_builder.R
#' Matrix Builder help content
#'
#' Returns the UI content for the Matrix Builder help section.
#' Used by both mod_help and the Matrix Builder module's own help button.
#'
#' @param collapse_fn A function with signature (panel_id, icon_name, label, body_content).
#'   Defaults to the internal make_collapse_panel.
#' @param id_prefix A string prefix to namespace panel IDs and avoid duplicate DOM ids.
#'
#' @noRd
help_content_matrix_builder <- function(collapse_fn = NULL, id_prefix = "") {

  pid <- function(x) if (nchar(id_prefix) > 0) paste0(id_prefix, "_", x) else x

  if (is.null(collapse_fn)) {
    collapse_fn <- make_collapse_panel
  }

  shiny::tagList(

    shiny::h6(shiny::tagList(shiny::icon("circle-info"), " Overview"),
              style = "font-weight: bold;"),
    shiny::p(
      "Matrix Builder computes a pedigree-based (A), genomic (G), or combined
      single-step (H) relationship matrix using the AGHmatrix package. Build a
      matrix here, download it as a CSV, then upload it in Mate Allocation via
      “Upload precomputed matrix (A/G/H)” to use it for kinship-aware
      selection and OCS.",
      style = "font-size: 13px;"
    ),
    shiny::hr(style = "margin: 8px 0;"),

    shiny::h6(shiny::tagList(shiny::icon("list-ol"), " Steps"),
              style = "font-weight: bold;"),
    shiny::tags$ol(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML("<strong>Choose a matrix type</strong> — A, G, or H.")),
      shiny::tags$li(shiny::HTML(
        "<strong>Upload the required file(s)</strong> — a pedigree for A or H, a
        marker/dosage file for G or H."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Set ploidy</strong> and, if relevant, the G/H method, then click
        <em>Build Matrix</em>."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Review the preview and summary</strong> — dimensions, mean
        diagonal/off-diagonal, and value range."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Download the matrix</strong> as a CSV and upload it in Mate
        Allocation's “Upload precomputed matrix” option."
      ))
    ),
    shiny::hr(style = "margin: 8px 0;"),

    shiny::h6(shiny::tagList(shiny::icon("file-lines"), " Input File Formats"),
              style = "font-weight: bold;"),
    shiny::p("Click each file type to expand format details.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),

    collapse_fn(
      panel_id     = pid("mb_help_pedigree"),
      icon_name    = "sitemap",
      label        = "Pedigree File (.txt or .csv)",
      body_content = shiny::tagList(
        shiny::p("Same format used in Mate Allocation: one row per individual.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$table(
          class = "table table-bordered table-sm",
          style = "width: auto; font-size: 12px; margin-top: 4px;",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("id"), shiny::tags$th("male_parent"), shiny::tags$th("female_parent")
          )),
          shiny::tags$tbody(
            shiny::tags$tr(shiny::tags$td("A001"), shiny::tags$td("S001"), shiny::tags$td("D001")),
            shiny::tags$tr(shiny::tags$td("S001"), shiny::tags$td("0"), shiny::tags$td("0")),
            shiny::tags$tr(shiny::tags$td("D001"), shiny::tags$td("0"), shiny::tags$td("0"))
          )
        ),
        shiny::p("Use 0 for unknown parents. Required for A and H matrices.",
                 style = "font-size: 11px; color: #6c757d;")
      )
    ),

    collapse_fn(
      panel_id     = pid("mb_help_genotypes"),
      icon_name    = "dna",
      label        = "Marker/Dosage File (.csv or .txt)",
      body_content = shiny::tagList(
        shiny::p("One row per individual: an ID column followed by one column per marker,
        coded as allele dosage (0, 1, 2 for diploids). Leave cells blank for missing genotypes.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$table(
          class = "table table-bordered table-sm",
          style = "width: auto; font-size: 12px; margin-top: 4px;",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("id"), shiny::tags$th("SNP1"), shiny::tags$th("SNP2"), shiny::tags$th("SNP3")
          )),
          shiny::tags$tbody(
            shiny::tags$tr(shiny::tags$td("A001"), shiny::tags$td("0"), shiny::tags$td("1"), shiny::tags$td("2")),
            shiny::tags$tr(shiny::tags$td("A002"), shiny::tags$td("2"), shiny::tags$td("1"), shiny::tags$td("0"))
          )
        ),
        shiny::p("Required for G and H matrices.",
                 style = "font-size: 11px; color: #6c757d;")
      )
    ),

    shiny::hr(style = "margin: 8px 0;"),

    shiny::h6(shiny::tagList(shiny::icon("sliders"), " Matrix Types Explained"),
              style = "font-weight: bold;"),
    collapse_fn(
      panel_id     = pid("mb_help_a"),
      icon_name    = "sitemap",
      label        = "A — Pedigree-based relationship matrix",
      body_content = shiny::p(
        "Expected additive relationship from pedigree alone (AGHmatrix::Amatrix). Diagonal
        approx. 1 + F, where F is the inbreeding coefficient.",
        style = "font-size: 13px; margin: 0;"
      )
    ),
    collapse_fn(
      panel_id     = pid("mb_help_g"),
      icon_name    = "dna",
      label        = "G — Genomic relationship matrix",
      body_content = shiny::p(
        "Realized relationship estimated from marker genotypes (AGHmatrix::Gmatrix,
        VanRaden method by default). Captures Mendelian sampling that pedigree alone cannot.",
        style = "font-size: 13px; margin: 0;"
      )
    ),
    collapse_fn(
      panel_id     = pid("mb_help_h"),
      icon_name    = "layer-group",
      label        = "H — Combined single-step matrix",
      body_content = shiny::p(
        "Blends the pedigree-based A matrix with the genomic G matrix (AGHmatrix::Hmatrix),
        so genotyped and non-genotyped individuals can be evaluated together. Requires both
        a pedigree and a marker/dosage file.",
        style = "font-size: 13px; margin: 0;"
      )
    ),
    collapse_fn(
      panel_id     = pid("mb_help_scale"),
      icon_name    = "ruler",
      label        = "Relationship vs. kinship scale",
      body_content = shiny::p(
        "Matrix Builder outputs relationship-scale matrices (diagonal approx. 1 + F), matching
        AGHmatrix's convention. When uploading a downloaded matrix in Mate Allocation, leave
        “Values are already kinship coefficients” unchecked so it is automatically
        halved to the kinship scale (approx. 0.5 × (1 + F)) used by optiSel.",
        style = "font-size: 13px; margin: 0;"
      )
    ),

    shiny::hr(style = "margin: 8px 0;"),

    shiny::h6(shiny::tagList(shiny::icon("box"), " Dependency"),
              style = "font-weight: bold;"),
    shiny::p(
      shiny::HTML("Matrix Builder requires the <code>AGHmatrix</code> package. If it is not
      installed, building a matrix will show an error with installation instructions."),
      style = "font-size: 13px;"
    )
  )
}
