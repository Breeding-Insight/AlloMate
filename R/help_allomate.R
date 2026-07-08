# help_content_allomate.R  
#' AlloMate help content
#'
#' Returns the UI content for the AlloMate help section.
#' Used by both mod_help and the AlloMate module's own help button.
#'
#' @param collapse_fn A function with signature (panel_id, icon_name, label, body_content)
#'   used to build collapsible sub-panels. Defaults to the internal make_collapse_panel.
#' @param id_prefix A string prefix to namespace panel IDs and avoid duplicate DOM ids.
#'
#' @noRd
help_content_allomate <- function(collapse_fn = NULL, id_prefix = "") {

  pid <- function(x) if (nchar(id_prefix) > 0) paste0(id_prefix, "_", x) else x

  if (is.null(collapse_fn)) {
    collapse_fn <- function(panel_id, icon_name, label, body_content) {
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
  }

  shiny::tagList(

    shiny::h6(shiny::tagList(shiny::icon("circle-info"), " Overview"),
              style = "font-weight: bold;"),
    shiny::p(
      "AlloMate helps you select optimal breeding pairs by combining pedigree-based
      kinship estimates with estimated breeding values (EBVs). It supports both
      index-based mate selection and Optimum Contribution Selection (OCS) to
      balance genetic gain against inbreeding.",
      style = "font-size: 13px;"
    ),
    shiny::hr(style = "margin: 8px 0;"),

    # ── Steps ────────────────────────────────────────────────────────
    shiny::h6(shiny::tagList(shiny::icon("list-ol"), " Steps"),
              style = "font-weight: bold;"),
    shiny::tags$ol(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML(
        "<strong>Upload a candidate list</strong> — a <code>.csv</code> or <code>.txt</code>
        file with columns <code>id</code> and <code>sex</code> (M / F)."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Upload a pedigree file</strong> — a tab-separated <code>.txt</code> file
        with columns <code>id</code>, <code>male_parent</code>, <code>female_parent</code>.
        The kinship matrix is computed automatically."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Set the kinship threshold</strong> — crosses with kinship at or above
        this value will be excluded from results."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Add trait files and weights</strong> — upload one EBV file per trait
        and assign relative weights. Weights must sum to 1."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Review the Kinship and EBV tab</strong> — inspect the distribution
        summary table and the filtered results matrix."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Configure and run OCS</strong> — set your desired inbreeding rate and
        number of offspring, then click <em>Run OCS</em>."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Export results</strong> — click <em>Export All Results</em> to download
        a <code>.zip</code> file containing all tables and a parameter log."
      ))
    ),
    shiny::hr(style = "margin: 8px 0;"),

    # ── Input File Formats ───────────────────────────────────────────
    shiny::h6(shiny::tagList(shiny::icon("file-lines"), " Input File Formats"),
              style = "font-weight: bold;"),
    shiny::p("Click each file type to expand format details.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),

    collapse_fn(
      panel_id     = pid("am_help_candidates"),  # <-- prefixed
      icon_name    = "users",
      label        = "Candidate List (.csv or .txt)",
      body_content = shiny::tagList(
        shiny::p("A comma- or tab-separated file with one row per candidate.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Required columns:"),
        shiny::tags$table(
          class = "table table-bordered table-sm",
          style = "width: auto; font-size: 12px; margin-top: 4px;",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("id"), shiny::tags$th("sex")
          )),
          shiny::tags$tbody(
            shiny::tags$tr(shiny::tags$td("A001"), shiny::tags$td("M")),
            shiny::tags$tr(shiny::tags$td("A002"), shiny::tags$td("F")),
            shiny::tags$tr(shiny::tags$td("A003"), shiny::tags$td("M"))
          )
        ),
        shiny::p(shiny::HTML("Sex values must be <code>M</code> (male) or <code>F</code> (female)."),
                 style = "font-size: 11px; color: #6c757d;")
      )
    ),

    collapse_fn(
      panel_id     = pid("am_help_pedigree"),  # <-- prefixed
      icon_name    = "sitemap",
      label        = "Pedigree File (.txt)",
      body_content = shiny::tagList(
        shiny::p("A tab-separated file with one row per individual.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Required columns:"),
        shiny::tags$table(
          class = "table table-bordered table-sm",
          style = "width: auto; font-size: 12px; margin-top: 4px;",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("id"),
            shiny::tags$th("male_parent"),
            shiny::tags$th("female_parent")
          )),
          shiny::tags$tbody(
            shiny::tags$tr(
              shiny::tags$td("A001"), shiny::tags$td("S001"), shiny::tags$td("D001")
            ),
            shiny::tags$tr(
              shiny::tags$td("S001"), shiny::tags$td("0"), shiny::tags$td("0")
            ),
            shiny::tags$tr(
              shiny::tags$td("D001"), shiny::tags$td("0"), shiny::tags$td("0")
            )
          )
        ),
        shiny::p(
          shiny::HTML("Use <code>0</code> for unknown parents. The pedigree is automatically
          cleaned before kinship computation."),
          style = "font-size: 11px; color: #6c757d;"
        )
      )
    ),

    collapse_fn(
      panel_id     = pid("am_help_ebv"),  # <-- prefixed
      icon_name    = "chart-line",
      label        = "Trait EBV File (.csv or .txt, one per trait)",
      body_content = shiny::tagList(
        shiny::p("A comma- or tab-separated file with one row per individual.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Required columns:"),
        shiny::tags$table(
          class = "table table-bordered table-sm",
          style = "width: auto; font-size: 12px; margin-top: 4px;",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("ID"), shiny::tags$th("EBV")
          )),
          shiny::tags$tbody(
            shiny::tags$tr(shiny::tags$td("A001"), shiny::tags$td("1.23")),
            shiny::tags$tr(shiny::tags$td("A002"), shiny::tags$td("-0.45")),
            shiny::tags$tr(shiny::tags$td("A003"), shiny::tags$td("0.87"))
          )
        ),
        shiny::p("Upload one file per trait. Each file should cover the same set of candidates.
        Candidates without an EBV for any trait will be excluded from analysis.",
                 style = "font-size: 11px; color: #6c757d;")
      )
    ),

    shiny::hr(style = "margin: 8px 0;"),

    # ── Tutorial Data ────────────────────────────────────────────────
    shiny::h6(shiny::tagList(shiny::icon("download"), " Tutorial Data"),
              style = "font-weight: bold;"),
    shiny::p("Download a complete example data set for the Mate Allocation workflow.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),
    shiny::tags$a(
      href = "www/tutorial_data.zip",
      download = "tutorial_data.zip",
      class = "btn btn-sm btn-outline-danger",
      shiny::tagList(shiny::icon("download"), " Download Tutorial Data")
    ),

    shiny::hr(style = "margin: 8px 0;"),

    # ── Parameters ───────────────────────────────────────────────────
    shiny::h6(shiny::tagList(shiny::icon("sliders"), " Parameters Explained"),
              style = "font-weight: bold;"),
    shiny::p("Click each parameter to expand its description.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),

    collapse_fn(
      panel_id     = pid("am_help_thresh"),  # <-- prefixed
      icon_name    = "filter",
      label        = "Kinship Threshold",
      body_content = shiny::p(
        "The maximum kinship coefficient allowed between two mates. Pairs at or above
        this value are excluded from the filtered results table and the EBV matrix.
        A value of 0.0625 corresponds roughly to a first-cousin relationship.
        Defaults to 1 (no filtering).",
        style = "font-size: 13px; margin: 0;"
      )
    ),
    collapse_fn(
      panel_id     = pid("am_help_weights"),  # <-- prefixed
      icon_name    = "balance-scale",
      label        = "Trait Weights",
      body_content = shiny::p(
        "Each trait is assigned a relative weight that determines its contribution to the
        selection index. Weights must sum to 1. For example, two traits of equal importance
        would each receive a weight of 0.5. Adding or removing traits resets file uploads.",
        style = "font-size: 13px; margin: 0;"
      )
    ),
    collapse_fn(
      panel_id     = pid("am_help_inbreeding"),  # <-- prefixed
      icon_name    = "arrows-to-circle",
      label        = "Desired Inbreeding Rate (OCS)",
      body_content = shiny::p(
        "The target rate of inbreeding per generation used by the OCS optimiser. Lower
        values preserve more genetic diversity but may reduce short-term genetic gain.
        Typical values range from 0.01 to 0.05. Must be greater than 0.",
        style = "font-size: 13px; margin: 0;"
      )
    ),
    collapse_fn(
      panel_id     = pid("am_help_noffspring"),  # <-- prefixed
      icon_name    = "baby",
      label        = "Number of Offspring (OCS)",
      body_content = shiny::p(
        "The total number of offspring to be allocated across all mating pairs in the OCS
        plan. This controls how contributions are rounded and distributed among selected
        parents.",
        style = "font-size: 13px; margin: 0;"
      )
    ),
    collapse_fn(
      panel_id     = pid("am_help_greedy"),  # <-- prefixed
      icon_name    = "gear",
      label        = "Advanced OCS Options (optiSel required)",
      body_content = shiny::tagList(
        shiny::p(shiny::HTML("These options are only shown when the <code>optiSel</code> package is available:"),
                 style = "font-size: 13px; margin-bottom: 6px;"),
        shiny::tags$ul(
          style = "font-size: 13px;",
          shiny::tags$li(shiny::HTML(
            "<strong>Enforce per-pair kinship threshold</strong> — filters the mating plan
            so that no allocated pair exceeds the desired inbreeding rate."
          )),
          shiny::tags$li(shiny::HTML(
            "<strong>Use greedy mating (browser-safe)</strong> — replaces the LP-based
            mating allocator with a greedy heuristic. Use this if the app freezes during
            large mating plan generation."
          )),
          shiny::tags$li(shiny::HTML(
            "<strong>Bypass quadprog (heuristic contributions)</strong> — skips the
            quadratic programming solver for contribution optimisation and uses a fast
            heuristic instead."
          ))
        )
      )
    ),

    shiny::hr(style = "margin: 8px 0;"),

    # ── Output Tabs ──────────────────────────────────────────────────
    shiny::h6(shiny::tagList(shiny::icon("table-columns"), " Output Tabs"),
              style = "font-weight: bold;"),
    shiny::tags$ul(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML(
        "<strong>Kinship and EBV</strong> — displays the distribution summary
        (Q25 / Q50 / Q75 / Q100) for both kinship and EBV, plus a filtered table of
        crosses that pass the kinship threshold and have a positive index EBV."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>OCS</strong> — shows the selected candidates with their optimised
        contributions from Optimum Contribution Selection."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Mate Allocation</strong> — shows the recommended mating plan with
        allocated offspring counts. Maximize the results panel (expand icon, top-right)
        for a dashboard view: the Kinship/EBV quantile summary as a full-width header
        with the OCS and Mate Allocation tables side by side beneath it."
      ))
    ),

    shiny::hr(style = "margin: 8px 0;"),

    # ── Export Contents ──────────────────────────────────────────────
    shiny::h6(shiny::tagList(shiny::icon("download"), " Export Contents"),
              style = "font-weight: bold;"),
    shiny::tags$ul(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML("<code>README.tsv</code> — overview and explanation of all files.")),
      shiny::tags$li(shiny::HTML("<code>Filtered_Results.tsv</code> — crosses with positive EBVs below the kinship threshold.")),
      shiny::tags$li(shiny::HTML("<code>EBV_Matrix.tsv</code> — full male × female matrix with failing crosses masked.")),
      shiny::tags$li(shiny::HTML("<code>OCS_Candidates.tsv</code> — selected candidates and their optimised contributions (OCS only).")),
      shiny::tags$li(shiny::HTML("<code>Mating_Plan.tsv</code> — recommended mating pairs and offspring allocation (OCS only).")),
      shiny::tags$li(shiny::HTML("<code>Parameters.tsv</code> — kinship threshold, inbreeding rate, and offspring count used."))
    )
  )
}
