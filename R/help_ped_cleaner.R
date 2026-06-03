# help_content_ped_cleaner.R
#' Pedigree Cleaner help content
#'
#' Returns the UI content for the Pedigree Cleaner help section.
#' Used by both mod_help and the Pedigree Cleaner module's own help button.
#'
#' @param collapse_fn A function with signature (panel_id, icon_name, label, body_content).
#'   Defaults to the internal make_collapse_panel.
#' @param id_prefix A string prefix to namespace panel IDs and avoid duplicate DOM ids.
#'
#' @noRd
help_content_ped_cleaner <- function(collapse_fn = NULL, id_prefix = "") {
  
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
  
  # Shared pedigree table helpers
  ped_table <- function(rows) {
    shiny::tags$table(
      class = "table table-bordered table-sm",
      style = "width: auto; font-size: 12px; margin-bottom: 6px;",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("id"),
          shiny::tags$th("male_parent"),
          shiny::tags$th("female_parent")
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
  
  shiny::tagList(
    
    shiny::h6(shiny::tagList(shiny::icon("circle-info"), " Overview"),
              style = "font-weight: bold;"),
    shiny::p(
      "The Pedigree Cleaner automatically detects and corrects common pedigree issues.
      Exact duplicates and missing parents are always corrected.
      Conflicting trios and inconsistent sex roles are corrected when their
      respective options are checked. Cycles and dependencies are reported only
      and must be resolved manually. The corrected pedigree and per-issue reports
      are bundled into a single zip file for download.",
      style = "font-size: 13px;"
    ),
    shiny::hr(style = "margin: 8px 0;"),
    
    # Steps
    shiny::h6(shiny::tagList(shiny::icon("list-ol"), " Steps"),
              style = "font-weight: bold;"),
    shiny::tags$ol(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML(
        "<strong>Upload</strong> a tab-separated .txt or .tsv, or a comma-separated .csv,
        file with three columns: <code>id</code>, <code>male_parent</code>, <code>female_parent</code>."
      )),
      shiny::tags$li(shiny::HTML(
        "<strong>Select correction options</strong> -- choose whether to correct conflicting trios
        and/or inconsistent sex roles."
      )),
      shiny::tags$li(shiny::HTML("<strong>Run Pedigree Check</strong> -- scans for all five issue types below.")),
      shiny::tags$li(shiny::HTML(
        "<strong>Review</strong> the Run Summary panel on the right and the expandable result tables
        in the Issue Tables tab."
      )),
      shiny::tags$li(shiny::HTML("<strong>Export</strong> the corrected pedigree and reports as a .zip file."))
    ),
    shiny::hr(style = "margin: 8px 0;"),
    
    # Issue Types
    shiny::h6(
      shiny::tagList(shiny::icon("triangle-exclamation"), " Issue Types -- What Is Detected and How It Is Corrected"),
      style = "font-weight: bold;"
    ),
    shiny::p("Click each issue type to expand a before/after example.",
             style = "color: #6c757d; font-size: 12px; margin-bottom: 8px;"),
    
    collapse_fn(
      panel_id     = pid("pc_help_exact_dup"),
      icon_name    = "copy",
      label        = "Exact Duplicates Removed (always corrected)",
      body_content = shiny::tagList(
        shiny::p("Fully identical rows are removed, keeping only one copy. This is always applied.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Before:"),
        ped_table(shiny::tagList(
          ped_row("A1", "B1", "C1"),
          ped_row("A1", "B1", "C1", bg = "#f8d7da")
        )),
        shiny::p("The highlighted row is an exact copy -- only one row will be kept.",
                 style = "color: #721c24; font-size: 11px; margin-bottom: 6px;"),
        shiny::tags$strong("After:"),
        ped_table(ped_row("A1", "B1", "C1"))
      )
    ),
    
    collapse_fn(
      panel_id     = pid("pc_help_conflict"),
      icon_name    = "exclamation",
      label        = "Conflicting Trios Resolved (corrected when option is checked)",
      body_content = shiny::tagList(
        shiny::p("The same individual ID appears with different parents.
        When correction is enabled, the ambiguous parent field is set to 0 (unknown)
        and duplicate rows are collapsed to one.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Before:"),
        ped_table(shiny::tagList(
          ped_row("A1", "B1", "C1"),
          ped_row("A1", "B2", "C1", bg = "#f8d7da")
        )),
        shiny::p("A1 has two different male parents (B1 and B2) -- male_parent cannot be determined.",
                 style = "color: #856404; font-size: 11px; margin-bottom: 6px;"),
        shiny::tags$strong("After (correct conflicting trios = TRUE):"),
        ped_table(ped_row("A1", "0", "C1", bg = "#fff3cd")),
        shiny::p("The conflicting male_parent is set to 0 (unknown). female_parent is unchanged since it was consistent.",
                 style = "color: #856404; font-size: 11px;")
      )
    ),
    
    collapse_fn(
      panel_id     = pid("pc_help_messy"),
      icon_name    = "shuffle",
      label        = "Inconsistent Sex Roles (corrected when option is checked)",
      body_content = shiny::tagList(
        shiny::p("An individual appears in the male_parent column in one record and the female_parent
        column in another, indicating an inconsistent sex/role assignment across the pedigree.
        When correction is enabled, the parent fields referencing that individual are set to 0
        and any resulting exact duplicates are removed.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Before:"),
        ped_table(shiny::tagList(
          ped_row("A1", "B1", "C1"),
          ped_row("A2", "C1", "B1", bg = "#f8d7da")
        )),
        shiny::p("B1 and C1 swap male_parent/female_parent roles across records -- flagged for review.",
                 style = "color: #856404; font-size: 11px; margin-bottom: 6px;"),
        shiny::tags$strong("After (correct inconsistent sex roles = TRUE):"),
        ped_table(shiny::tagList(
          ped_row("A1", "0", "0", bg = "#fff3cd"),
          ped_row("A2", "0", "0", bg = "#fff3cd")
        )),
        shiny::p("Parent fields referencing conflicting IDs are set to 0. Exact duplicates resulting from this correction are removed.",
                 style = "color: #856404; font-size: 11px;")
      )
    ),
    
    collapse_fn(
      panel_id     = pid("pc_help_missing"),
      icon_name    = "user-plus",
      label        = "Missing Parents Added (always corrected)",
      body_content = shiny::tagList(
        shiny::p("A parent is referenced in the male_parent or female_parent column but never appears as an individual.
        Missing parents are always added as founders with unknown parents (0).",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Before:"),
        ped_table(ped_row("A1", "B1", "C1")),
        shiny::p("B1 and C1 are referenced but have no row of their own.",
                 style = "color: #0c5460; font-size: 11px; margin-bottom: 6px;"),
        shiny::tags$strong("After:"),
        ped_table(shiny::tagList(
          ped_row("A1", "B1", "C1"),
          ped_row("B1", "0",  "0",  bg = "#d4edda"),
          ped_row("C1", "0",  "0",  bg = "#d4edda")
        ))
      )
    ),
    
    collapse_fn(
      panel_id     = pid("pc_help_cycles"),
      icon_name    = "rotate",
      label        = "Cycles / Dependencies Detected (reported only -- must be resolved manually)",
      body_content = shiny::tagList(
        shiny::p("An animal appears as its own ancestor, creating a circular relationship.
        These are detected and reported but are never automatically corrected -- they must be resolved manually.",
                 style = "margin-bottom: 6px;"),
        shiny::tags$strong("Example:"),
        ped_table(shiny::tagList(
          ped_row("A1", "B1", "C1"),
          ped_row("B1", "A1", "C2", bg = "#f8d7da")
        )),
        shiny::p("A1 is listed as the male_parent of B1, but B1 is also listed as the male_parent of A1 -- flagged for manual review.",
                 style = "color: #721c24; font-size: 11px;")
      )
    ),
    
    shiny::hr(style = "margin: 8px 0;"),
    
    # Export Contents
    shiny::h6(shiny::tagList(shiny::icon("download"), " Export Contents"),
              style = "font-weight: bold;"),
    shiny::tags$ul(
      style = "font-size: 13px;",
      shiny::tags$li(shiny::HTML("<code>corrected_pedigree.txt</code> -- the cleaned pedigree, tab-separated.")),
      shiny::tags$li(shiny::HTML("One .txt report per issue type (only included if issues were found):
        <code>exact_duplicates.txt</code>, <code>conflicting_trios.txt</code>,
        <code>inconsistent_sex_roles.txt</code>, <code>missing_parents.txt</code>,
        <code>dependencies.txt</code>."))
    )
  )
}