# Data Processing Functions
# Functions for reading, cleaning, and processing input data

#' Fallback fixParents
#' @param id vector of IDs
#' @param male_parent vector of male parents
#' @param female_parent vector of female parents
#' @param sex vector of sex values
#' @param missid missing id value
#' @return data.frame
fallback_fixParents <- function(id, male_parent, female_parent, sex, missid = "0") {
  data.frame(id = id, dadid = male_parent, momid = female_parent, sex = sex, stringsAsFactors = FALSE)
}

#' Fallback pedigree
#' @param id ids
#' @param dadid sire ids
#' @param momid dam ids
#' @param sex sex
#' @param missid missing id
#' @return list
fallback_pedigree <- function(id, dadid, momid, sex, missid = "0") {
  list(id = id, dadid = dadid, momid = momid, sex = sex)
}

#' Fallback kinship
#' @param ped pedigree list
#' @return matrix
fallback_kinship <- function(ped) {
  n <- length(ped$id)
  matrix(0.5, n, n, dimnames = list(ped$id, ped$id))
}

#' Read an uploaded delimited text table
#' @param file Uploaded Shiny file object or file path
#' @param file_type Type of file for error messages
#' @return data.frame
read_uploaded_table <- function(file, file_type = "file") {
  path <- if (is.character(file)) file else file$datapath
  name <- if (is.character(file)) basename(file) else file$name
  if (is.null(path) || !file.exists(path)) {
    stop(sprintf("%s: uploaded file is missing or unavailable.", file_type))
  }

  encodings <- c("UTF-8", "UTF-16", "UTF-16LE", "UTF-16BE", "Latin1")
  readers <- list(
    list(
      label = "whitespace-delimited",
      fn = function(path, locale) {
        readr::read_table(path, locale = locale, show_col_types = FALSE)
      }
    ),
    list(
      label = "tab-delimited",
      fn = function(path, locale) {
        readr::read_tsv(path, locale = locale, show_col_types = FALSE)
      }
    ),
    list(
      label = "comma-delimited",
      fn = function(path, locale) {
        readr::read_csv(path, locale = locale, show_col_types = FALSE)
      }
    )
  )

  # Default order favors tab-delimited: pedigree/candidate/EBV .txt files are
  # documented as tab-separated, and read_table()'s whitespace/fixed-width
  # column guessing is fragile — it infers column widths from the first rows
  # it sees, and silently misaligns (without a hard error) once a long run of
  # narrow values (e.g. founders with parent id "0") is followed by much
  # wider ones (real parent ids). Kept as a fallback for genuinely
  # whitespace-delimited files, but tried after the delimited readers.
  ext <- tolower(tools::file_ext(name))
  if (identical(ext, "csv")) {
    readers <- readers[c(3, 2, 1)]
  } else {
    readers <- readers[c(2, 3, 1)]
  }

  first_error <- NULL
  best_single_column <- NULL

  for (encoding in encodings) {
    locale <- readr::locale(encoding = encoding)
    for (reader in readers) {
      df <- tryCatch(
        reader$fn(path, locale),
        error = function(e) {
          if (is.null(first_error)) first_error <<- e$message
          NULL
        }
      )

      if (is.null(df)) next

      # A reader can "succeed" (return a data.frame) while still having
      # silently misparsed some rows — e.g. read_table()'s column-width
      # guess turning out wrong partway through the file. readr records
      # these as problems() without raising an error, so treat any reader
      # that logged problems as failed and fall through to the next one
      # rather than returning corrupted data.
      parse_problems <- tryCatch(readr::problems(df), error = function(e) NULL)
      if (!is.null(parse_problems) && nrow(parse_problems) > 0) {
        if (is.null(first_error)) {
          first_problem <- parse_problems[1, ]
          first_error <<- sprintf(
            "%s reader logged %d parsing problem(s) starting at row %s, column '%s' (expected %s, got %s).",
            reader$label, nrow(parse_problems),
            first_problem$row, first_problem$col,
            first_problem$expected, first_problem$actual
          )
        }
        next
      }

      df <- as.data.frame(df, stringsAsFactors = FALSE)
      names(df) <- trimws(sub("^\ufeff", "", names(df)))

      if (ncol(df) > 1) {
        return(df)
      }

      if (is.null(best_single_column)) best_single_column <- df
    }
  }

  if (!is.null(best_single_column)) {
    stop(sprintf(
      "%s decoded, but appears to contain only one column. Check that it is tab-, comma-, or whitespace-delimited text.",
      file_type
    ))
  }

  details <- if (is.null(first_error)) "" else paste0(" Last read error: ", first_error)
  stop(sprintf(
    "%s could not be decoded. Save it as UTF-8 or UTF-16 delimited text and upload again.%s",
    file_type,
    details
  ))
}

#' Read and process candidate files
#' @importFrom readr read_table
#' @importFrom dplyr filter pull
#' @param file file input
#' @return list
read_candidates <- function(file) {
  df <- read_uploaded_table(file, file_type = "CANDIDATES")
  names(df) <- tolower(names(df))
  if (!"id" %in% names(df) && "candidate" %in% names(df)) {
    names(df)[names(df) == "candidate"] <- "id"
  }
  required <- c("id", "sex")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("CANDIDATES: missing required column(s): %s", paste(missing_cols, collapse = ", ")))
  }
  df$id  <- as.character(df$id)
  df$sex <- toupper(as.character(df$sex))
  list(
    candidates = df,
    males   = filter(df, sex == "M") %>% pull(id),
    females = filter(df, sex == "F") %>% pull(id)
  )
}

#' Clean pedigree
#' @param ped raw ped
#' @importFrom dplyr mutate across case_when rename
#' @importFrom stats na.omit
#' @param return_stats whether to return stats
#' @return cleaned pedigree
clean_pedigree <- function(ped, return_stats = FALSE) {
  kinship2_available <- requireNamespace("kinship2", quietly = TRUE)
  
  # Fail fast with a clear message if required columns are missing
  required     <- c("id", "male_parent", "female_parent")
  missing_cols <- setdiff(required, tolower(names(ped)))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "PEDIGREE: missing required column(s): %s. Expected columns: id, male_parent, female_parent.",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  names(ped) <- tolower(names(ped))
  
  # Keep as character throughout cleaning — only factorize when needed
  ped_chr <- ped %>% mutate(across(c(id, male_parent, female_parent), as.character))
  
  is_missing_parent    <- function(x) { is.na(x) | x == "" | x == "0" }
  total_records        <- nrow(ped_chr)
  unknown_parent_count <- sum(
    is_missing_parent(ped_chr$male_parent) | is_missing_parent(ped_chr$female_parent),
    na.rm = TRUE
  )
  circular_rows            <- (ped_chr$id == ped_chr$male_parent) | (ped_chr$id == ped_chr$female_parent)
  circular_reference_count <- sum(circular_rows, na.rm = TRUE)
  duplicates_removed       <- sum(duplicated(ped_chr$id))
  
  # Fix messy parents while still in character form to avoid factor NA assignment
  # (an id used as a male_parent in some rows and a female_parent in others —
  # ambiguous sex — so both roles are treated as unknown wherever they occur)
  messy_parents <- setdiff(
    intersect(ped_chr$male_parent, ped_chr$female_parent),
    c("0", NA, "")
  )
  messy_parent_count <- sum(
    ped_chr$male_parent %in% messy_parents | ped_chr$female_parent %in% messy_parents,
    na.rm = TRUE
  )
  ped_chr$male_parent[ped_chr$male_parent %in% messy_parents]     <- "0"
  ped_chr$female_parent[ped_chr$female_parent %in% messy_parents] <- "0"

  # Remove duplicates
  ped_chr <- ped_chr[!duplicated(ped_chr$id), ]
  
  # Remove circular dependencies
  circdep <- ped_chr[
    ped_chr$id == ped_chr$male_parent | ped_chr$id == ped_chr$female_parent,
  ]
  ped_chr <- ped_chr[!ped_chr$id %in% circdep$id, ]
  
  # Now factorize and assign sex
  ped_fct <- ped_chr %>%
    mutate(across(c(id, male_parent, female_parent), as.factor)) %>%
    mutate(sex = case_when(
      id %in% male_parent   ~ 0,
      id %in% female_parent ~ 1,
      TRUE                  ~ 2
    ))
  
  final_ped <- with(ped_fct,
                    if (kinship2_available)
                      kinship2::fixParents(id, male_parent, female_parent, sex, missid = "0")
                    else
                      fallback_fixParents(id, male_parent, female_parent, sex, missid = "0")
  ) %>%
    with(.,
         if (kinship2_available)
           kinship2::pedigree(id, dadid, momid, sex, missid = "0")
         else
           fallback_pedigree(id, dadid, momid, sex, missid = "0")
    )
  
  if (return_stats) {
    stats <- list(
      records_loaded           = total_records,
      unknown_parent_count     = unknown_parent_count,
      circular_reference_count = circular_reference_count,
      duplicates_removed       = duplicates_removed,
      messy_parent_count       = messy_parent_count
    )
    # Cleaned tabular pedigree (id/male_parent/female_parent, post dedup and
    # circular-reference removal, pre kinship2 conversion). Callers that need
    # a plain Ind/Sire/Dam table (e.g. AGHmatrix via build_relationship_matrix())
    # should use this instead of reverse-engineering it from the kinship2
    # pedigree object's internal findex/mindex bookkeeping.
    cleaned_df <- ped_chr[, c("id", "male_parent", "female_parent")]
    return(list(pedigree = final_ped, stats = stats, cleaned_df = cleaned_df))
  }

  return(final_ped)
}

#' Compute kinship matrix from a pedigree
#' @importFrom tibble tibble as_tibble column_to_rownames
#' @importFrom tidyr pivot_longer
#' @param ped pedigree
#' @param males males
#' @param females females
#' @return list
compute_kinship_matrix <- function(ped, males, females) {
  kinship2_available <- requireNamespace("kinship2", quietly = TRUE)
  kinship_matrix <- if (exists("kinship2_available") && kinship2_available) kinship2::kinship(ped) else fallback_kinship(ped)
  summarize_kinship_matrix(kinship_matrix, males, females)
}

#' Summarize a kinship matrix (quantiles + long-format table)
#'
#' Shared by compute_kinship_matrix() (pedigree-derived) and the "upload
#' precomputed matrix" path in mod_allomate, so any A/G/H-derived kinship
#' matrix can populate the same Kinship & EBV preview.
#'
#' @importFrom tibble tibble as_tibble column_to_rownames
#' @importFrom tidyr pivot_longer
#' @param kinship_matrix A kinship matrix (rownames/colnames = individual IDs)
#' @param males male candidate ids
#' @param females female candidate ids
#' @return list
summarize_kinship_matrix <- function(kinship_matrix, males, females) {
  # Restrict to candidates actually present in the kinship matrix. A candidate id
  # missing from the matrix would otherwise trigger "subscript out of bounds"
  # and abort kinship-quantile computation, leaving only the EBV quantile row.
  males   <- intersect(males,   rownames(kinship_matrix))
  females <- intersect(females, colnames(kinship_matrix))
  kin_mat_sel    <- kinship_matrix[males, females, drop = FALSE]
  kin_quads      <- tibble(
    Data = "Kinship",
    Q25  = quantile(kin_mat_sel, 0.25),
    Q50  = quantile(kin_mat_sel, 0.50),
    Q75  = quantile(kin_mat_sel, 0.75),
    Q100 = quantile(kin_mat_sel, 1.00)
  ) %>% column_to_rownames("Data")
  kinship_results <- as_tibble(kin_mat_sel, rownames = "Male") %>%
    pivot_longer(-Male, names_to = "Female", values_to = "Kinship")
  list(results = kinship_results, quads = kin_quads, matrix = kin_mat_sel)
}

#' Process EBVs
#' @importFrom readr read_table
#' @importFrom dplyr select rename full_join mutate
#' @importFrom purrr imap reduce
#' @importFrom tidyr replace_na
#' @param trait_counter n traits
#' @param input shiny input
#' @param prefix prefix
#' @return list
process_ebvs <- function(trait_counter, input, prefix = "") {
  ebv_inputs <- list()
  for (i in seq_len(trait_counter)) {
    file_i   <- input[[paste0(prefix, "trait_file_",   i)]]
    weight_i <- input[[paste0(prefix, "trait_weight_", i)]]
    if (!is.null(file_i) && !is.null(weight_i)) {
      df_raw <- read_uploaded_table(file_i, file_type = paste("EBV trait", i))
      if (!"ID"  %in% names(df_raw)) names(df_raw)[1] <- "ID"
      if (!"EBV" %in% names(df_raw)) names(df_raw)[2] <- "EBV"
      df_raw$EBV <- as.numeric(df_raw$EBV)
      ebv_inputs <- append(ebv_inputs, list(select(df_raw, ID, EBV), weight_i))
    }
  }
  if (length(ebv_inputs) >= 2 && length(ebv_inputs) %% 2 == 0) {
    rel_weights  <- unlist(ebv_inputs[seq(2, length(ebv_inputs), by = 2)])
    weight_total <- sum(rel_weights)
    ebv_dfs      <- ebv_inputs[seq(1, length(ebv_inputs), by = 2)]
    ebv_dfs      <- imap(ebv_dfs, ~ rename(.x, !!paste0("EBV.", .y) := EBV))
    joint_ebvs   <- reduce(ebv_dfs, full_join, by = "ID") %>%
      mutate(across(starts_with("EBV."), ~ replace_na(.x, 0)))
    list(joint_ebvs = joint_ebvs, rel_weights = rel_weights, weight_total = weight_total)
  } else NULL
}

#' Calculate index
#' @param joint_ebvs df
#' @param rel_weights weights
#' @return df
calculate_index <- function(joint_ebvs, rel_weights) {
  ebv_cols <- grep("^EBV\\.", names(joint_ebvs))
  joint_ebvs[ebv_cols] <- lapply(joint_ebvs[ebv_cols], as.numeric)
  joint_ebvs$index_val <- as.vector(as.matrix(joint_ebvs[ebv_cols]) %*% rel_weights)
  joint_ebvs
}

#' Format ID list
#' @param ids ids
#' @param limit limit
#' @return string
format_id_list <- function(ids, limit = 4) {
  ids <- unique(ids)
  ids <- ids[!is.na(ids) & ids != ""]
  if (length(ids) == 0) return("")
  if (length(ids) <= limit) {
    paste(ids, collapse = ", ")
  } else {
    paste(c(ids[seq_len(limit)], "(5 or more)"), collapse = ", ")
  }
}

#' Render pedigree-cleaning validation stats as status boxes
#'
#' Shared by mod_allomate.R (pedigree_status_display) and mod_matrix_builder.R
#' so both pedigree upload points show the same records-loaded / unknown-parent /
#' circular-reference / missing-candidate / duplicates-removed summary produced
#' by clean_pedigree(..., return_stats = TRUE).
#'
#' @param stats A stats list as returned by clean_pedigree()$stats, optionally
#'   with a missing_candidates / missing_candidate_ids entry added by the caller.
#' @return A shiny::HTML() object, or NULL if stats is NULL.
#' @noRd
render_pedigree_status_html <- function(stats) {
  if (is.null(stats)) return(NULL)
  get_count    <- function(val) if (is.null(val) || is.na(val)) 0L else as.integer(val)
  format_count <- function(val) format(get_count(val), big.mark = ",", scientific = FALSE)
  records        <- format_count(stats$records_loaded)
  unknown_count  <- get_count(stats$unknown_parent_count)
  circular_count <- get_count(stats$circular_reference_count)
  missing_count  <- get_count(stats$missing_candidates)
  duplicates     <- get_count(stats$duplicates_removed)
  messy_count    <- get_count(stats$messy_parent_count)
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
  if (messy_count > 0) yellow_warnings <- c(yellow_warnings,
                                            paste0("<p style='margin:", if (length(yellow_warnings) == 0) "0" else "4px 0 0", ";'>",
                                                   format_count(stats$messy_parent_count),
                                                   " relationships used an individual as both a male_parent and a female_parent (ambiguous sex); that parent was treated as unknown for those rows</p>"))
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
}

#' Build a Bootstrap collapsible panel card
#'
#' A shared utility used by help content functions and module servers to render
#' collapsible sub-panels in the UI.
#'
#' @param panel_id A unique string used as the HTML element id for the collapse target.
#' @param icon_name A Font Awesome icon name passed to [shiny::icon()].
#' @param label A string label displayed in the panel header button.
#' @param body_content A shiny tag or [shiny::tagList()] rendered inside the panel body.
#'
#' @return A [shiny::tags$div()] structure representing a Bootstrap card with a
#'   collapsible body.
#'
#' @importFrom shiny tags icon tagList
#'
#' @noRd
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
