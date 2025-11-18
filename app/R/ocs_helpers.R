# OCS (Optimum Contribution Selection) Functions
# Functions for running OCS analysis with either optiSel or custom fallback

#' Run OCS analysis with unified interface
#' @param candidates_df Candidates data frame with id, sex, and index_val columns
#' @param kinship_matrix Kinship matrix for all individuals
#' @param ebv_index Vector of breeding value indices
#' @param desired_inbreeding_rate Target inbreeding rate constraint
#' @param num_offspring Number of offspring to allocate
#' @param per_pair_kinship_limit Optional maximum kinship allowed for any mating pair
#' @return List with Candidate and Mating results
run_ocs <- function(candidates_df,
                    kinship_matrix,
                    ebv_index,
                    desired_inbreeding_rate,
                    num_offspring,
                    per_pair_kinship_limit = NULL) {
  using_optisel <- isTRUE(get0("optisel_available", inherits = TRUE)) &&
    requireNamespace("optiSel", quietly = TRUE)
  using_fallback <- isTRUE(get0("custom_ocs_available", inherits = TRUE)) && !using_optisel

  if (!using_optisel && !using_fallback) {
    stop("❌ OCS functionality is not available. Load optiSel or enable the custom fallback before running OCS.")
  }

  phen <- data.frame(
    Indiv = candidates_df$id,
    Sex = ifelse(candidates_df$sex == "M", "male", "female"),
    BV = ebv_index,
    isCandidate = TRUE,
    stringsAsFactors = FALSE
  )
  candidate_ids <- candidates_df$id
  sKin <- kinship_matrix[candidate_ids, candidate_ids, drop = FALSE]
  rownames(sKin) <- candidate_ids
  colnames(sKin) <- candidate_ids

  if (using_optisel) {
    cand <- optiSel::candes(phen = phen, pKin = sKin)
    con <- list(ub.pKin = desired_inbreeding_rate)
    Offspring <- optiSel::opticont(method = "max.BV", cand = cand, con = con)
  } else {
    cand <- custom_candes(phen = phen, pKin = sKin)
    con <- list(ub.pKin = desired_inbreeding_rate)
    Offspring <- custom_opticont(method = "max.BV", cand = cand, con = con)
  }

  if (using_optisel && "summary" %in% names(Offspring)) {
    # Check if any constraints failed (OK = FALSE)
    failed_constraints <- Offspring$summary[Offspring$summary$OK == FALSE & !is.na(Offspring$summary$OK), ]
    if (nrow(failed_constraints) > 0) {
      constraint_names <- paste(failed_constraints$Name, collapse = ", ")
      stop(paste("❌ OCS optimization failed: Constraints not met:", constraint_names,
                 "Try increasing the inbreeding rate threshold or check your kinship matrix."))
    }
  }
  
  # Guard against empty or invalid solution (infeasible constraint)
  if (is.null(Offspring$parent) || nrow(Offspring$parent) == 0) {
    stop(paste0("❌ No feasible OCS solution found under the current inbreeding constraint (ub.pKin = ",
                desired_inbreeding_rate, "). ",
                "This typically means your candidate population is too closely related to meet this target. ",
                "Try increasing the inbreeding rate threshold (e.g., 0.10 or higher) or reducing the number of offspring."))
  }
  
  # Preserve BV if present; backfill from phen if missing
  Candidate <- Offspring$parent
  if (!"BV" %in% names(Candidate)) {
    Candidate$BV <- phen$BV[match(Candidate$Indiv, phen$Indiv)]
  }
  
  # Additional check: verify non-zero contributions
  if (nrow(Candidate) == 0 || all(Candidate$oc == 0)) {
    stop(paste0("❌ No feasible OCS solution found under the current inbreeding constraint (ub.pKin = ", 
                desired_inbreeding_rate, "). ",
                "This typically means your candidate population is too closely related to meet this target. ",
                "Try increasing the inbreeding rate threshold (e.g., 0.10 or higher) or reducing the number of offspring."))
  }
  
  if (!is.null(per_pair_kinship_limit)) {
    if (!is.numeric(per_pair_kinship_limit) || length(per_pair_kinship_limit) != 1 ||
        is.na(per_pair_kinship_limit) || per_pair_kinship_limit <= 0 ||
        per_pair_kinship_limit >= 1) {
      stop("❌ Per-pair kinship limit must be a numeric value between 0 and 1.")
    }
  }

  # Safe to call noffspring now that Candidate has valid data
  Candidate$n <- if (using_optisel) {
    optiSel::noffspring(Candidate, num_offspring)$nOff
  } else {
    custom_noffspring(Candidate, num_offspring)$nOff
  }
  Candidate <- filter(Candidate, n > 0)
  if (length(unique(Candidate$Sex)) < 2) {
    stop("❌ OCS resulted in only one sex being selected. Cannot generate mating pairs.")
  }
  
  selected_ids <- Candidate$Indiv
  sKin_subset <- sKin[selected_ids, selected_ids, drop = FALSE]

  should_use_custom_matings <- !is.null(per_pair_kinship_limit) || !using_optisel

  if (!should_use_custom_matings) {
    Mating <- optiSel::matings(Candidate, Kin = sKin_subset)
    if (nrow(Mating) > 0 && !"Kin" %in% names(Mating)) {
      Mating$Kin <- vapply(
        seq_len(nrow(Mating)),
        function(i) {
          sire <- Mating$Sire[i]
          dam <- Mating$Dam[i]
          sKin_subset[sire, dam]
        },
        numeric(1)
      )
    }
  } else {
    Mating <- custom_matings(
      Candidate,
      Kin = sKin_subset,
      max_pair_kinship = per_pair_kinship_limit
    )
  }

  if (nrow(Mating) == 0) {
    stop("❌ No feasible mating plan could be generated under the chosen settings. Try increasing the per-pair kinship limit or adjusting offspring counts.")
  }

  validate_mating_consistency(Candidate, Mating)

  list(Candidate = Candidate, Mating = Mating)
}

#' Reconcile offspring counts with realized matings
#' @param Candidate Candidate data frame with n column from noffspring
#' @param Mating Mating plan containing Sire, Dam, and n columns
reconcile_offspring_with_matings <- function(Candidate, Mating) {
  if (nrow(Mating) == 0) {
    Candidate$n <- 0
    return(Candidate[0, ])
  }

  if (!all(c("Sire", "Dam", "n") %in% names(Mating))) {
    stop("❌ Mating plan is missing required columns (Sire, Dam, n).")
  }

  male_totals <- tapply(Mating$n, Mating$Sire, sum)
  female_totals <- tapply(Mating$n, Mating$Dam, sum)

  Candidate$n <- ifelse(
    Candidate$Sex == "male",
    male_totals[Candidate$Indiv],
    female_totals[Candidate$Indiv]
  )
  Candidate$n[is.na(Candidate$n)] <- 0
  dplyr::filter(Candidate, n > 0)
}

#' Validate that mating plan matches expected offspring targets
#' @param Candidate Candidate data frame with target n column from noffspring
#' @param Mating Mating plan containing Sire, Dam, and n columns
validate_mating_consistency <- function(Candidate, Mating) {
  if (nrow(Mating) == 0) {
    stop("❌ No matings available to validate.")
  }

  required_cols <- c("Sire", "Dam", "n")
  if (!all(required_cols %in% names(Mating))) {
    stop("❌ Mating plan is missing required columns (Sire, Dam, n).")
  }

  if (!"n" %in% names(Candidate)) {
    stop("❌ Candidate table is missing target offspring counts (n).")
  }

  male_idx <- Candidate$Sex == "male"
  female_idx <- Candidate$Sex == "female"

  expected_males <- setNames(Candidate$n[male_idx], Candidate$Indiv[male_idx])
  expected_females <- setNames(Candidate$n[female_idx], Candidate$Indiv[female_idx])

  actual_males <- tapply(Mating$n, Mating$Sire, sum)
  actual_females <- tapply(Mating$n, Mating$Dam, sum)

  if (is.null(actual_males)) {
    actual_males <- setNames(numeric(0), character(0))
  }
  if (is.null(actual_females)) {
    actual_females <- setNames(numeric(0), character(0))
  }

  check_counts <- function(expected_vec, actual_vec, label) {
    if (length(expected_vec) == 0) {
      return()
    }
    aligned_actual <- actual_vec[names(expected_vec)]
    if (length(aligned_actual) == 0 && length(expected_vec) > 0) {
      aligned_actual <- rep(0, length(expected_vec))
      names(aligned_actual) <- names(expected_vec)
    } else {
      names(aligned_actual) <- names(expected_vec)
      aligned_actual[is.na(aligned_actual)] <- 0
    }
    mismatches <- names(expected_vec)[aligned_actual != expected_vec]
    if (length(mismatches) > 0) {
      details <- paste(
        sprintf("%s: expected %d, got %d",
                mismatches,
                expected_vec[mismatches],
                aligned_actual[mismatches]),
        collapse = "; "
      )
      stop(sprintf("❌ %s mating plan inconsistent with target offspring counts (%s).", label, details))
    }
  }

  check_counts(expected_males, actual_males, "Male")
  check_counts(expected_females, actual_females, "Female")

  invisible(TRUE)
}

#' Validate OCS inputs before running analysis
#' @param candidates_df Candidates data frame
#' @param kinship_matrix Kinship matrix
#' @param ebv_index Breeding value indices
#' @param desired_inbreeding_rate Target inbreeding rate
#' @param num_offspring Number of offspring
#' @return TRUE if valid, throws error if invalid
validate_ocs_inputs <- function(candidates_df, kinship_matrix, ebv_index, 
                               desired_inbreeding_rate, num_offspring) {
  # Check candidates data
  if (is.null(candidates_df) || nrow(candidates_df) == 0) {
    stop("❌ No candidates provided")
  }
  
  required_cols <- c("id", "sex")
  missing_cols <- setdiff(required_cols, names(candidates_df))
  if (length(missing_cols) > 0) {
    stop(paste("❌ Missing required columns in candidates:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check sex balance
  n_males <- sum(candidates_df$sex == "M")
  n_females <- sum(candidates_df$sex == "F")
  if (n_males == 0 || n_females == 0) {
    stop("❌ Need both males and females for OCS analysis")
  }
  
  # Check kinship matrix
  if (is.null(kinship_matrix) || nrow(kinship_matrix) == 0) {
    stop("❌ Kinship matrix is empty or invalid")
  }
  
  # Check EBV indices
  if (is.null(ebv_index) || length(ebv_index) != nrow(candidates_df)) {
    stop("❌ EBV indices must match number of candidates")
  }
  
  # Check parameters
  if (desired_inbreeding_rate <= 0 || desired_inbreeding_rate > 1) {
    stop("❌ Desired inbreeding rate must be between 0 and 1")
  }
  
  if (num_offspring <= 0 || num_offspring %% 1 != 0) {
    stop("❌ Number of offspring must be a positive integer")
  }
  
  TRUE
}

#' Format OCS results for display
#' @param results OCS results list
#' @return Formatted results for UI display
format_ocs_results <- function(results) {
  # Format candidate table
  candidate_table <- results$Candidate %>%
    select(Indiv, Sex, oc, n) %>%
    mutate(`Optimal Contribution (%)` = round(oc * 100, 1)) %>%
    rename(`ID` = Indiv, `# of offspring` = n) %>%
    select(ID, Sex, `Optimal Contribution (%)`, `# of offspring`)
  
  # Format mating table - handle different column naming schemes
  mating_df <- results$Mating
  
  # Check and standardize column names (optiSel vs custom implementation)
  if ("Sire" %in% names(mating_df)) {
    mating_df <- mating_df %>% rename(Male = Sire)
  }
  if ("Dam" %in% names(mating_df)) {
    mating_df <- mating_df %>% rename(Female = Dam)
  }
  if ("Kin" %in% names(mating_df)) {
    mating_df <- mating_df %>% rename(Kinship = Kin)
  } else if ("kinship" %in% names(mating_df)) {
    mating_df <- mating_df %>% rename(Kinship = kinship)
  } else if ("coeff" %in% names(mating_df)) {
    mating_df <- mating_df %>% rename(Kinship = coeff)
  } else {
    # If no kinship column found, add a placeholder
    mating_df$Kinship <- NA
  }
  
  # Estimate expected EBV for each mating pair by averaging parent EBVs
  bv_lookup <- results$Candidate %>%
    dplyr::select(Indiv, BV)
  sire_bv <- bv_lookup$BV[match(mating_df$Male, bv_lookup$Indiv)]
  dam_bv <- bv_lookup$BV[match(mating_df$Female, bv_lookup$Indiv)]
  expected_bv <- rowMeans(cbind(sire_bv, dam_bv), na.rm = TRUE)
  expected_bv[is.nan(expected_bv)] <- NA_real_
  
  # Ensure numeric columns remain numeric (n and Kinship should be numeric for export)
  # Rename n to a more descriptive name for export
  mating_table <- mating_df %>%
    mutate(
      # Keep n as integer/numeric
      n = as.integer(n),
      # Keep Kinship as numeric
      Kinship = as.numeric(Kinship)
    ) %>%
    rename(`# Matings` = n) %>%
    mutate(`Expected EBV` = round(expected_bv, 3)) %>%
    select(Male, Female, Kinship, `# Matings`, `Expected EBV`)
  
  # Calculate summary statistics - handle different kinship column names
  kinship_values <- NA
  if ("Kin" %in% names(results$Mating)) {
    kinship_values <- results$Mating$Kin
  } else if ("kinship" %in% names(results$Mating)) {
    kinship_values <- results$Mating$kinship
  } else if ("coeff" %in% names(results$Mating)) {
    kinship_values <- results$Mating$coeff
  } else if ("Kinship" %in% names(mating_df)) {
    kinship_values <- mating_df$Kinship
  }
  
  summary_stats <- list(
    n_candidates = nrow(results$Candidate),
    n_males = sum(results$Candidate$Sex == "male"),
    n_females = sum(results$Candidate$Sex == "female"),
    n_matings = nrow(results$Mating),
    total_offspring = sum(results$Candidate$n),
    mean_kinship = if (all(is.na(kinship_values))) NA else mean(kinship_values, na.rm = TRUE),
    mean_contribution = mean(results$Candidate$oc),
    mating_info = {
      info <- attr(results$Mating, "info")
      if (is.null(info)) NA_character_ else info
    }
  )
  
  list(
    candidate_table = candidate_table,
    mating_table = mating_table,
    summary_stats = summary_stats
  )
}

#' Reset OCS runtime state (fallback only)
#' Clears any global aliases and performs GC when custom fallback is active.
reset_ocs_runtime <- function() {
  invisible(FALSE)
}

#' Create Excel workbook with OCS results
#' @param results OCS results list
#' @param params OCS parameters used
#' @param kinship_threshold Optional per-pair kinship threshold for filtering matings
#' @return Workbook object ready for saving
create_ocs_workbook <- function(results, params = NULL, kinship_threshold = NULL) {
  wb <- openxlsx::createWorkbook()
  
  # Add README sheet
  openxlsx::addWorksheet(wb, "README")
  readme_text <- c(
    "Optimum Contribution Selection Results",
    "",
    paste("Generated:", Sys.Date()),
    "",
    "Parameters used:",
    if (!is.null(params)) {
      c(
        paste("- Target inbreeding rate:", params$inbreeding_rate),
        paste("- Number of offspring:", params$num_offspring),
        paste("- Implementation:", if (exists("custom_ocs_available") && custom_ocs_available) "Custom fallback" else "optiSel")
      )
    } else {
      "Parameters not recorded"
    },
    "",
    "Sheets included:",
    "1. Optimal Contributions - Selected candidates and their contributions",
    "2. Mating Plan - Optimal mate pairings to minimize inbreeding",
    "",
    "The patterns in your genetic data have been thoroughly analyzed."
  )
  openxlsx::writeData(wb, "README", readme_text)
  
  # Add optimal contributions
  openxlsx::addWorksheet(wb, "Optimal Contributions")
  contrib_df <- results$Candidate %>%
    select(Indiv, Sex, oc, n) %>%
    mutate(`Contribution (%)` = round(oc * 100, 2)) %>%
    rename(`ID` = Indiv, `# Offspring` = n)
  openxlsx::writeData(wb, "Optimal Contributions", contrib_df)
  
  # Add mating plan
  openxlsx::addWorksheet(wb, "Mating Plan")
  
  # Handle different column naming schemes for mating results
  mating_export <- results$Mating
  
  # Standardize kinship column name
  if ("Kin" %in% names(mating_export)) {
    mating_export <- mating_export %>% mutate(Kinship = round(Kin, 4))
  } else if ("kinship" %in% names(mating_export)) {
    mating_export <- mating_export %>% mutate(Kinship = round(kinship, 4))
  } else if ("coeff" %in% names(mating_export)) {
    mating_export <- mating_export %>% mutate(Kinship = round(coeff, 4))
  } else {
    mating_export$Kinship <- NA
  }
  
  # Optionally enforce per-pair kinship threshold before export
  if (!is.null(kinship_threshold)) {
    mating_export <- mating_export %>%
      dplyr::filter(is.na(Kinship) | Kinship < kinship_threshold)
  }

  # Select and rename columns based on what's available
  if (all(c("Sire", "Dam") %in% names(mating_export))) {
    mating_df <- mating_export %>%
      dplyr::select(Sire, Dam, Kinship, n) %>%
      dplyr::rename(`# Matings` = n)
  } else if (all(c("Male", "Female") %in% names(mating_export))) {
    mating_df <- mating_export %>%
      dplyr::select(Male, Female, Kinship, n) %>%
      dplyr::rename(`# Matings` = n)
  } else {
    # Fallback if column names are different
    mating_df <- mating_export %>%
      dplyr::rename(`# Matings` = n)
  }
  openxlsx::writeData(wb, "Mating Plan", mating_df)
  
  wb
}
