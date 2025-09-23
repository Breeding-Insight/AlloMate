# OCS (Optimum Contribution Selection) Functions
# Functions for running OCS analysis with either optiSel or custom fallback

#' Run OCS analysis with unified interface
#' @param candidates_df Candidates data frame with id, sex, and index_val columns
#' @param kinship_matrix Kinship matrix for all individuals
#' @param ebv_index Vector of breeding value indices
#' @param desired_inbreeding_rate Target inbreeding rate constraint
#' @param num_offspring Number of offspring to allocate
#' @return List with Candidate and Mating results
run_ocs <- function(candidates_df, kinship_matrix, ebv_index, desired_inbreeding_rate, num_offspring) {
  # Check if OCS functions are available (either optiSel or custom fallback)
  if (!exists("candes") || !exists("opticont") || !exists("noffspring") || !exists("matings")) {
    stop("❌ OCS functions are not available. Neither optiSel nor custom fallback could be loaded.")
  }
  
  # Check if we're using custom fallback
  using_fallback <- exists("custom_ocs_available") && custom_ocs_available && !optisel_available
  
  phen <- data.frame(
    Indiv = candidates_df$id,
    Sex = ifelse(candidates_df$sex == "M", "male", "female"),
    BV = ebv_index,
    isCandidate = TRUE,
    stringsAsFactors = FALSE
  )
  candidate_ids <- candidates_df$id
  sKin <- kinship_matrix[candidate_ids, candidate_ids]
  rownames(sKin) <- candidate_ids
  colnames(sKin) <- candidate_ids
  
  cand <- candes(phen = phen, pKin = sKin)
  con <- list(ub.pKin = desired_inbreeding_rate)
  Offspring <- opticont(method = "max.BV", cand = cand, con = con)
  
  # Check if solution is valid (only needed for real optiSel package)
  if (exists("optisel_available") && optisel_available && "summary" %in% names(Offspring)) {
    # Check if any constraints failed (OK = FALSE)
    failed_constraints <- Offspring$summary[Offspring$summary$OK == FALSE & !is.na(Offspring$summary$OK), ]
    if (nrow(failed_constraints) > 0) {
      constraint_names <- paste(failed_constraints$Name, collapse = ", ")
      stop(paste("❌ OCS optimization failed: Constraints not met:", constraint_names,
                 "Try increasing the inbreeding rate threshold or check your kinship matrix."))
    }
  }
  
  Candidate <- Offspring$parent[, c("Indiv", "Sex", "oc")]
  Candidate$n <- noffspring(Candidate, num_offspring)$nOff
  Candidate <- filter(Candidate, n > 0)
  if (length(unique(Candidate$Sex)) < 2) {
    stop("❌ OCS resulted in only one sex being selected. Cannot generate mating pairs.")
  }
  
  # For real optiSel package, subset kinship matrix to match selected candidates
  if (exists("optisel_available") && optisel_available) {
    selected_ids <- Candidate$Indiv
    sKin_subset <- sKin[selected_ids, selected_ids]
    # optiSel matings function expects the Candidate data frame, not phenotype data
    Mating <- matings(Candidate, Kin = sKin_subset)
  } else {
    # Custom fallback already handles this correctly
    Mating <- matings(Candidate, Kin = sKin)
  }
  list(Candidate = Candidate, Mating = Mating)
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
  
  mating_table <- mating_df %>% mutate_all(as.character)
  
  # Calculate summary statistics - handle different kinship column names
  kinship_values <- NA
  if ("Kin" %in% names(results$Mating)) {
    kinship_values <- results$Mating$Kin
  } else if ("kinship" %in% names(results$Mating)) {
    kinship_values <- results$Mating$kinship
  } else if ("coeff" %in% names(results$Mating)) {
    kinship_values <- results$Mating$coeff
  }
  
  summary_stats <- list(
    n_candidates = nrow(results$Candidate),
    n_males = sum(results$Candidate$Sex == "male"),
    n_females = sum(results$Candidate$Sex == "female"),
    n_matings = nrow(results$Mating),
    total_offspring = sum(results$Candidate$n),
    mean_kinship = if (all(is.na(kinship_values))) NA else mean(kinship_values, na.rm = TRUE),
    mean_contribution = mean(results$Candidate$oc)
  )
  
  list(
    candidate_table = candidate_table,
    mating_table = mating_table,
    summary_stats = summary_stats
  )
}

#' Create Excel workbook with OCS results
#' @param results OCS results list
#' @param params OCS parameters used
#' @return Workbook object ready for saving
create_ocs_workbook <- function(results, params = NULL) {
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
  
  # Select and rename columns based on what's available
  if (all(c("Sire", "Dam") %in% names(mating_export))) {
    mating_df <- mating_export %>%
      select(Sire, Dam, Kinship, n) %>%
      rename(`# Matings` = n)
  } else {
    # Fallback if column names are different
    mating_df <- mating_export %>%
      rename(`# Matings` = n)
  }
  openxlsx::writeData(wb, "Mating Plan", mating_df)
  
  wb
}
