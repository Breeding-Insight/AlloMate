# Data Processing Functions
# Functions for reading, cleaning, and processing input data

# Fallback functions for when kinship2 is not available
fallback_fixParents <- function(id, sire, dam, sex, missid = "0") {
  # Simple fallback - just return the data as is
  data.frame(id = id, dadid = sire, momid = dam, sex = sex, stringsAsFactors = FALSE)
}

fallback_pedigree <- function(id, dadid, momid, sex, missid = "0") {
  # Simple fallback - return a list with the pedigree data
  list(id = id, dadid = dadid, momid = momid, sex = sex)
}

fallback_kinship <- function(ped) {
  # Simple fallback - return identity matrix
  # This is a very basic approximation
  n <- length(ped$id)
  matrix(0.5, n, n, dimnames = list(ped$id, ped$id))
}

#' Read and process candidate files
#' @param file Uploaded file object
#' @return List with candidates data frame and male/female ID vectors
read_candidates <- function(file) {
  df <- readr::read_table(file$datapath, show_col_types = FALSE)
  names(df) <- tolower(names(df))

  if (!"id" %in% names(df) && "candidate" %in% names(df)) {
    names(df)[names(df) == "candidate"] <- "id"
  }

  required <- c("id", "sex")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("CANDIDATES: missing required column(s): %s", paste(missing_cols, collapse = ", ")))
  }

  df$id <- as.character(df$id)
  df$sex <- toupper(as.character(df$sex))

  list(
    candidates = df,
    males = filter(df, sex == "M") %>% pull(id),
    females = filter(df, sex == "F") %>% pull(id)
  )
}

#' Clean and validate pedigree data
#' @param ped Raw pedigree data frame
#' @return Cleaned pedigree object for kinship calculation
clean_pedigree <- function(ped, return_stats = FALSE) {
  ped_chr <- ped %>%
    mutate(across(c(id, sire, dam), as.character))

  is_missing_parent <- function(x) {
    is.na(x) | x == "" | x == "0"
  }

  total_records <- nrow(ped_chr)
  unknown_parent_count <- sum(is_missing_parent(ped_chr$sire) | is_missing_parent(ped_chr$dam), na.rm = TRUE)
  circular_rows <- (ped_chr$id == ped_chr$sire) | (ped_chr$id == ped_chr$dam)
  circular_reference_count <- sum(circular_rows, na.rm = TRUE)
  # Count only the extra duplicates (not the first occurrence we'll keep)
  duplicates_removed <- sum(duplicated(ped_chr$id))

  final_ped <- ped_chr %>%
    mutate(across(c(id, sire, dam), as.factor)) %>%
    mutate(sex = case_when(id %in% sire ~ 0, id %in% dam ~ 1, TRUE ~ 2)) %>%
    {
      # Fix messy parents (same logic as original)
      messy_parents <- setdiff(intersect(.$sire, .$dam), 0) %>% as.data.frame() %>% rename(id = 1)
      parents_fixed <- .
      parents_fixed$sire[parents_fixed$sire %in% messy_parents$id] <- 0
      parents_fixed$dam[parents_fixed$dam %in% messy_parents$id] <- 0
      parents_fixed
    } %>%
    {
      # Remove duplicate rows, keeping first occurrence
      .[!duplicated(.$id), ]
    } %>%
    {
      # Remove circular dependencies (same as original)
      circdep <- .
      circdep$id <- as.character(circdep$id)
      circdep$sire <- as.character(circdep$sire)
      circdep$dam <- as.character(circdep$dam)
      circdep <- circdep[circdep$id == circdep$sire | circdep$id == circdep$dam, ]
      .[!.$id %in% circdep$id, ]
    } %>%
    with(., if (exists("kinship2_available") && kinship2_available) {
      kinship2::fixParents(id, sire, dam, sex, missid = "0")
    } else {
      fallback_fixParents(id, sire, dam, sex, missid = "0")
    }) %>%
    with(., if (exists("kinship2_available") && kinship2_available) {
      kinship2::pedigree(id, dadid, momid, sex, missid = "0")
    } else {
      fallback_pedigree(id, dadid, momid, sex, missid = "0")
    })

  if (return_stats) {
    stats <- list(
      records_loaded = total_records,
      unknown_parent_count = unknown_parent_count,
      circular_reference_count = circular_reference_count,
      duplicates_removed = duplicates_removed
    )
    return(list(pedigree = final_ped, stats = stats))
  }

  return(final_ped)
}

#' Compute kinship matrix and statistics
#' @param ped Pedigree object
#' @param males Vector of male IDs
#' @param females Vector of female IDs
#' @return List with kinship results, quartiles, and matrix
compute_kinship_matrix <- function(ped, males, females) {
  kinship_matrix <- if (exists("kinship2_available") && kinship2_available) {
    kinship2::kinship(ped)
  } else {
    fallback_kinship(ped)
  }
  kin_mat_sel <- kinship_matrix[males, females]
  
  kin_quads <- tibble(
    Data = "Kinship",
    Q25 = quantile(kin_mat_sel, 0.25),
    Q50 = quantile(kin_mat_sel, 0.50),
    Q75 = quantile(kin_mat_sel, 0.75),
    Q100 = quantile(kin_mat_sel, 1.00)
  ) %>% column_to_rownames("Data")
  
  kinship_results <- as_tibble(kin_mat_sel, rownames = "Male") %>%
    pivot_longer(-Male, names_to = "Female", values_to = "Kinship")
  
  list(results = kinship_results, quads = kin_quads, matrix = kin_mat_sel)
}

#' Process EBV files and combine with weights
#' @param trait_counter Number of traits
#' @param input Shiny input object
#' @param prefix Prefix for input names (for OCS-specific inputs)
#' @return List with combined EBVs, weights, and total
process_ebvs <- function(trait_counter, input, prefix = "") {
  ebv_inputs <- list()
  for (i in seq_len(trait_counter)) {
    file_i <- input[[paste0(prefix, "trait_file_", i)]]
    weight_i <- input[[paste0(prefix, "trait_weight_", i)]]
    if (!is.null(file_i) && !is.null(weight_i)) {
      df_raw <- readr::read_table(file_i$datapath)
      if (!"ID" %in% names(df_raw)) names(df_raw)[1] <- "ID"
      if (!"EBV" %in% names(df_raw)) names(df_raw)[2] <- "EBV"
      # Ensure EBV column is numeric
      df_raw$EBV <- as.numeric(df_raw$EBV)
      ebv_inputs <- append(ebv_inputs, list(select(df_raw, ID, EBV), weight_i))
    }
  }
  
  if (length(ebv_inputs) >= 2 && length(ebv_inputs) %% 2 == 0) {
    rel_weights <- unlist(ebv_inputs[seq(2, length(ebv_inputs), by = 2)])
    weight_total <- sum(rel_weights)
    ebv_dfs <- ebv_inputs[seq(1, length(ebv_inputs), by = 2)]
    ebv_dfs <- purrr::imap(ebv_dfs, ~ rename(.x, !!paste0("EBV.", .y) := EBV))
    
    joint_ebvs <- purrr::reduce(ebv_dfs, full_join, by = "ID") %>%
      mutate(across(starts_with("EBV."), ~ replace_na(.x, 0)))
    
    list(joint_ebvs = joint_ebvs, rel_weights = rel_weights, weight_total = weight_total)
  } else {
    NULL
  }
}

#' Calculate breeding value index from multiple traits
#' @param joint_ebvs Combined EBV data frame
#' @param rel_weights Vector of trait weights
#' @return Data frame with calculated index values
calculate_index <- function(joint_ebvs, rel_weights) {
  ebv_cols <- grep("^EBV\\.", names(joint_ebvs))
  # Ensure EBV columns are numeric
  joint_ebvs[ebv_cols] <- lapply(joint_ebvs[ebv_cols], as.numeric)
  joint_ebvs$index_val <- as.vector(as.matrix(joint_ebvs[ebv_cols]) %*% rel_weights)
  joint_ebvs
}
