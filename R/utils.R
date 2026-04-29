# Data Processing Functions
# Functions for reading, cleaning, and processing input data

#' Fallback fixParents
#' @param id vector of IDs
#' @param sire vector of sires
#' @param dam vector of dams
#' @param sex vector of sex values
#' @param missid missing id value
#' @return data.frame
fallback_fixParents <- function(id, sire, dam, sex, missid = "0") {
  data.frame(id = id, dadid = sire, momid = dam, sex = sex, stringsAsFactors = FALSE)
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

#' Read and process candidate files
#' @importFrom readr read_table
#' @importFrom dplyr filter pull
#' @param file file input
#' @return list
read_candidates <- function(file) {
  df <- read_table(file$datapath, show_col_types = FALSE)
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

#' Clean pedigree
#' @param ped raw ped
#' @importFrom dplyr mutate across case_when rename
#' @importFrom stats na.omit
#' @param return_stats whether to return stats
#' @return cleaned pedigree
clean_pedigree <- function(ped, return_stats = FALSE) {
  kinship2_available <- requireNamespace("kinship2", quietly = TRUE)

  ped_chr <- ped %>% mutate(across(c(id, male_parent, female_parent), as.character))
  is_missing_parent <- function(x) { is.na(x) | x == "" | x == "0" }
  total_records <- nrow(ped_chr)
  unknown_parent_count <- sum(is_missing_parent(ped_chr$male_parent) | is_missing_parent(ped_chr$female_parent), na.rm = TRUE)
  circular_rows <- (ped_chr$id == ped_chr$male_parent) | (ped_chr$id == ped_chr$female_parent)
  circular_reference_count <- sum(circular_rows, na.rm = TRUE)
  duplicates_removed <- sum(duplicated(ped_chr$id))
  final_ped <- ped_chr %>% mutate(across(c(id, male_parent, female_parent), as.factor)) %>% mutate(sex = case_when(id %in% male_parent ~ 0, id %in% female_parent ~ 1, TRUE ~ 2)) %>% {
    messy_parents <- setdiff(intersect(.$male_parent, .$female_parent), 0) %>% as.data.frame() %>% rename(id = 1)
    parents_fixed <- .
    parents_fixed$male_parent[parents_fixed$male_parent %in% messy_parents$id] <- 0
    parents_fixed$female_parent[parents_fixed$female_parent %in% messy_parents$id] <- 0
    parents_fixed
  } %>% { .[!duplicated(.$id), ] } %>% { circdep <- .; circdep$id <- as.character(circdep$id); circdep$male_parent <- as.character(circdep$male_parent); circdep$female_parent <- as.character(circdep$female_parent); circdep <- circdep[circdep$id == circdep$male_parent | circdep$id == circdep$female_parent, ]; .[!.$id %in% circdep$id, ] } %>%
    with(., if (exists("kinship2_available") && kinship2_available) kinship2::fixParents(id, male_parent, female_parent, sex, missid = "0") else fallback_fixParents(id, male_parent, female_parent, sex, missid = "0")) %>%
    with(., if (exists("kinship2_available") && kinship2_available) kinship2::pedigree(id, dadid, momid, sex, missid = "0") else fallback_pedigree(id, dadid, momid, sex, missid = "0"))
  if (return_stats) {
    stats <- list(records_loaded = total_records, unknown_parent_count = unknown_parent_count, circular_reference_count = circular_reference_count, duplicates_removed = duplicates_removed)
    return(list(pedigree = final_ped, stats = stats))
  }
  return(final_ped)
}

#' Compute kinship matrix
#' @importFrom tibble tibble as_tibble column_to_rownames
#' @importFrom tidyr pivot_longer
#' @param ped pedigree
#' @param males males
#' @param females females
#' @return list
compute_kinship_matrix <- function(ped, males, females) {
  kinship2_available <- requireNamespace("kinship2", quietly = TRUE)

  kinship_matrix <- if (exists("kinship2_available") && kinship2_available) kinship2::kinship(ped) else fallback_kinship(ped)
  kin_mat_sel <- kinship_matrix[males, females]
  kin_quads <- tibble(Data = "Kinship", Q25 = quantile(kin_mat_sel, 0.25), Q50 = quantile(kin_mat_sel, 0.50), Q75 = quantile(kin_mat_sel, 0.75), Q100 = quantile(kin_mat_sel, 1.00)) %>% column_to_rownames("Data")
  kinship_results <- as_tibble(kin_mat_sel, rownames = "Male") %>% pivot_longer(-Male, names_to = "Female", values_to = "Kinship")
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
    file_i <- input[[paste0(prefix, "trait_file_", i)]]
    weight_i <- input[[paste0(prefix, "trait_weight_", i)]]
    if (!is.null(file_i) && !is.null(weight_i)) {
      df_raw <- read_table(file_i$datapath)
      if (!"ID" %in% names(df_raw)) names(df_raw)[1] <- "ID"
      if (!"EBV" %in% names(df_raw)) names[df_raw][2] <- "EBV"
      df_raw$EBV <- as.numeric(df_raw$EBV)
      ebv_inputs <- append(ebv_inputs, list(select(df_raw, ID, EBV), weight_i))
    }
  }
  if (length(ebv_inputs) >= 2 && length(ebv_inputs) %% 2 == 0) {
    rel_weights <- unlist(ebv_inputs[seq(2, length(ebv_inputs), by = 2)])
    weight_total <- sum(rel_weights)
    ebv_dfs <- ebv_inputs[seq(1, length(ebv_inputs), by = 2)]
    ebv_dfs <- imap(ebv_dfs, ~ rename(.x, !!paste0("EBV.", .y) := EBV))
    joint_ebvs <- reduce(ebv_dfs, full_join, by = "ID") %>% mutate(across(starts_with("EBV."), ~ replace_na(.x, 0)))
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
  if (length(ids) <= limit) paste(ids, collapse = ", ") else paste(c(ids[seq_len(limit)], "(5 or more)"), collapse = ", ")
}
