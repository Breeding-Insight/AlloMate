#!/usr/bin/env Rscript

# Comparison script for OCS results
# This script compares custom OCS fallback results with optiSel results
# Run this when optiSel is available to validate the fallback implementation

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(kinship2)
library(quadprog)

# Source the custom OCS functions
source("scripts/optsel_fallback.R")

# Test parameters (same as test script)
TEST_PARAMS <- list(
  inbreeding_rate = 0.05,
  num_offspring = 100,
  trait_weights = c(0.5, 0.5)
)

cat("🔍 OCS Results Comparison\n")
cat("========================\n\n")

# Load test data
candidates <- readr::read_table("data/candidates_2024_10_29.txt")
ped_data <- readr::read_table("data/pedigree_with_family.txt")
weight_ebvs <- readr::read_table("data/weight_ebvs_for_app_with_family.txt")
length_ebvs <- readr::read_table("data/length_ebvs_for_app_with_family.txt")

# Process data (same as test script)
clean_pedigree <- function(ped) {
  ped <- ped %>% mutate(across(c(id, sire, dam), as.factor))
  sex_ped <- ped %>% mutate(sex = case_when(id %in% sire ~ 0, id %in% dam ~ 1, TRUE ~ 2))
  messy_parents <- setdiff(intersect(sex_ped$sire, sex_ped$dam), 0) %>% as.data.frame() %>% rename(id = 1)
  parents_fixed <- sex_ped
  parents_fixed$sire[parents_fixed$sire %in% messy_parents$id] <- 0
  parents_fixed$dam[parents_fixed$dam %in% messy_parents$id] <- 0
  doubled <- parents_fixed %>% count(id, name = "freq") %>% filter(freq > 1) %>% pull(id)
  nodup <- filter(parents_fixed, !id %in% doubled)
  circdep <- nodup %>% mutate(across(c(id, sire, dam), as.character)) %>% filter(id == sire | id == dam)
  clean_ped <- anti_join(nodup, circdep, by = "id")
  ready_ped <- with(clean_ped, kinship2::fixParents(id, sire, dam, sex, missid = "0"))
  final_ped <- with(ready_ped, kinship2::pedigree(id, dadid, momid, sex, missid = "0"))
  final_ped
}

final_ped <- clean_pedigree(ped_data)
kinship_matrix <- kinship2::kinship(final_ped)

# Process EBVs
joint_ebvs <- full_join(
  weight_ebvs %>% rename(EBV.weight = EBV),
  length_ebvs %>% rename(EBV.length = EBV),
  by = "ID"
) %>%
  mutate(
    EBV.weight = tidyr::replace_na(EBV.weight, 0),
    EBV.length = tidyr::replace_na(EBV.length, 0)
  )

joint_ebvs$index_val <- as.vector(
  as.matrix(joint_ebvs[c("EBV.weight", "EBV.length")]) %*% TEST_PARAMS$trait_weights
)

candidates_with_ebv <- left_join(candidates, joint_ebvs, by = c("id" = "ID")) %>%
  filter(!is.na(index_val))

# Run custom OCS
cat("🚀 Running custom OCS...\n")
custom_results <- run_custom_ocs(
  candidates_df = candidates_with_ebv,
  kinship_matrix = kinship_matrix,
  ebv_index = candidates_with_ebv$index_val,
  desired_inbreeding_rate = TEST_PARAMS$inbreeding_rate,
  num_offspring = TEST_PARAMS$num_offspring
)

cat("  ✓ Custom OCS completed\n\n")

# Check if optiSel is available
optisel_available <- FALSE
tryCatch({
  library(optiSel)
  optisel_available <- TRUE
  cat("✅ optiSel package available - running comparison\n\n")
}, error = function(e) {
  cat("⚠️ optiSel package not available - showing only custom results\n\n")
})

if (optisel_available) {
  cat("🎯 Running optiSel OCS...\n")
  
  # Prepare data for optiSel
  phen <- data.frame(
    Indiv = candidates_with_ebv$id,
    Sex = ifelse(candidates_with_ebv$sex == "M", "male", "female"),
    BV = candidates_with_ebv$index_val,
    isCandidate = TRUE,
    stringsAsFactors = FALSE
  )
  
  candidate_ids <- candidates_with_ebv$id
  sKin <- kinship_matrix[candidate_ids, candidate_ids]
  rownames(sKin) <- candidate_ids
  colnames(sKin) <- candidate_ids
  
  # Run optiSel
  cand <- candes(phen = phen, pKin = sKin)
  con <- list(ub.pKin = TEST_PARAMS$inbreeding_rate)
  Offspring <- opticont(method = "max.BV", cand = cand, con = con)
  Candidate <- Offspring$parent[, c("Indiv", "Sex", "oc")]
  Candidate$n <- noffspring(Candidate, TEST_PARAMS$num_offspring)$nOff
  Candidate <- filter(Candidate, n > 0)
  Mating <- matings(Candidate, Kin = sKin)
  
  optisel_results <- list(Candidate = Candidate, Mating = Mating)
  
  cat("  ✓ optiSel OCS completed\n\n")
  
  # Compare results
  cat("📊 Results Comparison\n")
  cat("====================\n\n")
  
  # Compare candidate selections
  cat("👥 Candidate Selection Comparison:\n")
  cat("  Custom OCS selected:", nrow(custom_results$Candidate), "candidates\n")
  cat("  optiSel selected:", nrow(optisel_results$Candidate), "candidates\n")
  
  # Compare overlap
  custom_ids <- custom_results$Candidate$Indiv
  optisel_ids <- optisel_results$Candidate$Indiv
  overlap <- intersect(custom_ids, optisel_ids)
  cat("  Overlap:", length(overlap), "candidates (", 
      round(length(overlap) / length(union(custom_ids, optisel_ids)) * 100, 1), "%)\n\n")
  
  # Compare contributions
  cat("📈 Contribution Comparison:\n")
  custom_mean_contrib <- mean(custom_results$Candidate$oc)
  optisel_mean_contrib <- mean(optisel_results$Candidate$oc)
  cat("  Custom mean contribution:", round(custom_mean_contrib, 4), "\n")
  cat("  optiSel mean contribution:", round(optisel_mean_contrib, 4), "\n")
  cat("  Difference:", round(abs(custom_mean_contrib - optisel_mean_contrib), 4), "\n\n")
  
  # Compare mating plans
  cat("💕 Mating Plan Comparison:\n")
  cat("  Custom matings:", nrow(custom_results$Mating), "pairs\n")
  cat("  optiSel matings:", nrow(optisel_results$Mating), "pairs\n")
  
  # Compare mean kinship
  custom_mean_kin <- mean(custom_results$Mating$Kin)
  optisel_mean_kin <- mean(optisel_results$Mating$Kin)
  cat("  Custom mean kinship:", round(custom_mean_kin, 4), "\n")
  cat("  optiSel mean kinship:", round(optisel_mean_kin, 4), "\n")
  cat("  Difference:", round(abs(custom_mean_kin - optisel_mean_kin), 4), "\n\n")
  
  # Detailed comparison of top candidates
  cat("🏆 Top 10 Candidates Comparison:\n")
  custom_top <- custom_results$Candidate %>%
    arrange(desc(oc)) %>%
    head(10) %>%
    select(Indiv, Sex, oc) %>%
    rename(Custom_oc = oc)
  
  optisel_top <- optisel_results$Candidate %>%
    arrange(desc(oc)) %>%
    head(10) %>%
    select(Indiv, Sex, oc) %>%
    rename(optiSel_oc = oc)
  
  comparison_df <- full_join(custom_top, optisel_top, by = c("Indiv", "Sex")) %>%
    mutate(
      Custom_oc = round(Custom_oc * 100, 2),
      optiSel_oc = round(optiSel_oc * 100, 2),
      Difference = abs(Custom_oc - optiSel_oc)
    )
  
  print(comparison_df)
  
} else {
  # Show only custom results
  cat("📊 Custom OCS Results Summary:\n")
  cat("==============================\n\n")
  
  cat("👥 Selected Candidates:\n")
  cat("  Total:", nrow(custom_results$Candidate), "\n")
  cat("  Males:", sum(custom_results$Candidate$Sex == "male"), "\n")
  cat("  Females:", sum(custom_results$Candidate$Sex == "female"), "\n")
  cat("  Mean contribution:", round(mean(custom_results$Candidate$oc) * 100, 2), "%\n\n")
  
  cat("💕 Mating Plan:\n")
  cat("  Total pairs:", nrow(custom_results$Mating), "\n")
  cat("  Total matings:", sum(custom_results$Mating$n), "\n")
  cat("  Mean kinship:", round(mean(custom_results$Mating$Kin), 4), "\n")
  cat("  Kinship range:", round(range(custom_results$Mating$Kin), 4), "\n\n")
  
  cat("🏆 Top 10 Candidates:\n")
  print(custom_results$Candidate %>%
    select(Indiv, Sex, oc, n) %>%
    mutate(`Contribution (%)` = round(oc * 100, 2)) %>%
    arrange(desc(oc)) %>%
    head(10))
  
  cat("\n🔗 Top 10 Mating Pairs:\n")
  print(custom_results$Mating %>%
    arrange(Kin) %>%
    head(10))
}

cat("\n✅ Comparison completed!\n")
