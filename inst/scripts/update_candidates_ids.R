#!/usr/bin/env Rscript
# Update candidate IDs to match the fake pedigree mapping

library(readr)
library(dplyr)

# Read the original pedigree to recreate the ID mapping
ped_file <- "data/play_data/pedigree_with_family.txt"
ped <- read_tsv(ped_file, show_col_types = FALSE)

# Collect all unique IDs from all columns
all_ids <- unique(c(ped$id, ped$dam, ped$sire))
all_ids <- all_ids[!is.na(all_ids)]  # Remove NA values

# Create a mapping from original IDs to short fake IDs
# Use sequential numbering: ID001, ID002, etc.
id_mapping <- data.frame(
  original_id = sort(all_ids),
  fake_id = sprintf("ID%04d", 1:length(all_ids))
)

# Create named vector for easy lookup
id_lookup <- setNames(id_mapping$fake_id, id_mapping$original_id)

# Read the candidates file
candidates_file <- "data/play_data/candidates_2024_10_29.txt"
candidates <- read_tsv(candidates_file, show_col_types = FALSE)

# Replace IDs in candidates file
candidates_fake <- candidates %>%
  mutate(
    id = ifelse(id %in% names(id_lookup), id_lookup[id], id)
  )

# Write the updated candidates file
output_file <- "data/play_data/candidates_2024_10_29.txt"
write_tsv(candidates_fake, output_file, na = "")

cat(sprintf("Updated candidates file with %d candidates\n", nrow(candidates_fake)))
cat(sprintf("Output written to: %s\n", output_file))
cat(sprintf("First few lines:\n"))
print(head(candidates_fake, 10))

