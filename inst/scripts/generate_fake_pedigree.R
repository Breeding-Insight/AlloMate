#!/usr/bin/env Rscript
# Generate a fake play version of pedigree with short names
# Preserves all progeny-parent relationships

library(readr)
library(dplyr)

# Read the original pedigree
ped_file <- "data/play_data_2/pedigree_with_family.txt"
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

# Function to replace IDs in a column
replace_ids <- function(col) {
  ifelse(is.na(col), NA, id_lookup[col])
}

# Apply replacements
ped_fake <- ped %>%
  mutate(
    id = replace_ids(id),
    dam = replace_ids(dam),
    sire = replace_ids(sire)
  )

# Write the fake pedigree
output_file <- "data/play_data_2/pedigree_with_family_fake.txt"
write_tsv(ped_fake, output_file, na = "")

cat(sprintf("Generated fake pedigree with %d individuals\n", length(all_ids)))
cat(sprintf("Output written to: %s\n", output_file))
cat(sprintf("First few lines:\n"))
print(head(ped_fake, 10))

