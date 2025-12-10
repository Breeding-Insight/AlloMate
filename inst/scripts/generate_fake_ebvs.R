#!/usr/bin/env Rscript
# Generate fake length and width EBV values with specified standard deviations

library(readr)
library(dplyr)

# Read the length EBV file
length_file <- "data/play_data/length_ebvs_for_app_with_family.txt"
length_ebvs <- read_tsv(length_file, show_col_types = FALSE)

# Get all unique IDs from the EBV file
all_ids <- unique(length_ebvs$ID)
all_ids <- all_ids[!is.na(all_ids)]  # Remove NA values

# Create a mapping from original IDs to fake IDs
# Use the same format as the fake pedigree (ID####)
# Start from ID2000 to avoid conflicts with pedigree IDs (which start around ID1600-2400)
id_mapping <- data.frame(
  original_id = sort(all_ids),
  fake_id = sprintf("ID%04d", 2000 + 1:length(all_ids))
)
id_lookup <- setNames(id_mapping$fake_id, id_mapping$original_id)

# Set seed for reproducibility (optional, can remove for truly random)
set.seed(42)

# Generate random length EBVs with mean 0 and SD 10
length_ebvs_fake <- length_ebvs %>%
  mutate(
    ID = ifelse(ID %in% names(id_lookup), id_lookup[ID], ID),
    EBV = rnorm(n = n(), mean = 0, sd = 10)
  )

# Generate width EBV file with same IDs and SD 5
width_ebvs_fake <- length_ebvs_fake %>%
  mutate(
    EBV = rnorm(n = n(), mean = 0, sd = 5)
  )

# Write the updated length EBV file
output_length <- "data/play_data/length_ebvs_for_app_with_family.txt"
write_tsv(length_ebvs_fake, output_length, na = "")

# Write the width EBV file
output_width <- "data/play_data/width_ebvs_for_app_with_family.txt"
write_tsv(width_ebvs_fake, output_width, na = "")

cat(sprintf("Generated fake EBV files:\n"))
cat(sprintf("  Length EBVs: %d records, SD=10\n", nrow(length_ebvs_fake)))
cat(sprintf("  Width EBVs: %d records, SD=5\n", nrow(width_ebvs_fake)))
cat(sprintf("\nLength EBV summary:\n"))
print(summary(length_ebvs_fake$EBV))
cat(sprintf("\nWidth EBV summary:\n"))
print(summary(width_ebvs_fake$EBV))
cat(sprintf("\nFirst few length EBVs:\n"))
print(head(length_ebvs_fake, 10))
cat(sprintf("\nFirst few width EBVs:\n"))
print(head(width_ebvs_fake, 10))

