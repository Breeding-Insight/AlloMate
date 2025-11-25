  library(tidyverse)
  library(skimr)
  library(waldo)
  
  ped <- read_table("/Users/aja294/Documents/Allomate/AlloMate/data/pedigree_with_family.txt")
  
# original function
ped_og <- ped %>% mutate(across(c(id, sire, dam), as.factor))
sex_ped <- ped_og %>% mutate(sex = case_when(id %in% sire ~ 0, id %in% dam ~ 1, TRUE ~ 2))
messy_parents <- setdiff(intersect(sex_ped$sire, sex_ped$dam), 0) %>% as.data.frame() %>% rename(id = 1)
parents_fixed <- sex_ped
parents_fixed$sire[parents_fixed$sire %in% messy_parents$id] <- 0
parents_fixed$dam[parents_fixed$dam %in% messy_parents$id] <- 0
doubled <- parents_fixed %>% count(id, name = "freq") %>% filter(freq > 1) %>% pull(id)
nodup <- filter(parents_fixed, !id %in% doubled)
circdep <- nodup %>% mutate(across(c(id, sire, dam), as.character)) %>% filter(id == sire | id == dam)
clean_ped <- anti_join(nodup, circdep, by = "id")
ready_ped <- with(clean_ped, kinship2::fixParents(id, sire, dam, sex, missid = "0"))
final_ped_og <- with(ready_ped, kinship2::pedigree(id, dadid, momid, sex, missid = "0"))


# Updated function - efficiently piped version
final_ped <- ped %>%
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
    # Remove duplicates (same as original)
    doubled <- count(., id, name = "freq") %>% filter(freq > 1) %>% pull(id)
    filter(., !id %in% doubled)
  } %>%
  {
    # Remove circular dependencies (same as original)
    circdep <- mutate(., across(c(id, sire, dam), as.character)) %>% 
               filter(id == sire | id == dam)
    anti_join(., circdep, by = "id")
  } %>%
  with(., kinship2::fixParents(id, sire, dam, sex, missid = "0")) %>%
  with(., kinship2::pedigree(id, dadid, momid, sex, missid = "0"))

# Verify identical output
waldo::compare(final_ped_og, final_ped)


