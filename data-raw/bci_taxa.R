## code to prepare `bci_taxa` dataset goes here

library(dplyr)
library(tibble)

# Discard species not identified to species level
bci_taxa <- taxa %>% filter(IDLevel == "species") %>%
  select(GenusSpecies, Genus, Family) %>%
  filter(GenusSpecies %in% rownames(bci_2010)) %>%
  unique %>% as_tibble

# Store in package
usethis::use_data(bci_taxa, overwrite = TRUE)
