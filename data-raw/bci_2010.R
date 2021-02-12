## code to prepare `bci_2010` dataset goes here

library(dplyr)
library(reshape2)

# Clean data to remove secondary and dead stems of trees,
# and species not in taxonomy
records <- data %>%
  filter(PrimaryStem == "main", Status == "alive", !is.na(QuadratName)) %>%
  filter(GenusSpecies %in% taxa$GenusSpecies) %>%
  select(GenusSpecies, PlotCensusNumber, QuadratName) %>%
  mutate(col=as.integer(floor(QuadratName/100)),
         row=as.integer(QuadratName-col*100)) %>%
  filter(row < 25)

# Extract table at a single timepoint
bci_2010 <- records %>% filter(PlotCensusNumber == 7) %>%
  select(GenusSpecies, QuadratName) %>%
  acast(GenusSpecies ~ QuadratName, fill = 0,
        value.var = "QuadratName", fun.aggregate = length)

# Call columns Q.xxyy, and store package
colnames(bci_2010) <- sprintf("Q.%04d", as.integer(colnames(bci_2010)))

# Store in package
usethis::use_data(bci_2010, overwrite = TRUE)
