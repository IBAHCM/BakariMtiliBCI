## code to prepare `bci_quadrats` dataset goes here

library(dplyr)
library(tibble)

# Work out useful information about the quadrats themselves
## Note: they index from 0 and they are 20m x 20m quadrats
bci_quadrats <- records %>% select(QuadratName, row, col) %>%
  unique %>%
  mutate(x = row * 20, y = col * 20, row = row + 1, col = col + 1) %>%
  mutate(Quadrat = sprintf("Q.%04d", as.integer(QuadratName))) %>%
  arrange(Quadrat) %>%
  as_tibble

# Store in package
usethis::use_data(bci_quadrats, overwrite = TRUE)
