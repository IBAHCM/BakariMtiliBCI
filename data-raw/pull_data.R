# Put the datasets into the package
library(dplyr)

## Move to the data-raw subfolder to access the raw data
devtools::wd(".", "data-raw")

## BCI data

### Load individual BCI records
data <- read.delim("ViewFullTable.txt")

### Load BCI taxonomic data, create new species column, and extract species
taxa <- read.delim("ViewTax.txt") %>%
  mutate(GenusSpecies = as.factor(paste(Genus, SpeciesName))) %>%
  filter(IDLevel == "species")

### Store species counts from 2010 census in package
source("bci_2010.R")

### Store quadrat metadata in package
source("bci_quadrats.R")

### Store taxonomic data in package
source("bci_taxa.R")
