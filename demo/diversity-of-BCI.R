#' ---
#' title: "Biodiversity BCI data"
#' subtitle: "Manipulation of the BCI biodiversity data."
#' author: "Bakari Mtili"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# Call the required libraries
library(BakariMtiliBCI) # all analysis scripts will start with this library
library(rdiversity)
library(magrittr)
library(vegan)


# Make a copy of the taxonomy as a data frame
taxonomy <- as.data.frame(bci_taxa)

# Make sure the species are ordered correctly
rownames(taxonomy) <- taxonomy$GenusSpecies
taxonomy <- taxonomy[rownames(bci_2010),]

# Create an object which will be used for analyses of diversity
meta <- metacommunity(bci_2010)

# Calculate the gamma diversity using meta_gamma() in rdiversity package
results <- meta_gamma(meta, qs = seq(from = 0, to = 5))

# Plot the results
plot(diversity ~ q, type = "l", data = results)

# Create a distance matrix from a taxonomy
taxSim <- taxonomy %>%
  tax2dist(c(GenusSpecies=0, Genus=1, Family=2, Other=3)) %>%
  dist2sim("linear") # dist2sim creates similarity matrix from the distance matrix
metatax <- metacommunity(bci_2010, taxSim)
results4tax <- meta_gamma(metatax, qs = seq(from = 0, to = 5))
lines(diversity ~ q, col=2, data = results4tax)


# Swap rows and columns
abundances <- t(bci_2010)

# Calculate species richness (gamma diversity)
specnumber(abundances, groups = 1)
