#' Subsample a dataset by species
#'
#' Sample a dataset as if we only recorded some species.
#'
#' @param dataset A matrix, data frame or tibble containing abundance or incidence data
#' @param count The number of species to retain
#' @return The subsampled dataset in the format it was passed in
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' sample_by_species(bci_2010, count = 20) %>%
#'   sample_by_subcommunities(count = 6)
#'
sample_by_species <- function(dataset, count)
{
  rows <- nrow(dataset)
  if (count > rows) {
    warning("Trying to pick more species than are present")
    count = rows
  }

  sample.rows <- sample(rows, count)
  return(dataset[sample.rows,, drop=FALSE])
}
