#' Subsample a dataset by subcommunities
#'
#' Sample a dataset as if we only counted in some subcommunities.
#'
#' @param dataset A matrix, data frame or tibble containing abundance or incidence data
#' @param count The number of subcommunities to retain
#' @return The subsampled dataset in the format it was passed in
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' sample_by_species(bci_2010, count = 20) %>%
#'   sample_by_subcommunities(count = 6)
#'
sample_by_subcommunities <- function(dataset, count)
{
  cols <- ncol(dataset)
  if (count > cols) {
    warning("Trying to pick more subcommunities than are present")
    count = cols
  }

  sample.cols <- sample(cols, count)
  return(dataset[, sample.cols, drop=FALSE])
}
