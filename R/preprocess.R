preprocess_dataset <- function(
  dataset,
  grouping
) {
  if(!is.null(grouping)) {
    dataset <- dataset %>% add_grouping(grouping)
  }
  dataset
}

preprocess_trajectory <- function(
  trajectory,
  pseudotime
) {
  if(!is.null(pseudotime)) {
    trajectory <- trajectory %>% add_pseudotime(pseudotime)
  }
  trajectory
}
