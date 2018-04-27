#' @export
`plot.dynwrap::with_expression` <- function(x, ...) {
  message("Using plot_dimred")
  plot_dimred(x)
}

#' @export
`plot.dynwrap::with_trajectory` <- function(x, ...) {
  message("Using plot_graph")
  plot_graph(x)
}
