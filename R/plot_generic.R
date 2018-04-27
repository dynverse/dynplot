#' @export
`plot.dynwrap::with_expression` <- function(x, ...) {
  plot_dimred(x)
}

#' @export
`plot.dynwrap::with_trajectory` <- function(x, ...) {
  plot_graph(x)
}
