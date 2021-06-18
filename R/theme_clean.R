#' We like our plots clean
#'
#' @keywords plot_helpers
#'
#' @export
#'
#' @returns A ggplot2 theme.
#'
#' @examples
#' data(example_bifurcating)
#' g <- plot_dimred(example_bifurcating)
#' g + theme_clean()
theme_clean <- function() {
  theme_bw() +
  theme(
    strip.text = element_text(size = rel(0.8)),
    plot.margin = unit(c(0, 0, 0, 0), "lines")
  )
}

#' We like our plots clean
#'
#' @keywords plot_helpers
#'
#' @export
#'
#' @returns A ggplot2 theme.
#'
#' @examples
#' data(example_bifurcating)
#' g <- plot_dimred(example_bifurcating)
#' g + theme_graph()
theme_graph <- function() {
  theme_void()
}


#' Create an empty plot for spacing
#'
#' @keywords plot_helpers
#'
#' @export
#'
#' @returns An empty ggplot2.
#'
#' @examples
#' empty_plot()
empty_plot <- function() {
  ggplot(tibble(x = character())) + geom_point(aes_string("x", "x")) + theme_graph()
}


new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}
