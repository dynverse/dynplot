#' We like our plots clean
#'
#' @keywords plot_helpers
#'
#' @export
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
theme_graph <- function() {
  theme_void()
}


#' Create an empty plot for spacing
#'
#' @keywords plot_helpers
#'
#' @export
empty_plot <- function() {
  ggplot(tibble(x = character())) + geom_point(aes(x, x)) + theme_graph()
}
