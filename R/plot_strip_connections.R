#' @import dplyr ggplot2 purrr
#' @export
plot_strip_connections <- function(task1, task2, reorder=TRUE, margin=0.1) {
  if (reorder) {
    # make sure the order of the milestone_networks stay the same between the connection plots and the strip plots, therefore we already sort them here
    task1$milestone_network <- optimize_order(task1$milestone_network)
    task2$milestone_network <- optimize_order(task2$milestone_network)
  }

  empty_max <- function(x) {if(length(x) > 0) {max(x)} else {0}}

  strip_plot <- plot_strip(task1, task2, reorder = FALSE, margin=margin)
  connections_plotdata1 <- make_connection_plotdata(task1$milestone_network, margin=margin)
  connections_plotdata2 <- make_connection_plotdata(task2$milestone_network, margin=margin)

  connections_plot1 <- plot_connections(task1$milestone_network, orientation = -1, plotdata=connections_plotdata1)
  connections_plot2 <- plot_connections(task2$milestone_network, orientation = -1, plotdata=connections_plotdata2) + coord_flip()

  size1 <- empty_max(connections_plotdata1$connections$level) + 1
  size2 <- empty_max(connections_plotdata2$connections$level) + 1

  cowplot::plot_grid(connections_plot2, strip_plot, ggplot() + theme_clean(), connections_plot1, align="hv", axis="tblr", rel_widths=c(size2, 10), rel_heights=c(10, size1))
}
