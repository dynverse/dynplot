#' Plot strip onedim
#'
#' @param task1 The first task
#' @param task2 The second task
#' @param reorder ?? TODO: Zouter/wouters
#' @param margin The margin to add
#' @param reorder_second_by ?? TODO: Zouter/wouters
#'
#' @export
#' @importFrom cowplot plot_grid
#' @export
plot_strip_onedim <- function(task1, task2, reorder=TRUE, margin=0.05, reorder_second_by="mapping") {
  if (reorder) {
    # make sure the order of the milestone_networks stay the same between the connection plots and the strip plots, therefore we already sort them here
    task1$milestone_network <- optimize_order(task1$milestone_network)
    if(reorder_second_by == "mapping") {
      task2$milestone_network <- map_order(task2, task1)
    } else if (reorder_second_by == "optimization") {
      task2$milestone_network <- optimize_order(task2$milestone_network)
    }
  }

  empty_max <- function(x) {if(length(x) > 0) {max(x)} else {0}}

  strip_plot <- plot_strip(task1, task2, reorder = FALSE, margin=margin)
  onedim_plotdata1 <- make_connection_plotdata(task1$milestone_network, margin=margin)
  onedim_plotdata2 <- make_connection_plotdata(task2$milestone_network, margin=margin)

  onedim_plot1 <- plot_onedim(task1, orientation = -1, plotdata=onedim_plotdata1)
  onedim_plot2 <- plot_onedim(task2, orientation = -1, plotdata=onedim_plotdata2) + coord_flip()

  size1 <- empty_max(onedim_plotdata1$onedim$level) + 1
  size2 <- empty_max(onedim_plotdata2$onedim$level) + 1

  cowplot::plot_grid(onedim_plot2, strip_plot, ggplot() + theme_clean(), onedim_plot1, align="hv", axis="tblr", rel_widths=c(size2, 10), rel_heights=c(10, size1))
}
