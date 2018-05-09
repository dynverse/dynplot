#' Plot strip onedim
#'
#' @param traj1 The first traj
#' @param traj2 The second traj
#' @param reorder ?? TODO: Zouter/wouters
#' @param margin The margin to add
#' @param reorder_second_by ?? TODO: Zouter/wouters
#'
#' @export
#' @importFrom patchwork wrap_plots
#' @export
plot_linearised_comparison <- function(traj1, traj2, reorder=TRUE, margin=0.05, reorder_second_by="mapping") {
  if (reorder) {
    # make sure the order of the milestone_networks stay the same between the connection plots and the strip plots, therefore we already sort them here
    traj1$milestone_network <- optimize_order(traj1$milestone_network)
    if(reorder_second_by == "mapping") {
      traj2$milestone_network <- map_order(traj2, traj1)
    } else if (reorder_second_by == "optimization") {
      traj2$milestone_network <- optimize_order(traj2$milestone_network)
    }
  }

  empty_max <- function(x) {if(length(x) > 0) {max(x)} else {0}}

  strip_plot <- plot_strip(traj1, traj2, reorder = FALSE, margin=margin)

  onedim_plot1 <- plot_onedim(traj1, orientation = -1)
  onedim_plot2 <- plot_onedim(traj2, orientation = -1) + coord_flip()

  patchwork::wrap_plots(
    onedim_plot2,
    strip_plot,
    empty_plot(),
    onedim_plot1,
    ncol = 2,
    widths = c(1, 10),
    heights = c(10, 1)
  )
}
