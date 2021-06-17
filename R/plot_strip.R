#' Plot strip
#'
#' @param traj1 The first trajectory
#' @param traj2 The second traj
#' @param reorder Whether to reorder
#'
#' @keywords compare_trajectory
#'
#' @inheritParams linearise_cells
#'
#' @returns A scatterplot comparison ggplot of two linearised trajectories.
#'
#' @export
#'
#' @examples
#' data(example_bifurcating)
#' plot_strip(example_bifurcating, example_bifurcating)
plot_strip <- function(traj1, traj2, margin = 0.05, reorder = TRUE) {
  if (reorder) {
    traj1$milestone_network <- optimize_order(traj1$milestone_network)
    traj2$milestone_network <- map_order(traj2, traj1)
  }

  linearised1 <- linearise_cells(traj1, margin)
  milestone_network1 <- linearised1$milestone_network
  prog1 <- linearised1$progressions %>% rename_at(vars(-.data$cell_id), ~paste0(., 1))

  linearised2 <- linearise_cells(traj2, margin)
  milestone_network2 <- linearised2$milestone_network
  prog2 <- linearised2$progressions %>% rename_at(vars(-.data$cell_id), ~paste0(., 2))

  prog <- full_join(prog1, prog2, by = c("cell_id"))

  ymax <- max(milestone_network2$cumend)
  xmax <- max(milestone_network1$cumend)

  ggplot(prog) +
    geom_rect(aes(xmin = .data$cumstart, xmax = .data$cumend, ymin = 0, ymax = ymax), data = milestone_network1, alpha = 0.1) +
    geom_rect(aes(ymin = .data$cumstart, ymax = .data$cumend, xmin = 0, xmax = xmax), data = milestone_network2, alpha = 0.1) +
    geom_point(aes(.data$cumpercentage1, .data$cumpercentage2)) +
    geom_vline(aes(xintercept = .data$cumstart), data = milestone_network1, alpha = 0.5) +
    geom_vline(aes(xintercept = .data$cumend), data = milestone_network1, linetype = "dashed", alpha = 0.5) +
    geom_hline(aes(yintercept = .data$cumstart), data = milestone_network2, alpha = 0.5) +
    geom_hline(aes(yintercept = .data$cumend), data = milestone_network2, linetype = "dashed", alpha = 0.5) +
    ggtitle(paste0(traj1$id, " -> ", traj2$id)) +
    theme_clean()
}
