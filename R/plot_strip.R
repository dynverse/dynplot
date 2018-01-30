#' Plot strip
#'
#' @param task1 The first task
#' @param task2 The second task
#' @param margin The margin to add
#' @param reorder ?? TODO: Zouter/wouters
#'
#' @export
plot_strip <- function(task1, task2, margin=0.05, reorder = TRUE) {
  if (reorder) {
    task1$milestone_network <- optimize_order(task1$milestone_network)
    task2$milestone_network <- map_order(task2, task1)
  }

  linearized1 <- linearize_cells(task1$milestone_network, task1$progression, margin)
  milestone_network1 <- linearized1$milestone_network
  prog1 <- linearized1$progressions %>% rename_at(vars(-cell_id), ~paste0(., 1))

  linearized2 <- linearize_cells(task2$milestone_network, task2$progression, margin)
  milestone_network2 <- linearized2$milestone_network
  prog2 <- linearized2$progressions %>% rename_at(vars(-cell_id), ~paste0(., 2))

  prog <- full_join(prog1, prog2, by=c("cell_id"))

  ymax <- max(milestone_network2$cumend)
  xmax <- max(milestone_network1$cumend)

  ggplot(prog) +
    geom_rect(aes(xmin=cumstart, xmax=cumend, ymin=0, ymax=ymax), data=milestone_network1, alpha=0.1) +
    geom_rect(aes(ymin=cumstart, ymax=cumend, xmin=0, xmax=xmax), data=milestone_network2, alpha=0.1) +
    geom_point(aes(cumpercentage1, cumpercentage2)) +
    geom_vline(aes(xintercept=cumstart), data=milestone_network1, alpha=0.5) +
    geom_vline(aes(xintercept=cumend), data=milestone_network1, linetype="dashed", alpha=0.5) +
    geom_hline(aes(yintercept=cumstart), data=milestone_network2, alpha=0.5) +
    geom_hline(aes(yintercept=cumend), data=milestone_network2, linetype="dashed", alpha=0.5) +
    ggtitle(paste0(task1$id, " -> ", task2$id)) +
    theme_clean()
}
