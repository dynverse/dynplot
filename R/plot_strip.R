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

  margin1 <- sum(task1$milestone_network$length) * margin
  margin2 <- sum(task2$milestone_network$length) * margin

  milestone_network1 <- task1$milestone_network %>%
    mutate(
      cumstart = c(0, cumsum(length)[-length(length)]) + margin1 * (seq_len(n())-1),
      cumend = c(cumsum(length)) + margin1 * (seq_len(n())-1)
    )
  milestone_network2 <- task2$milestone_network %>%
    mutate(
      cumstart = c(0, cumsum(length)[-length(length)]) + margin2 * (seq_len(n())-1),
      cumend = c(cumsum(length)) + margin2 * (seq_len(n())-1)
    )

  prog1 <- task1$progression %>% left_join(milestone_network1, by=c("from", "to")) %>% mutate(cumpercentage=percentage*length + cumstart) %>% rename_at(vars(-cell_id), ~paste0(., 1))
  prog2 <- task2$progression %>% left_join(milestone_network2, by=c("from", "to")) %>% mutate(cumpercentage=percentage*length + cumstart) %>% rename_at(vars(-cell_id), ~paste0(., 2))

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
