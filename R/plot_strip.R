#' @export
plot_strip <- function(task1, task2, margin=0.2) {
  task1$milestone_network <- task1$milestone_network %>%
    mutate(
      cumstart = c(0, cumsum(length)[-length(length)]) + margin * (seq_len(n())-1),
      cumend = c(cumsum(length)) + margin * (seq_len(n())-1)
    )
  task2$milestone_network <- task2$milestone_network %>%
    mutate(
      cumstart = c(0, cumsum(length)[-length(length)]) + margin * (seq_len(n())-1),
      cumend = c(cumsum(length)) + margin * (seq_len(n())-1)
    )

  prog1 <- task1$progression %>% left_join(task1$milestone_network, by=c("from", "to")) %>% mutate(cumpercentage=percentage*length + cumstart) %>% rename_at(vars(-cell_id), ~paste0(., 1))
  prog2 <- task2$progression %>% left_join(task2$milestone_network, by=c("from", "to")) %>% mutate(cumpercentage=percentage*length + cumstart) %>% rename_at(vars(-cell_id), ~paste0(., 2))

  prog <- full_join(prog1, prog2, by=c("cell_id"))

  ggplot(prog) +
    geom_point(aes(cumpercentage1, cumpercentage2)) +
    geom_vline(aes(xintercept=cumstart), data=task1$milestone_network) +
    geom_vline(aes(xintercept=cumend), data=task1$milestone_network, linetype="dashed") +
    geom_hline(aes(yintercept=cumstart), data=task2$milestone_network) +
    geom_hline(aes(yintercept=cumend), data=task2$milestone_network, linetype="dashed") +
    ggtitle(paste0(task1$id, " -> ", task2$id)) +
    theme_clean()
}
