#' Linearize a trajectory
#'
#' @param milestone_network The milestone network
#' @param progressions The progressions
#' @param margin The margin to add
#' @param one_edge If TRUE, assigns each cell to one edge only
linearize_cells <- function(milestone_network, progressions, margin=0.05, one_edge=FALSE) {
  margin <- sum(milestone_network$length) * margin

  milestone_network <- milestone_network %>%
    mutate(
      cumstart = c(0, cumsum(length)[-n()]) + margin * (seq_len(n())-1),
      cumend = c(cumsum(length)) + margin * (seq_len(n())-1),
      edge_id = factor(seq_len(n()))
    )

  if(one_edge) {
    progressions <- progressions %>%
      group_by(cell_id) %>%
      arrange(-percentage) %>%
      filter(row_number() == 1) %>%
      ungroup()
  }

  progressions <- progressions %>%
    left_join(milestone_network, by=c("from", "to")) %>%
    mutate(cumpercentage = cumstart + percentage * (cumend-cumstart))

  lst(milestone_network, progressions)
}
