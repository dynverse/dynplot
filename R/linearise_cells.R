#' Linearise a trajectory
#'
#' @param milestone_network The milestone network
#' @param progressions The progressions
#' @param margin The margin to add
#' @param one_edge If TRUE, assigns each cell to one edge only
#' @param equal_cell_width if TRUE, will give each cell equal width
linearise_cells <- function(milestone_network, progressions, margin=0.05, one_edge=FALSE, equal_cell_width=FALSE) {
  if(one_edge | equal_cell_width) {
    progressions <- progressions %>%
      group_by(cell_id) %>%
      arrange(-percentage) %>%
      filter(row_number() == 1) %>%
      ungroup()
  }

  margin <- sum(milestone_network$length) * margin

  milestone_network <- milestone_network %>%
    mutate(
      cumstart = c(0, cumsum(length)[-n()]) + margin * (seq_len(n())-1),
      cumend = c(cumsum(length)) + margin * (seq_len(n())-1),
      edge_id = factor(seq_len(n()))
    )

  if (equal_cell_width) {
    progressions <- progressions %>%
      group_by(from, to) %>%
      mutate(percentage2 = percentage + runif(n(), 0, 1e-6)) %>%
      mutate(position = rank(percentage2)-1)

    milestone_network <- progressions %>%
      group_by(from, to) %>%
      summarise(length=max(position)) %>%
      left_join(milestone_network %>% select(-length), c("from", "to")) %>%
      ungroup()

    progressions <- progressions %>%
      left_join(milestone_network, by=c("from", "to")) %>%
      mutate(cumpercentage = cumstart + position * length)
  } else {
    progressions$position <- progressions$percentage

    progressions <- progressions %>%
      left_join(milestone_network, by=c("from", "to")) %>%
      mutate(cumpercentage = cumstart + position * length)
  }

  lst(milestone_network, progressions)
}
