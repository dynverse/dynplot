#' Linearise a trajectory
#'
#' @param milestone_network The milestone network
#' @param progressions The progressions
#' @param margin The margin to add
#' @param no_margin_between_linear Whether to add a margin only when
#' @param one_edge If TRUE, assigns each cell to one edge only
#' @param equal_cell_width if TRUE, will give each cell equal width
linearise_cells <- function(milestone_network, progressions, margin=0.05, no_margin_between_linear = TRUE, one_edge=FALSE, equal_cell_width=FALSE) {
  if(one_edge | equal_cell_width) {
    progressions <- progressions_one_edge(progressions)
  }

  if (equal_cell_width) {
    progressions <- progressions %>%
      group_by(from, to) %>%
      mutate(percentage2 = percentage + runif(n(), 0, 1e-6)) %>%
      mutate(percentage2 = (rank(percentage2)-1)/n())

    milestone_network <- progressions %>%
      group_by(from, to) %>%
      summarise(length=n()) %>%
      right_join(milestone_network %>% select(-length), c("from", "to")) %>%
      ungroup()
  } else {
    progressions$percentage2 <- progressions$percentage
  }

  margin <- sum(milestone_network$length) * margin

  if (no_margin_between_linear) {
    milestone_network$add_margin <- milestone_network$to != lead(milestone_network$from, default="")
  } else {
    milestone_network$add_margin <- TRUE
  }

  milestone_network$n_margins <- c(0, cumsum(milestone_network$add_margin)[-nrow(milestone_network)])

  milestone_network <- milestone_network %>%
    mutate(
      cumstart = c(0, cumsum(length)[-n()]) + n_margins * margin,
      cumend = cumstart + length,
      edge_id = factor(seq_len(n()))
    )

  progressions <- progressions %>%
    left_join(milestone_network, by=c("from", "to")) %>%
    mutate(cumpercentage = cumstart + percentage2 * length)

  lst(milestone_network, progressions)
}


# Put every cell on only one edge
progressions_one_edge <- function(progressions) {
  progressions %>%
    group_by(cell_id) %>%
    arrange(-percentage) %>%
    filter(row_number() == 1) %>%
    ungroup()
}
