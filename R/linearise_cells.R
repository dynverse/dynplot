#' Linearise a trajectory
#'
#' @param trajectory The trajectory
#' @param margin The margin to add
#' @param no_margin_between_linear Whether to add a margin only when
#' @param one_edge If TRUE, assigns each cell to one edge only
#' @param equal_cell_width if TRUE, will give each cell equal width
linearise_cells <- function(
  trajectory,
  margin = 0.05,
  no_margin_between_linear = TRUE,
  one_edge = FALSE,
  equal_cell_width = FALSE
) {
  if (!is_rooted_milestone_network(trajectory)) {
    trajectory <- trajectory %>% add_root()
  }

  milestone_network <- trajectory$milestone_network
  progressions <- trajectory$progressions

  if (one_edge | equal_cell_width) {
    progressions <- progressions_one_edge(progressions)
  }

  if (equal_cell_width) {
    progressions <- progressions %>%
      group_by(from, to) %>%
      mutate(percentage2 = percentage + runif(n(), 0, 1e-6)) %>% # randomly position cells at same position
      mutate(percentage2 = (rank(percentage2)-1)/n())

    milestone_network <- progressions %>%
      group_by(from, to) %>%
      summarise(length = n()) %>%
      right_join(milestone_network %>% select(-length), c("from", "to")) %>%
      mutate(length = ifelse(is.na(length), 1e-6, length)) %>% # add length of edges with no cells
      ungroup()
  } else {
    progressions$percentage2 <- progressions$percentage
  }

  margin <- sum(milestone_network$length) * margin

  if (no_margin_between_linear) {
    # add margin only if froms not directly connected, or if to is a forking milestone, or if to is a converging milestone
    milestone_network$add_margin <- (milestone_network$to != lead(milestone_network$from, default = "")) |
      (table(milestone_network$from)[milestone_network$to] > 1) |
      (table(milestone_network$to)[milestone_network$to] > 1)
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
    left_join(milestone_network, by = c("from", "to")) %>%
    mutate(cumpercentage = cumstart + percentage2 * length)

  lst(milestone_network, progressions, margin)
}


# Put every cell on only one edge
progressions_one_edge <- function(progressions) {
  progressions %>%
    group_by(cell_id) %>%
    arrange(percentage) %>%
    filter(dplyr::row_number() == 1) %>%
    ungroup()
}

# check whether the order of the edges of the milestone network
# is such that you can easily linearise the edges
is_rooted_milestone_network <- function(trajectory) {
  # # does not work yet, e.g.:
  # # tribble(from = c("a", "b", "d", "c"), to = c("b", "c", "b", "d"))
  # tibble(
  #   milestone_id = unique(c(milestone_network$from, milestone_network$to)),
  #   from = match(milestone_id, milestone_network$from),
  #   to = match(milestone_id, milestone_network$to),
  #   ok = is.na(from) | is.na(to) | from >= to
  # ) %>%
  #   pull(ok) %>%
  #   all()
  "root_milestone_id" %in% names(trajectory)
}
