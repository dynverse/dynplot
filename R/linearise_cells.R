#' Prepare a trajectory for linearised visualisation.
#'
#' This is an internal function and should probably not be used manually.
#'
#' @param trajectory A dynwrap trajectory.
#' @param margin A margin between trajectory segments.
#' @param no_margin_between_linear Whether to add a margin only when a branch occurs.
#' @param one_edge Whether or not to assign each cell to one cell only. This can occur when a
#'   cell is on a branching point, or in between multiple edges.
#' @param equal_cell_width Whether or not to space segments according to cell count.
#'
#' @returns A named list with values:
#'  * `milestone_network`: A linearised version of `trajectory$milestone_network` with extra columns: `add_margin`, `n_margins`, `cumstart`, `cumend`, `edge_id`.
#'  * `progressions`: A linearised version of `trajectory$progressions` with extra columns:
#'    `percentage2`, `length`, `directed`, `add_margin`, `n_margins`, `cumstart`, `cumend`, `edge_id`, `cumpercentage`.
#'  * `margin`: The used margin (numeric).
#'
#' @importFrom dplyr lead
#'
#' @examples
#' linearise_cells(example_bifurcating)
#'
#' @export
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
      group_by(.data$from, .data$to) %>%
      mutate(percentage2 = .data$percentage + stats::runif(n(), 0, 1e-6)) %>% # randomly position cells at same position
      mutate(percentage2 = (rank(.data$percentage2)-1)/n())

    milestone_network <-
      left_join(
        milestone_network %>% select(-.data$length),
        progressions %>%
          group_by(.data$from, .data$to) %>%
          summarise(length = n(), .groups = "drop"),
        by = c("from", "to")
      ) %>%
      mutate(length = ifelse(is.na(.data$length), 1e-6, .data$length)) %>% # add length of edges with no cells
      ungroup()
  } else {
    progressions$percentage2 <- progressions$percentage
  }

  margin_mult <- sum(milestone_network$length) * margin

  if (no_margin_between_linear) {
    # add margin only if froms not directly connected, or if to is a forking milestone, or if to is a converging milestone
    num_to <- table(c(milestone_network$to, trajectory$milestone_ids)) - 1
    num_from <- table(c(milestone_network$from, trajectory$milestone_ids)) - 1
    milestone_network$add_margin <- unname(
      (milestone_network$to != lead(milestone_network$from, default = "")) |
      (num_from[milestone_network$to] > 1) |
      (num_to[milestone_network$to] > 1)
    )
  } else {
    milestone_network$add_margin <- TRUE
  }

  milestone_network$n_margins <- unname(c(0, cumsum(milestone_network$add_margin)[-nrow(milestone_network)]))

  milestone_network <- milestone_network %>%
    mutate(
      cumstart = lag(cumsum(.data$length), default = 0) + .data$n_margins * margin_mult,
      cumend = .data$cumstart + .data$length,
      edge_id = factor(seq_len(n()))
    )

  progressions <- progressions %>%
    left_join(milestone_network, by = c("from", "to")) %>%
    mutate(cumpercentage = .data$cumstart + .data$percentage2 * .data$length)

  lst(milestone_network, progressions, margin = margin_mult)
}


# Put every cell on only one edge
progressions_one_edge <- function(progressions) {
  progressions %>%
    group_by(.data$cell_id) %>%
    arrange(-.data$percentage) %>%
    filter(row_number() == 1) %>%
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
