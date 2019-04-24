# Trajectory linearisation ------------------------------------------------

#' Linearise a trajectory
#'
#' @param trajectory The trajectory
#' @param margin The margin to add
#' @param no_margin_between_linear Whether to remove the margin if a milestone does not split or converge
#' @param one_edge Whether to assign each cell to an edge only once. Only relevant in case of divergence regions
#' @param equal_cell_width Whether to give each cell an equal width. Useful when plotting heatmaps.
linearise_trajectory <- function(
  trajectory,
  margin = 0.05,
  no_margin_between_linear = TRUE,
  one_edge = TRUE,
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
      mutate(percentage = (rank(percentage, ties.method = "random")-1)/n()) %>%
      ungroup()

    milestone_network <- progressions %>%
      group_by(from, to) %>%
      summarise(length = n()) %>%
      right_join(milestone_network %>% select(-length), c("from", "to")) %>%
      mutate(length = ifelse(is.na(length), 0, length)) %>% # add length of edges with no cells
      ungroup()
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
    mutate(cumpercentage = cumstart + percentage * length)

  lst(milestone_network, progressions, margin)

  milestone_positions <- bind_rows(
    milestone_network %>% select(milestone_id = from, comp_1 = cumstart) %>% mutate(type = "start"),
    milestone_network %>% select(milestone_id = to, comp_1 = cumend) %>% mutate(type = "end")
  ) %>%
    mutate(comp_2 = 0)

  edge_positions <- milestone_network %>%
    select(from, to, comp_1_from = cumstart, comp_1_to = cumend) %>%
    mutate(comp_2_from = 0, comp_2_to = 0)

  cell_positions <- progressions %>%
    select(cell_id, comp_1 = cumpercentage) %>%
    mutate(comp_2 = 0)

  lst(
    milestone_positions,
    edge_positions,
    cell_positions,
    margin = margin
  )
}


# Put every cell on the edge with the highest progression
# Only has an effect in case of divergence regions
progressions_one_edge <- function(progressions) {
  progressions %>%
    group_by(cell_id) %>%
    arrange(-percentage) %>%
    filter(dplyr::row_number() == 1) %>%
    ungroup()
}

# Check whether the order of the edges of the milestone network
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






# Optimise order ----------------------------------------------------------

#' @importFrom GA ga
optimize_order <- function(milestone_network) {
  # the first state will be kept at the beginning
  n <- nrow(milestone_network)
  if (n > 3) {
    score_order <- function(ordered) {
      from <- milestone_network$from[c(1, ordered+1)]
      to <- milestone_network$to[c(1, ordered+1)]

      -sum(
        ((match(from, to) - seq_len(n) + 1)^2) %>% sum(na.rm = TRUE),
        ((match(to, from) - seq_len(n) - 1)^2) %>% sum(na.rm = TRUE)
      )
    }

    result <- GA::ga(
      type = "permutation",
      score_order,
      lower = 1,
      upper = n - 1,
      maxiter = 30*nrow(milestone_network),
      popSize = 20,
      maxFitness = 0,
      elitism = 5
    )
    ordered <- result@solution[1, ]

    milestone_network[c(1, ordered+1), ]
  } else {
    milestone_network
  }
}


# will use the ordering of the first trajectory, to optimize the ordering of the second trajectory, maximizing the correlation between the two
map_order <- function(traj, rel_dataset) {

  # first get the cell cumulative percentage of the relative traj
  margin <- 0
  milestone_network <- rel_dataset$milestone_network %>%
    mutate(
      cumstart = c(0, cumsum(length)[-length(length)]) + margin * (seq_len(n())-1),
      cumend = c(cumsum(length)) + margin * (seq_len(n())-1)
    )

  prog <- rel_dataset$progression %>% left_join(milestone_network, by = c("from", "to")) %>% mutate(cumpercentage = percentage*length + cumstart)

  # use these cumulative percentages to find the optimal ordering of the traj of interest, by calculating the mean relative cumulative percentage, and then ordering the milestone_network along this measure
  milestone_network_ordered <- traj$progressions %>%
    left_join(
      prog %>%
        group_by(cell_id) %>%
        summarise(mincumpercentage = min(cumpercentage)),
      by = "cell_id") %>%
    group_by(from, to) %>%
    summarise(mean_mincumpercentage = mean(mincumpercentage))

  # add missing milestone edges (without any cells)
  milestone_network_ordered <- milestone_network_ordered %>%
    right_join(traj$milestone_network, by = c("from", "to")) %>%
    mutate(mean_mincumpercentage = ifelse(is.na(mean_mincumpercentage), Inf, mean_mincumpercentage))

  milestone_network_ordered %>% arrange(mean_mincumpercentage) %>% select(from, to, length) %>% ungroup()
}




# Connections between milestones ------------------------------------------

calculate_connections <- function(linearised) {
  # get all connections that are necessary
  # direct connections are those that are reachable without up
  connections <- crossing(
    linearised$edge_positions %>% select(from, comp_1_from),
    linearised$edge_positions %>% select(to, comp_1_to)
  ) %>% filter(
    from == to,
    comp_1_from != comp_1_to
  ) %>% mutate(
    comp_1_diff = abs(comp_1_to-comp_1_from)
  ) %>%
    arrange(comp_1_diff) %>%
    select(milestone_id = from, comp_1_from, comp_1_to, comp_1_diff) %>%
    mutate(
      level = NA,
      direct = near(comp_1_diff, linearised$margin)
    )

  for (i in seq_len(nrow(connections))) {
    connection <- connections %>% extract_row_to_list(i)

    overlapping_connections <- connections %>%
      filter(
        dplyr::row_number() < i,
        pmax(comp_1_from, comp_1_to) > min(connection$comp_1_from, connection$comp_1_to),
        pmin(comp_1_from, comp_1_to) < max(connection$comp_1_from, connection$comp_1_to)
      )

    if (nrow(overlapping_connections)) {
      connections$level[i] <- max(overlapping_connections$level) + 1
    } else {
      if (connections$direct[i]) {
        connections$level[i] <- 0
      } else {
        connections$level[i] <- 1
      }
    }
  }

  # calculate connection positions
  connections_direct <- connections %>% filter(direct)
  connections_indirect <- connections %>% filter(!direct)

  connection_positions <- bind_rows(
    connections_direct %>% mutate(connection_ix = 1, comp_2_from = 0, comp_2_to = 0),
    connections_indirect %>% mutate(comp_1_to = comp_1_from, comp_2_from = 0, comp_2_to = level, connection_ix = 1),
    connections_indirect %>% mutate(comp_2_from = level, comp_2_to = level, connection_ix = 2),
    connections_indirect %>% mutate(comp_1_from = comp_1_to, comp_2_from = level, comp_2_to = 0, connection_ix = 3)
  )

  connection_positions
}


