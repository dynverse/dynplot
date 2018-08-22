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
      min = rep(1, n-1),
      max = rep(n-1, n-1),
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
