# milestone_network <- tribble(
#   ~from, ~to, ~length, ~directed,
#   "3", "2", 11.6, TRUE,
#   "2", "1", 5.45, TRUE,
#   "1", "4", 13.9, TRUE,
#   "1", "5", 14.5, TRUE
# )

permutations <- function(n){
  if (n == 1){
    matrix(1)
  } else {
    sp <- permutations(n-1)
    p <- nrow(sp)
    A <- matrix(nrow = n*p, ncol = n)
    for (i in seq_len(n)){
      A[(i-1)*p+1:p,] <- cbind(i, sp + (sp>=i))
    }
    A
  }
}


#' @importFrom GA ga
optimize_order <- function(milestone_network) {
  # the first state will be kept at the beginning

  n <- nrow(milestone_network)
  score_order <- function(ordered) {
    from <- milestone_network$from[c(1, ordered)]
    to <- milestone_network$to[c(1, ordered)]

    -sum(
      ((match(from, to) - seq_len(n) + 1)^2) %>% sum(na.rm = TRUE),
      ((match(to, from) - seq_len(n) - 1)^2) %>% sum(na.rm = TRUE)
    )
  }

  ordered <-
    if (n == 1) {
      1
    } else if (n > 4) {
      result <- GA::ga(
        type = "permutation",
        fitness = score_order,
        lower = 2,
        upper = n,
        maxiter = 30*nrow(milestone_network),
        popSize = 20,
        maxFitness = 0,
        elitism = 5
      )
      ordered <- c(1, result@solution[1, ])
    } else {
      comb <- permutations(n-1)+1
      scores <- apply(comb, 1, score_order)
      ordered <- c(1, comb[which.max(scores), , drop = TRUE])
    }

  milestone_network[ordered, ]
}


# will use the ordering of the first trajectory, to optimize the ordering of the second trajectory, maximizing the correlation between the two
map_order <- function(traj, rel_dataset) {

  # first get the cell cumulative percentage of the relative traj
  margin <- 0
  milestone_network <- rel_dataset$milestone_network %>%
    mutate(
      cumstart = c(0, cumsum(.data$length)[-length(.data$length)]) + margin * (seq_len(n())-1),
      cumend = c(cumsum(.data$length)) + margin * (seq_len(n())-1)
    )

  prog <-
    rel_dataset$progression %>%
    left_join(milestone_network, by = c("from", "to")) %>%
    mutate(cumpercentage = .data$percentage * .data$length + .data$cumstart)

  # use these cumulative percentages to find the optimal ordering of the traj of interest, by calculating the mean relative cumulative percentage, and then ordering the milestone_network along this measure
  milestone_network_ordered <-
    traj$progressions %>%
    left_join(
      prog %>%
        group_by(.data$cell_id) %>%
        summarise(mincumpercentage = min(.data$cumpercentage)),
      by = "cell_id"
    ) %>%
    group_by(.data$from, .data$to) %>%
    summarise(mean_mincumpercentage = mean(.data$mincumpercentage))

  # add missing milestone edges (without any cells)
  milestone_network_ordered <-
    milestone_network_ordered %>%
    right_join(traj$milestone_network, by = c("from", "to")) %>%
    mutate(mean_mincumpercentage = ifelse(is.na(.data$mean_mincumpercentage), Inf, .data$mean_mincumpercentage))

  milestone_network_ordered %>%
    arrange(.data$mean_mincumpercentage) %>%
    select(.data$from, .data$to, .data$length, .data$directed) %>%
    ungroup()
}
