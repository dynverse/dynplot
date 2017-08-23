optimize_order <- function(milestone_network) {
  # the first state will be kept at the beginning
  n <- nrow(milestone_network)
  if (n > 3) {
    score_order <- function(ordered) {
      from <- milestone_network$from[c(1, ordered+1)]
      to <- milestone_network$to[c(1, ordered+1)]

      -sum(
        ((match(from, to) - seq_len(n) + 1)^2) %>% sum(na.rm=TRUE),
        ((match(to, from) - seq_len(n) - 1)^2) %>% sum(na.rm=TRUE)
      )
    }

    result <- GA::ga(type="permutation", score_order, min=rep(1, n-1), max=rep(n-1, n-1), maxiter=30*nrow(milestone_network), popSize=20, maxFitness = 0, elitism=5)
    ordered <- result@solution[1, ]

    milestone_network[c(1, ordered+1), ]
  } else {
    milestone_network
  }
}
