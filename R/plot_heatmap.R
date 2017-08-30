#' order the cells according to their progression, assign tented cells to the highest percentage
order_cells <- function(milestone_network, progressions) {
  milestone_network <- milestone_network %>%
    mutate(
      cumstart = c(0, cumsum(length)[-length(length)]),
      cumend = c(cumsum(length)),
      edge_id = seq_len(n())
    )
  filtered_progression <- progressions %>% # a cell can only be in one edge (maximum in tents)
    select(cell_id, from, to, percentage) %>%
    left_join(milestone_network, by=c("from", "to")) %>%
    group_by(cell_id) %>% arrange(-percentage) %>% filter(row_number() == 1)

  ordered_progression <- filtered_progression %>%
    mutate(cumpercentage=percentage*length + cumstart) %>%
    arrange(cumpercentage)

  lst(
    order = ordered_progression %>% pull(cell_id),
    edge_id = ordered_progression %>% pull(edge_id)
  )
}
