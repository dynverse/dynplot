#' Order the cells according to their progression,
#'  assign tented cells to the highest percentage
#'
#' @param milestone_network The milestone network
#' @param progressions The progressions
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

  ordered_progression %>% select(cell_id, edge_id) %>% ungroup() %>%  mutate(position=seq_len(n()), edge_id=factor(edge_id))
}

#' Plot the task as a heatmap
#'
#' @param task The task
#'
#' @importFrom pheatmap pheatmap
#' @export
plot_heatmap <- function(task) {
  milestone_network <- optimize_order(task$milestone_network)
  cell_order <- order_cells(milestone_network, task$progressions)
  counts <- task$counts[cell_order$cell_id, ]
  gaps <- which(diff(cell_order$edge_id) != 0)
  pheatmap::pheatmap(t(counts), gaps_col = gaps, cluster_col=FALSE)
}
