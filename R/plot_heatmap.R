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

  ordered_progression %>% select(cell_id, edge_id) %>% ungroup() %>% mutate(position=seq_len(n()), edge_id=factor(edge_id))
}

#' Plot the task as a heatmap
#'
#' @param task The task
#' @param genes_oi Genes to plot
#' @param clust The clustering of the genes as a `clust` object
#' @param margin The margin to add
#'
#' @import tidygraph
#' @import ggraph
#' @export
plot_heatmap <- function(
  task,
  genes_oi = colnames(task$counts)[1:20],
  clust = hclust(as.dist(correlation_distance(t(task$counts[, genes_oi]))), method = "ward.D2"),
  margin = 0.02
) {
  linearised <- linearise_cells(task$milestone_network, task$progressions, equal_cell_width = TRUE, margin=margin)

  # get gene order
  gene_order <- colnames(task$counts[, genes_oi])[clust$order]

  # process counts
  counts <- dynutils::scale_quantile(task$counts[, genes_oi])
  molten <- counts %>%
    reshape2::melt(varnames=c("cell_id", "gene_id"), value.name="expression") %>%
    mutate_if(is.factor, as.character) %>%
    mutate(gene_id = as.numeric(factor(gene_id, gene_order))) %>%
    left_join(linearised$progressions, "cell_id")

  x_limits <- c(min(linearised$milestone_network$cumstart) - 1, max(linearised$milestone_network$cumend) + 1)

  heatmap <- ggplot(molten) +
    # geom_point(aes(cumpercentage, gene_id, color=expression)) +
    geom_tile(aes(cumpercentage, gene_id, fill=expression)) +
    scale_fill_distiller(palette = "RdBu") +
    scale_color_distiller(palette = "RdBu") +
    scale_x_continuous(NULL, breaks = NULL, expand=c(0, 0), limits=x_limits) +
    scale_y_continuous(NULL, expand=c(0, 0), breaks = seq_along(gene_order), labels=gene_order, position="left") +
    cowplot::theme_cowplot() +
    theme(legend.position="none")

  connections <- plot_connections(linearised$milestone_network, orientation = -1, margin=margin) + scale_x_continuous(expand=c(0, 0), limits=x_limits)

  dendrogram <- ggraph::ggraph(as.dendrogram(clust)) +
    ggraph::geom_node_point() +
    ggraph::geom_edge_elbow() +
    scale_x_continuous(expand=c(0, 0)) +
    scale_y_reverse() +
    coord_flip() +
    ggraph::theme_graph() +
    theme(plot.margin=margin())


  cowplot::plot_grid(
    dendrogram,
    heatmap,
    ggplot() + theme_void(),
    connections,
    ncol=2,
    align="hv",
    axis="lrtb",
    rel_heights = c(0.8, 0.2),
    rel_widths = c(0.2, 0.8)
  )
}
