#' Plotting the genes
#'
#' @param task The task
#' @param genes_oi The genes of interest
#' @param margin The margin to add
#'
#' @export
plot_genes <- function(task, genes_oi=sample(colnames(task$counts), min(c(5, ncol(task$counts)))), margin=0.05) {
  counts <- task$counts %>% as.data.frame() %>%
    select(one_of(genes_oi)) %>%
    tibble::rownames_to_column("cell_id") %>%
    gather("gene_id", "expression", -cell_id)

  milestone_network <- optimize_order(task$milestone_network)
  linearised <- linearise_cells(milestone_network, task$progressions, one_edge = TRUE, margin=margin)
  prog <- linearised$progressions
  milestone_network <- linearised$milestone_network

  plotdata <- counts %>%
    left_join(task$prior_information$grouping_assignment, by="cell_id") %>%
    left_join(prog, by="cell_id")

  expression_plot <- ggplot(plotdata, aes(cumpercentage, expression, color=edge_id)) +
    geom_point() +
    geom_smooth() +
    facet_grid(gene_id~.) +
    geom_vline(aes(xintercept=cumstart), data=milestone_network, alpha=0.2) +
    geom_vline(aes(xintercept=cumend), data=milestone_network, alpha=0.2) +
    cowplot::theme_cowplot() +
    scale_x_continuous(NULL, breaks=NULL) +
    theme(legend.position="none", panel.border = element_rect(color = "black", size=0.5, linetype="solid"), panel.background = element_blank())

  onedim_plot <- plot_onedim(milestone_network, orientation = -1, margin=margin)

  cowplot::plot_grid(plotlist=list(expression_plot, onedim_plot), ncol=1, align = "v", axis="lr", rel_heights = c(length(genes_oi), 1))

}
