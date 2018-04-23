plot_grouped <- function(
  task,
  grouping_assignment = task$milestone_percentages %>% group_by(cell_id) %>% arrange(desc(percentage)) %>% filter(row_number() == 1) %>% select(-percentage),
  color_milestones,
  groups = tibble(group_id = task$milestone_ids),
  order_cells = c("pseudotime", "gene"),
  pseudotime=NULL,
  gene_oi=NULL,
  expression_source = "expression"
) {
  order_cells <- match.arg(order_cells)
  if(order_cells == "pseudotime") {
    task <- check_pseudotime(task, pseudotime)
    cell_positions <- tibble(cell_id = names(task$pseudotime), y = task$pseudotime)
    y_scale <- scale_y_continuous("pseudotime")
  } else if (order_cells == "gene") {
    check_gene(task, gene_oi, expression_source)
    cell_positions <- tibble(cell_id = rownames(task[[expression_source]]), y = task[[expression_source]][, gene_oi])
    y_scale <- scale_y_continuous(paste0(gene_oi, " ", expression_source))
  }

  cell_positions <- left_join(cell_positions, grouping_assigment, "cell_id")

  groups <- add_milestone_coloring(groups, color_milestones)

  ggplot(cell_positions, aes(group_id, y)) +
    geom_violin(aes(fill=group_id)) +
    scale_fill_manual("grouping", values=set_names(groups$color, groups$group_id)) +
    y_scale +
    theme_clean()

  plot <- left_join(grouping_assignment, cell_linearisation$progressions, "cell_id") %>%
    left_join(groups, "group_id") %>%
    mutate(group_id = factor(group_id, rev(group_order))) %>%
    ggplot(aes(cumpercentage, group_id)) +
    ggbeeswarm::geom_quasirandom(aes(color = color), groupOnX=F) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_y_discrete("", labels=NULL) +
    scale_color_identity() +
    scale_x_continuous("Pseudotime", breaks = c(0, 1)) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

  if(label) {
    label_data <- left_join(grouping_assignment, cell_linearisation$progressions, "cell_id") %>%
      mutate(group_id = factor(group_id, group_order)) %>%
      group_by(group_id) %>%
      summarise(cumpercentage = mean(cumpercentage)) %>%
      left_join(groups, "group_id")

    plot <- plot +
      ggrepel::geom_label_repel(aes(label=group_id, fill=color), data=label_data, direction="x", force=0.) +
      scale_fill_identity()
  }

  plot
}
