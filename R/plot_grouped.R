plot_grouped <- dynutils::inherit_default_params(
  list(add_milestone_coloring),
  function(
    task,
    color_milestones,
    grouping_assignment = task$milestone_percentages %>% group_by(cell_id) %>% arrange(desc(percentage)) %>% filter(row_number() == 1) %>% select(-percentage) %>% rename(group_id = milestone_id),
    groups = tibble(group_id = task$milestone_ids),
    order_cells = c("auto", "pseudotime", "gene"),
    pseudotime=NULL,
    gene_oi=NULL,
    expression_source = "expression"
  ) {
    order_cells <- match.arg(order_cells)
    if(order_cells == "auto") {
      if(!is.null(gene_oi)) {
        order_cells <- "gene"
      } else {
        order_cells <- "pseudotime"
      }
    }

    if(order_cells == "pseudotime") {
      task <- check_pseudotime(task, pseudotime)
      cell_positions <- tibble(cell_id = names(task$pseudotime), y = task$pseudotime)
      y_scale <- scale_y_continuous("pseudotime")
    } else if (order_cells == "gene") {
      check_gene(task, gene_oi, expression_source)
      cell_positions <- tibble(cell_id = rownames(task[[expression_source]]), y = task[[expression_source]][, gene_oi])
      y_scale <- scale_y_continuous(paste0(gene_oi, " ", expression_source))
    }

    cell_positions <- left_join(cell_positions, grouping_assignment, "cell_id")

    groups <- add_milestone_coloring(groups, color_milestones)

    ggplot(cell_positions, aes(group_id, y)) +
      ggbeeswarm::geom_quasirandom(aes(fill=group_id), shape=21) +
      scale_fill_manual("grouping", values=set_names(groups$color, groups$group_id)) +
      y_scale +
      theme_clean()
  }
)
