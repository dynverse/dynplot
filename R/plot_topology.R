#' Plotting the topology of a trajectory
#'
#' @param task The task
#' @inheritParams add_milestone_coloring
#' @export
plot_topology <- dynutils::inherit_default_params(
  list(add_milestone_coloring),
  function(
  task,
  color_milestones,
  milestones
  ) {
    milestone_graph <- as_tbl_graph(task$milestone_network)
    milestone_positions <- milestone_graph %>%
      create_layout("tree") %>%
      mutate(milestone_id = name)
    if(!is.null(milestones)) {
      milestone_positions <- left_join(milestone_positions, milestones, "milestone_id")
    }
    milestone_positions <- add_milestone_coloring(milestone_positions, color_milestones)

    milestone_graph <- tbl_graph(milestone_positions, task$milestone_network)

    ggraph(milestone_graph, "manual", node.positions=milestone_positions) +
      geom_edge_fan() +
      geom_edge_fan(aes(xend = x + (xend-x)/1.5, yend = y + (yend-y)/1.5), arrow=arrow(type="closed", length = unit(0.4, "cm"))) +
      geom_node_label(aes(fill=color, label=milestone_id)) +
      scale_fill_identity() +
      theme_graph()
  }
)
