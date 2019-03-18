#' Plotting the topology of a trajectory
#'
#' @inheritParams add_cell_coloring
#' @inheritParams add_milestone_coloring
#' @inheritParams ggraph::ggraph
#'
#' @keywords plot_trajectory
#'
#' @export
plot_topology <- dynutils::inherit_default_params(
  list(add_milestone_coloring),
  function(
  traj,
  color_milestones,
  milestones,
  layout = NULL
  ) {
    # make sure a trajectory was provided
    testthat::expect_true(dynwrap::is_wrapper_with_trajectory(traj))

    # determine optimal layout
    if (is.null(layout)) {
      gr <- traj$milestone_network %>% igraph::graph_from_data_frame()
      if (igraph::girth(gr)$girth > 0) {
        # cyclic
        layout <- "kk"
      } else {
        # tree
        if (is.null(traj$root_milestone_id)) {
          traj <- traj %>% add_root()
        }
        layout <- "tree"
      }
    }

    milestone_graph <- as_tbl_graph(traj$milestone_network)
    milestone_positions <- milestone_graph %>%
      create_layout(layout) %>%
      mutate(milestone_id = as.character(name))

    # check milestones, make sure it's a data_frame
    milestones <- check_milestones(traj, milestones) %>% add_milestone_coloring(color_milestones)
    milestone_positions <- left_join(milestone_positions, milestones, "milestone_id")

    milestone_graph <- igraph::graph_from_data_frame(traj$milestone_network, vertices = milestone_positions %>% select(-x, -y)) %>% as_tbl_graph()

    arrow <-
      if (any(traj$milestone_network$directed)) {
        arrow(type = "closed", length = unit(0.4, "cm"))
      } else {
        NULL
      }

    ggraph(milestone_graph, "manual", node.positions = milestone_positions) +
      geom_edge_fan() +
      geom_edge_fan(aes(xend = x + (xend-x)/1.5, yend = y + (yend-y)/1.5), arrow = arrow) +
      geom_node_label(aes(fill = color, label = milestone_id)) +
      scale_fill_identity() +
      theme_graph()
  }
)
