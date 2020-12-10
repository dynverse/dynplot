#' Plot the topology of a trajectory
#'
#' @inheritParams add_cell_coloring
#' @inheritParams add_milestone_coloring
#' @inheritParams ggraph::ggraph
#' @param arrow The type and size of arrow in case of directed trajectories. Set to NULL to remove arrow altogether.
#'
#' @keywords plot_trajectory
#'
#' @export
#'
#' @examples
#' data(example_disconnected)
#' plot_topology(example_disconnected)
#'
#' data(example_tree)
#' plot_topology(example_tree)
plot_topology <- dynutils::inherit_default_params(
  list(add_milestone_coloring),
  function(
    trajectory,
    color_milestones,
    milestones,
    layout = NULL,
    arrow = grid::arrow(type = "closed", length = unit(0.4, "cm"))
  ) {
    # make sure a trajectory was provided
    assert_that(dynwrap::is_wrapper_with_trajectory(trajectory))

    # determine optimal layout
    if (is.null(layout)) {
      gr <- trajectory$milestone_network %>% igraph::graph_from_data_frame()
      if (igraph::girth(gr)$girth > 0) {
        # cyclic
        layout <- "kk"
      } else {
        # tree
        if (is.null(trajectory$root_milestone_id)) {
          trajectory <- trajectory %>% add_root()
        }
        layout <- "tree"
      }
    }

    milestone_graph <- as_tbl_graph(trajectory$milestone_network)
    milestone_positions <- milestone_graph %>%
      create_layout(layout) %>%
      mutate(milestone_id = as.character(name))

    # check milestones, make sure it's a data_frame
    milestones <- check_milestones(trajectory, milestones) %>% add_milestone_coloring(color_milestones)
    milestone_positions <- left_join(milestone_positions, milestones, "milestone_id")

    milestone_graph <- igraph::graph_from_data_frame(
      trajectory$milestone_network,
      vertices = milestone_positions %>% select(-x, -y)
    ) %>% as_tbl_graph()

    # add arrow if directed


    plot <- ggraph(milestone_graph, "manual", x = milestone_positions$x, y = milestone_positions$y) +
      geom_edge_fan()

    if (!is.null(arrow) && any(trajectory$milestone_network$directed)) {
      plot <- plot + geom_edge_fan(aes_string(xend = "x + (xend-x)/1.5", yend = "y + (yend-y)/1.5"), arrow = arrow)
    }

    plot +
      geom_node_label(aes_string(fill = "color", label = "milestone_id")) +
      scale_fill_identity() +
      theme_graph()
  }
)
