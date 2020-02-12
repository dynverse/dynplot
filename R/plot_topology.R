#' Plot the topology of a trajectory
#'
#' @inheritParams add_cell_coloring
#' @inheritParams add_milestone_coloring
#' @inheritParams ggraph::ggraph
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
  layout = NULL
  ) {
    # make sure a trajectory was provided
    testthat::expect_true(dynwrap::is_wrapper_with_trajectory(trajectory))

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

    milestone_graph <- tidygraph::as_tbl_graph(trajectory$milestone_network)
    milestone_positions <- milestone_graph %>%
      ggraph::create_layout(layout) %>%
      mutate(milestone_id = as.character(name))

    # check milestones, make sure it's a data_frame
    milestones <- check_milestones(trajectory, milestones) %>% add_milestone_coloring(color_milestones)
    milestone_positions <- left_join(milestone_positions, milestones, "milestone_id")

    milestone_graph <- igraph::graph_from_data_frame(
      trajectory$milestone_network,
      vertices = milestone_positions %>% select(-x, -y)
    ) %>% tidygraph::as_tbl_graph()

    arrow <-
      if (any(trajectory$milestone_network$directed)) {
        arrow(type = "closed", length = unit(0.4, "cm"))
      } else {
        NULL
      }

    ggraph::ggraph(milestone_graph, "manual", x = milestone_positions$x, y = milestone_positions$y) +
      ggraph::geom_edge_fan() +
      ggraph::geom_edge_fan(aes(xend = x + (xend-x)/1.5, yend = y + (yend-y)/1.5), arrow = arrow) +
      ggraph::geom_node_label(aes(fill = color, label = milestone_id)) +
      scale_fill_identity() +
      ggraph::theme_graph()
  }
)
