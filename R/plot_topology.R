#' Plot the topology of a trajectory
#'
#' @inheritParams add_cell_coloring
#' @inheritParams add_milestone_coloring
#' @param arrow The type and size of arrow in case of directed trajectories. Set to NULL to remove arrow altogether.
#' @param layout The type of layout to create. See [ggraph::ggraph()] for more info.
#'
#' @keywords plot_trajectory
#'
#' @export
#'
#' @returns A topology ggplot of a trajectory.
#'
#' @include add_milestone_coloring.R
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
      ggraph::create_layout(layout) %>%
      mutate(milestone_id = as.character(.data$name))

    # check milestones, make sure it's a data_frame
    milestones <- check_milestones(trajectory, milestones) %>% add_milestone_coloring(color_milestones)
    milestone_positions <- left_join(milestone_positions, milestones, "milestone_id")

    milestone_graph <- igraph::graph_from_data_frame(
      trajectory$milestone_network,
      vertices = milestone_positions %>% select(-.data$x, -.data$y)
    ) %>%
      as_tbl_graph()

    # add arrow if directed
    plot <- ggraph::ggraph(milestone_graph, "manual", x = milestone_positions$x, y = milestone_positions$y) +
      ggraph::geom_edge_fan()

    if (!is.null(arrow) && any(trajectory$milestone_network$directed)) {
      plot <- plot +
        ggraph::geom_edge_fan(
          aes(
            xend = .data$x + (.data$xend-.data$x)/1.5,
            yend = .data$y + (.data$yend-.data$y)/1.5
          ),
          arrow = arrow
        )
    }

    plot +
      ggraph::geom_node_label(aes(fill = .data$color, label = .data$milestone_id)) +
      scale_fill_identity() +
      theme_graph()
  }
)
