#' Plot a trajectory as a graph
#'
#' @inheritParams dynwrap::calculate_trajectory_dimred
#' @inheritParams add_cell_coloring
#' @inheritParams add_milestone_coloring
#' @inheritParams plot_dimred
#' @param size_trajectory The size of the transition lines between milestones.
#' @param size_milestones The size of milestones.
#' @param arrow The type and size of arrow in case of directed trajectories. Set to NULL to remove arrow altogether.
#' @param plot_milestones Whether to plot the milestones.
#' @param alpha_cells The alpha of the cells.
#' @param size_cells The size of the cells.
#' @param border_radius_percentage The fraction of the radius that is used for the border.
#'
#'
#' @importFrom grid arrow unit
#' @importFrom ggrepel geom_label_repel
#'
#' @aliases plot_default
#'
#' @keywords plot_trajectory
#'
#' @include add_cell_coloring.R
#' @include add_milestone_coloring.R
#'
#' @export
#'
#' @returns A graph ggplot of a trajectory.
#'
#' @examples
#' data(example_disconnected)
#' plot_graph(example_disconnected)
#' plot_graph(example_disconnected, color_cells = "pseudotime")
#' plot_graph(
#'   example_disconnected,
#'   color_cells = "grouping",
#'   grouping = dynwrap::group_onto_nearest_milestones(example_disconnected)
#' )
#'
#' data(example_tree)
#' plot_graph(example_tree)
plot_graph <- dynutils::inherit_default_params(
  list(add_cell_coloring, add_milestone_coloring),
  function(
    trajectory,
    color_cells,
    color_milestones,
    grouping,
    groups,
    feature_oi,
    pseudotime,
    expression_source,
    milestones,
    milestone_percentages,
    size_trajectory = 3,
    size_milestones = 8,
    alpha_cells = 1,
    size_cells = 2.5,
    border_radius_percentage = .1,
    arrow = grid::arrow(length = grid::unit(1, "cm"), type = "closed"),
    label_milestones = dynwrap::is_wrapper_with_milestone_labelling(trajectory),
    plot_milestones = FALSE,
    adjust_weights = FALSE
  ) {
    # make sure a trajectory was provided
    assert_that(dynwrap::is_wrapper_with_trajectory(trajectory))

    # TODO: 'milestones', in this function, is both used as the colouring of the cells (which could be from a different trajectory),
    # and plotting the milestones in the same dimred as the cells.
    # it's so confusing

    # check whether object has already been graph-dimredded
    dimred_traj <- calculate_trajectory_dimred(trajectory, adjust_weights = adjust_weights)

    # check milestones, make sure it's a data_frame
    milestones <- check_milestones(trajectory, milestones, milestone_percentages = milestone_percentages)

    # add coloring of milestones if not present
    milestones <- add_milestone_coloring(milestones, color_milestones)

    # get information of cells
    cell_positions <- dimred_traj$cell_positions
    cell_coloring_output <- add_cell_coloring(
      cell_positions = cell_positions,
      color_cells = color_cells,
      trajectory = trajectory,
      grouping = grouping,
      groups = groups,
      feature_oi = feature_oi,
      expression_source = expression_source,
      pseudotime = pseudotime,
      color_milestones = color_milestones,
      milestones = milestones,
      milestone_percentages = milestone_percentages
    )

    cell_positions <- cell_coloring_output$cell_positions
    color_scale <- cell_coloring_output$color_scale

    # get trajectory dimred
    # add coloring of milestones only if milestone percentages are not given
    milestone_positions <- dimred_traj$milestone_positions
    if (cell_coloring_output$color_cells == "milestone") {
      milestone_positions <- left_join(milestone_positions, milestones, "milestone_id")
    } else {
      milestone_positions$color <- NA_character_
    }

    # get information of segments
    dimred_segments <- dimred_traj$edge_positions

    # plot the topology
    plot <-
      ggplot() +
      theme(legend.position = "none") +

      # Divergence gray backgrounds
      geom_polygon(
        aes(x = .data$comp_1, y = .data$comp_2, group = .data$triangle_id),
        dimred_traj$divergence_polygon_positions,
        fill = "#eeeeee"
      ) +

      # Divergence dashed lines
      geom_segment(
        aes(x = .data$comp_1_from, xend = .data$comp_1_to, y = .data$comp_2_from, yend = .data$comp_2_to),
        dimred_traj$divergence_edge_positions,
        colour = "darkgray",
        linetype = "dashed"
      )

    if (plot_milestones) {
      plot <- plot +
        geom_point(
          aes(.data$comp_1, .data$comp_2),
          size = 12,
          data = milestone_positions,
          colour = "gray"
        )
    }

    # add arrow if directed
    my_arrow <-
      if (any(trajectory$milestone_network$directed)) {
        arrow
      } else {
        NULL
      }

      # Transition gray border
    plot <- plot +
      geom_segment(
        aes(x = .data$comp_1_from, xend = .data$comp_1_to, y = .data$comp_2_from, yend = .data$comp_2_to),
        dimred_segments,
        size = size_trajectory + 2,
        colour = "grey"
      ) +

      # Transition halfway arrow
      geom_segment(
        aes(
          x = .data$comp_1_from,
          xend = .data$comp_1_from + (.data$comp_1_to - .data$comp_1_from) / 1.5,
          y = .data$comp_2_from,
          yend = .data$comp_2_from + (.data$comp_2_to - .data$comp_2_from) / 1.5
        ),
        dimred_segments %>% filter(.data$directed, .data$length > 0),
        size = 1,
        colour = "grey",
        arrow = my_arrow
      ) +

      # Transition white tube
      geom_segment(
        aes(x = .data$comp_1_from, xend = .data$comp_1_to, y = .data$comp_2_from, yend = .data$comp_2_to),
        dimred_segments,
        size = size_trajectory,
        colour = "white"
      )

    if (plot_milestones) {
      plot <- plot +
        # Milestone white bowl
        geom_point(aes(.data$comp_1, .data$comp_2), size = size_milestones, data = milestone_positions, colour = "white") +

        # Milestone fill
        geom_point(
          aes(.data$comp_1, .data$comp_2, colour = .data$color),
          size = size_milestones * .8,
          data = milestone_positions %>% filter(!is.na(.data$color)),
          alpha = .5
        )
    }

    # plot the cells
    if (border_radius_percentage > 0) {
      plot <- plot +
        geom_point(aes(.data$comp_1, .data$comp_2), size = size_cells, color = "black", data = cell_positions)
    }

    if (alpha_cells < 1) {
      plot <- plot +
        geom_point(aes(.data$comp_1, .data$comp_2), size = size_cells * (1 - border_radius_percentage), color = "white", data = cell_positions)
    }

    plot <- plot +
      # Cell fills
      geom_point(aes(.data$comp_1, .data$comp_2, color = .data$color), size = size_cells * (1 - border_radius_percentage), alpha = alpha_cells, data = cell_positions) +

      color_scale +
      theme_graph() +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

    # label milestones
    label_milestones <- get_milestone_labelling(trajectory, label_milestones)

    if (length(label_milestones)) {
      milestone_labels <- milestone_positions %>%
        mutate(label = label_milestones[.data$milestone_id]) %>%
        filter(!is.na(.data$label))

      plot <- plot + geom_label(aes(.data$comp_1, .data$comp_2, label = .data$label), data = milestone_labels)
    }

    plot
  }
)
