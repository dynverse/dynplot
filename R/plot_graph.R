#' Plot a dimensionality reduced trajectory as a 2D graph
#'
#' @inheritParams dynwrap::calculate_trajectory_dimred
#' @inheritParams add_cell_coloring
#' @inheritParams add_milestone_coloring
#' @inheritParams plot_dimred
#' @param transition_size The size of the transition lines between milestones.
#' @param milestone_size The size of milestones.
#' @param arrow_length length of the arrow.
#' @param plot_milestones Whether to plot the milestones.
#'
#'
#' @importFrom grid arrow unit
#' @importFrom ggrepel geom_label_repel
#'
#' @aliases plot_default
#'
#' @include add_coloring.R
#'
#' @export
plot_graph <- dynutils::inherit_default_params(
  list(add_cell_coloring, add_milestone_coloring),
  function(
    traj,
    color_cells,
    color_milestones,
    grouping,
    groups,
    feature_oi,
    pseudotime,
    expression_source,
    milestones,
    milestone_percentages,
    transition_size = 3,
    milestone_size = 5,
    arrow_length = grid::unit(1, "cm"),
    label_milestones = dynwrap::is_wrapper_with_milestone_labelling(traj),
    plot_milestones = FALSE,
    adjust_weights = FALSE
  ) {
    # make sure a trajectory was provided
    testthat::expect_true(dynwrap::is_wrapper_with_trajectory(traj))

    # TODO: 'milestones', in this function, is both used as the colouring of the cells (which could be from a different traj),
    # and plotting the milestones in the same dimred as the cells.
    # it's so confusing

    # check whether object has already been graph-dimredded
    dimred_traj <- calculate_trajectory_dimred(traj, adjust_weights = adjust_weights)

    # check milestones, make sure it's a data_frame
    milestones <- check_milestones(traj, milestones, milestone_percentages = milestone_percentages)

    # add coloring of milestones if not present
    milestones <- add_milestone_coloring(milestones, color_milestones)

    # get information of cells
    cell_positions <- dimred_traj$dimred_cells
    cell_coloring_output <- add_cell_coloring(
      cell_positions = cell_positions,
      color_cells = color_cells,
      traj = traj,
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
    milestone_positions <- dimred_traj$dimred_milestones
    if (cell_coloring_output$color_cells == "milestone") {
      milestone_positions <- left_join(milestone_positions, milestones, "milestone_id")
    } else {
      milestone_positions$color <- NA
    }

    # get information of segments
    dimred_segments <- dimred_traj$dimred_segments

    # plot the topology
    plot <-
      ggplot() +
      theme(legend.position = "none") +

      # Divergence gray backgrounds
      geom_polygon(
        aes(x = comp_1, y = comp_2, group = triangle_id),
        dimred_traj$dimred_divergence_polys,
        fill = "#eeeeee"
      ) +

      # Divergence dashed lines
      geom_segment(
        aes(x = from.comp_1, xend = to.comp_1, y = from.comp_2, yend = to.comp_2),
        dimred_traj$dimred_divergence_segments,
        colour = "darkgray",
        linetype = "dashed"
      ) +

      # Milestone gray border
      geom_point(aes(comp_1, comp_2), size = 12, data = milestone_positions, colour = "gray") +

      # Transition gray border
      geom_segment(
        aes(x = from.comp_1, xend = to.comp_1, y = from.comp_2, yend = to.comp_2),
        dimred_segments,
        size = transition_size + 2,
        colour = "grey"
      ) +

      # Transition halfway arrow
      geom_segment(
        aes(x = from.comp_1, xend = from.comp_1 + (to.comp_1 - from.comp_1) / 1.5, y = from.comp_2, yend = from.comp_2 + (to.comp_2 - from.comp_2) / 1.5),
        dimred_segments %>% filter(directed, length > 0),
        size = 1,
        colour = "grey",
        arrow = arrow(length = arrow_length, type = "closed")
      ) +

      # Transition white tube
      geom_segment(
        aes(x = from.comp_1, xend = to.comp_1, y = from.comp_2, yend = to.comp_2),
        dimred_segments,
        size = transition_size,
        colour = "white"
      )

    if (plot_milestones) {
      plot <- plot +
        # Milestone white bowl
        geom_point(aes(comp_1, comp_2), size = 10, data = milestone_positions, colour = "white") +

        # Milestone fill
        geom_point(aes(comp_1, comp_2, colour = color), size = 8, data = milestone_positions %>% filter(!is.na(color)), alpha = .5)
    }

    # plot the cells

    plot <- plot +
      # Cell borders
      geom_point(aes(comp_1, comp_2), size = 2.5, color = "black", data = cell_positions) +

      # Cell fills
      geom_point(aes(comp_1, comp_2, color = color), size = 2, data = cell_positions) +

      color_scale +
      theme_graph() +
      theme(legend.position = "bottom")

    # label milestones
    label_milestones <- get_milestone_labelling(traj, label_milestones)

    if(length(label_milestones)) {
      milestone_labels <- milestone_positions %>%
        mutate(label = label_milestones[milestone_id]) %>%
        filter(!is.na(label))

      plot <- plot + geom_label(aes(comp_1, comp_2, label = label), data = milestone_labels)
    }

    plot
  }
)

#' @export
plot_default <- plot_graph
