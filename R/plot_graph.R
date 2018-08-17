#' Plot a dimensionality reduced trajectory as a 2D graph
#'
#' @inheritParams dynwrap::check_or_perform_dimred
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
    plot_milestones = TRUE
  ) {
    # make sure a trajectory was provided
    testthat::expect_true(dynwrap::is_wrapper_with_trajectory(traj))

    # TODO: 'milestones', in this function, is both used as the colouring of the cells (which could be from a different traj),
    # and plotting the milestones in the same dimred as the cells.
    # it's so confusing

    # check whether object has already been graph-dimredded
    dimred_traj <- calculate_trajectory_dimred(traj)

    # check milestones, make sure it's a data_frame
    milestones <- check_milestones(traj, milestones)

    # add extra lines encompassing divergence regions
    if (nrow(traj$divergence_regions) > 0) {
      # determine the divergence triangles
      space_triags <- get_divergence_triangles(traj$divergence_regions)

      space_lines_divergence_regions <-
        space_triags %>%
        select(from = node1, to = node2) %>%
        mutate(line_type = "divergence", directed = FALSE)

      # define polygon triangles
      space_regions <-
        space_triags %>%
        mutate(triag_id = row_number()) %>%
        select(-divergence_id) %>%
        gather(triangle_part, milestone_id, -triag_id) %>%
        left_join(dimred_traj$dimred_milestones, "milestone_id")
    } else {
      space_lines_divergence_regions <- tibble()
      space_regions <- tibble(triag_id = character(0), comp_1 = numeric(0), comp_2 = numeric(0))
    }

    space_lines <- bind_rows(
      traj$milestone_network %>%
        select(-length) %>%
        mutate(line_type = "forward"),
      space_lines_divergence_regions
    ) %>%
      group_by(from, to) %>%
      filter(dplyr::row_number() == 1) %>%
      ungroup() %>%
      left_join(dimred_traj$dimred_milestones %>% select(milestone_id, comp_1, comp_2) %>% rename_all(~paste0("from.", .)), c("from" = "from.milestone_id"))%>%
      left_join(dimred_traj$dimred_milestones %>% select(milestone_id, comp_1, comp_2) %>% rename_all(~paste0("to.", .)), c("to" = "to.milestone_id"))

    # get information of cells
    cell_positions <- dimred_traj$dimred_cells
    cell_coloring_output <- do.call(add_cell_coloring, map(names(formals(add_cell_coloring)), get, envir = environment()))
    cell_positions <- cell_coloring_output$cell_positions
    color_scale <- cell_coloring_output$color_scale

    # get information of milestones
    if (!is.null(milestones)) {
      milestones <- left_join(dimred_traj$dimred_milestones, milestones, "milestone_id")
    } else {
      milestones <- dimred_traj$dimred_milestones
    }

    milestones <- milestone_positions <- add_milestone_coloring(milestones, color_milestones)

    # make plot
    plot <-
      ggplot() +
      theme(legend.position = "none") +
      geom_polygon(
        aes(x = comp_1, y = comp_2, group = triag_id),
        space_regions,
        fill = "#eeeeee"
      ) +
      geom_segment(
        aes(x = from.comp_1, xend = to.comp_1, y = from.comp_2, yend = to.comp_2),
        space_lines %>% filter(line_type == "divergence"),
        colour = "darkgray",
        linetype = "dashed"
      ) +
      geom_segment(
        aes(x = from.comp_1, xend = from.comp_1 + (to.comp_1 - from.comp_1) / 1.5, y = from.comp_2, yend = from.comp_2 + (to.comp_2 - from.comp_2) / 1.5),
        space_lines %>% filter(directed),
        size = 1, colour = "grey",
        arrow = arrow(length = arrow_length, type = "closed")
      ) +
      geom_segment(
        aes(x = from.comp_1, xend = to.comp_1, y = from.comp_2, yend = to.comp_2),
        space_lines %>% filter(line_type != "divergence"),
        size = transition_size + 2, colour = "grey"
      ) +
      geom_segment(
        aes(x = from.comp_1, xend = to.comp_1, y = from.comp_2, yend = to.comp_2),
        space_lines %>% filter(line_type != "divergence"),
        size = transition_size,
        colour = "white"
      ) +
      geom_point(aes(comp_1, comp_2), size = 2.5, color = "black", data = cell_positions) +
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
