#' Plot a dimensionality reduced trajectory as a 2D graph
#'
#' @inheritParams check_or_perform_dimred
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
    # check whether object has already been graph-dimredded
    dimred_traj <- check_or_perform_dimred(traj)

    # check milestones, make sure it's a data_frame
    milestones <- check_milestone_data_frame(milestones)

    # add extra lines encompassing divergence regions
    if(nrow(traj$divergence_regions)) {
      space_lines_divergence_regions <- traj$divergence_regions %>%
        group_by(divergence_id) %>%
        summarise(comb = list(combn(milestone_id, 2) %>% t() %>% as.data.frame() %>% mutate_if(is.factor, as.character))) %>%
        unnest(comb) %>%
        rename(from=V1, to=V2) %>%
        select(from, to) %>%
        mutate(line_type="divergence") %>%
        mutate(directed = FALSE)
    } else {
      space_lines_divergence_regions <- tibble()
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
      left_join(dimred_traj$space_milestones %>% select(milestone_id, comp_1, comp_2) %>% rename_all(~paste0("from.", .)), c("from"="from.milestone_id"))%>%
      left_join(dimred_traj$space_milestones %>% select(milestone_id, comp_1, comp_2) %>% rename_all(~paste0("to.", .)), c("to"="to.milestone_id"))

    space_regions <- traj$divergence_regions %>% left_join(dimred_traj$space_milestones, "milestone_id")

    # get information of milestones
    if (!is.null(milestones)) {
      milestones <- left_join(dimred_traj$space_milestones, milestones, "milestone_id")
    } else {
      milestones <- dimred_traj$space_milestones
    }

    milestones <- milestone_positions <- add_milestone_coloring(milestones, color_milestones)

    # get information of cells
    cell_positions <- dimred_traj$space_samples
    cell_coloring_output <- do.call(add_cell_coloring, map(names(formals(add_cell_coloring)), get, envir=environment()))
    cell_positions <- cell_coloring_output$cell_positions
    color_scale <- cell_coloring_output$color_scale

    # make plot
    plot <-
      ggplot() +
      theme(legend.position = "none") +
      # geom_segment(
      #   aes(x = from.comp_1, xend = from.comp_1 + (to.comp_1 - from.comp_1) / 2, y = from.comp_2, yend = from.comp_2 + (to.comp_2 - from.comp_2) / 2),
      #   dimred_traj$space_lines %>% filter(directed),
      #   size = transition_size, colour = "grey",
      #   arrow = arrow(length = arrow_length, type = "closed")
      # ) +
      # geom_segment(
      #   aes(x = from.comp_1, xend = to.comp_1, y = from.comp_2, yend = to.comp_2),
      #   dimred_traj$space_lines,
      #   size = transition_size, colour = "grey"
      # ) +
    #
    geom_segment(
      aes(x = from.comp_1, xend = from.comp_1 + (to.comp_1 - from.comp_1) / 1.5, y = from.comp_2, yend = from.comp_2 + (to.comp_2 - from.comp_2) / 1.5),
      space_lines %>% filter(directed),
      size = 1, colour = "grey",
      arrow = arrow(length = arrow_length, type = "closed")
    ) +
    geom_segment(
      aes(x = from.comp_1, xend = to.comp_1, y = from.comp_2, yend = to.comp_2),
      space_lines,
      size = transition_size + 2, colour = "grey",
    ) +
    geom_segment(
      aes(x = from.comp_1, xend = to.comp_1, y = from.comp_2, yend = to.comp_2),
      space_lines,
      size = transition_size, colour = "white",
    ) +
      geom_polygon(
        aes(x = comp_1, y = comp_2, group=divergence_id),
        space_regions,
        fill="white"
      ) +
      geom_point(aes(comp_1, comp_2), size=2.5, color="black", data=cell_positions) +
      geom_point(aes(comp_1, comp_2, color=color), size=2, data=cell_positions) +
      color_scale +
      theme_graph() +
      theme(legend.position="bottom")

    # label milestones
    label_milestones <- get_milestone_labelling(traj, label_milestones)

    if(length(label_milestones)) {
      milestone_labels <- milestone_positions %>%
        mutate(label = label_milestones[milestone_id]) %>%
        filter(!is.na(label))

      plot <- plot + geom_label(aes(comp_1, comp_2, label=label), data=milestone_labels)
    }

    plot
  })

#' @export
plot_default <- plot_graph
