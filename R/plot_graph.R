#' Plot a dimensionality reduced trajectory as a 2D graph
#'
#' @inheritParams check_or_perform_dimred
#' @inheritParams add_cell_coloring
#' @inheritParams add_milestone_coloring
#' @param transition_size The size of the transition lines between milestones.
#' @param cell_size The size of cells.
#' @param milestone_size The size of milestones.
#' @param arrow_length length of the arrow.
#' @param plot_label What to label. Must be one of \code{"leaves"}, \code{"all"}, or \code{"none"}.
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
    task,
    color_cells,
    color_milestones,
    grouping_assignment,
    groups,
    feature_oi,
    pseudotime,
    expression_source,
    milestones,
    milestone_percentages,
    transition_size = 3,
    cell_size = 2,
    milestone_size = 5,
    arrow_length = grid::unit(0.6, "cm"),
    plot_label = c("leaves", "all", "none"),
    plot_milestones = FALSE
  ) {
    # check whether object has already been graph-dimredded
    dimred_task <- check_or_perform_dimred(task)

    if(plot_milestones) {
      if (!is.null(milestones)) {
        milestones <- left_join(dimred_task$space_milestones, milestones, "milestone_id")
      } else {
        milestones <- dimred_task$space_milestones
      }

      # color milestones & cells
      milestones <- milestone_positions <- add_milestone_coloring(milestones, color_milestones)
    }

    cell_positions <- dimred_task$space_samples
    cell_coloring_output <- do.call(add_cell_coloring, map(names(formals(add_cell_coloring)), get, envir=environment()))
    cell_positions <- cell_coloring_output$cell_positions
    fill_scale <- cell_coloring_output$fill_scale

    # check and process label
    plot_label <- match.arg(plot_label)
    nodes_to_label <-
      if (plot_label == "leaves") {
        dimred_task$space_lines %>% {c(setdiff(.$from, .$to), setdiff(.$to, .$from))}
      } else if (plot_label == "all") {
        milestones$milestone_id
      } else if (plot_label == "none"){
        c()
      } else {
        stop(sQuote("plot_label"), " should be one of: \"leaves\", \"all\", or \"none\"")
      }

    # make plot
    g <-
      ggplot() +
      theme(legend.position = "none") +
      geom_segment(
        aes(x = from.Comp1, xend = from.Comp1 + (to.Comp1 - from.Comp1) / 2, y = from.Comp2, yend = from.Comp2 + (to.Comp2 - from.Comp2) / 2),
        dimred_task$space_lines %>% filter(directed),
        size = transition_size, colour = "grey",
        arrow = arrow(length = arrow_length, type = "open")
      ) +
      geom_segment(
        aes(x = from.Comp1, xend = to.Comp1, y = from.Comp2, yend = to.Comp2),
        dimred_task$space_lines,
        size = transition_size, colour = "grey"
      ) +
      geom_point(
        aes(Comp1, Comp2, fill = color),
        cell_positions,
        size = cell_size,
        shape=21
      ) +
      fill_scale +
      theme_graph() +
      theme(legend.position="bottom")
    # ggrepel::geom_label_repel(
    #   aes(Comp1, Comp2, label = milestone_id),
    #   dimred_task$space_milestones %>% filter(milestone_id %in% nodes_to_label)
    # ) +

    if (plot_milestones) {
      g <- g +
        geom_point(
          aes(Comp1, Comp2, color = color),
          milestone_positions,
          size = milestone_size, shape = 4, stroke = 2
        ) +
        scale_color_identity()
    }

    g

    # process_dynplot(g, object$id)
  })

#' @export
plot_default <- plot_graph
