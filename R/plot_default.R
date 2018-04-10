#' Plot a dimensionality reduced trajectory
#'
#' @inheritParams check_or_perform_dimred
#' @param transition_size The size of the transition lines between milestones.
#' @param cell_size The size of cells.
#' @param milestone_size The size of milestones.
#' @param arrow_length length of the arrow.
#' @param plot_label What to label. Must be one of \code{"leaves"}, \code{"all"}, or \code{"none"}.
#' @param plot_milestones Whether to plot the milestones.
#'
#' @importFrom grid arrow unit
#' @importFrom ggrepel geom_label_repel
#'
#' @export
plot_default <- function(
  object,
  colour_cells = "milestone",
  colour_milestones = "auto",
  transition_size = 8,
  cell_size = 3,
  milestone_size = 5,
  arrow_length = grid::unit(1, "cm"),
  plot_label = c("leaves", "all", "none"),
  plot_milestones = TRUE
) {
  # check whether object has already been graph-dimredded
  dimred_object <- check_or_perform_dimred(
    object,
    colour_cells = colour_cells,
    colour_milestones = colour_milestones
  )

  # check and process label
  plot_label <- match.arg(plot_label)
  nodes_to_label <-
    if (plot_label == "leaves") {
      dimred_object$space_lines %>% {c(setdiff(.$from, .$to), setdiff(.$to, .$from))}
    } else if (plot_label == "all") {
      dimred_object$space_milestones$milestone_id
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
      dimred_object$space_lines %>% filter(directed),
      size = transition_size, colour = "#444444",
      arrow = arrow(length = arrow_length, type = "open")
    ) +
    geom_segment(
      aes(x = from.Comp1, xend = to.Comp1, y = from.Comp2, yend = to.Comp2),
      dimred_object$space_lines,
      size = transition_size, colour = "#444444"
    ) +
    geom_point(
      aes(Comp1, Comp2, colour = colour),
      dimred_object$space_samples,
      size = cell_size
    ) +
    ggrepel::geom_label_repel(
      aes(Comp1, Comp2, label = milestone_id, fill = colour),
      dimred_object$space_milestones %>% filter(milestone_id %in% nodes_to_label)
    ) +
    scale_colour_identity() +
    scale_fill_identity()

  if (plot_milestones) {
    g <- g +
      geom_point(
        aes(Comp1, Comp2, colour = colour, fill = colour),
        dimred_object$space_milestones,
        size = milestone_size, shape = 4, stroke = 2
      )
  }

  process_dynplot(g, object$id)
}
