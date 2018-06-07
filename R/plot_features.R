#' Plotting a set of features in a line plot
#'
#' @param features_oi The features of interest, either the number of features or a vector giving the names of the different features
#' @param margin The margin to add between milestones
#'
#' @inheritParams plot_heatmap
#'
#' @importFrom patchwork wrap_plots
#' @export
plot_features <- function(
  traj,
  expression_source = "expression",
  features_oi = 2,
  margin = 0.02,
  cell_feature_importances = NULL,
  scale = dynutils::scale_quantile
) {
  requireNamespace("cobs")

  # process expression
  expression <- get_expression(traj, expression_source)

  if(is.function(scale)) {
    expression <- scale(expression)
    y_scale <- scale_y_continuous("expression", breaks = c(0, 1), labels = c("Low", "High"))
  } else if (scale) {
    expression <- dynutils::scale_quantile(expression)
    y_scale <- scale_y_continuous("expression", breaks = c(0, 1), labels = c("Low", "High"))
  } else {
    y_scale <- scale_y_continuous("expression")
  }

  # get features oi
  features_oi <- check_features_oi(traj, expression, features_oi, cell_feature_importances)
  expression <- expression[, features_oi]

  # linearise
  linearised <- linearise_cells(traj$milestone_network, traj$progressions, one_edge = TRUE, margin = margin)

  # melt
  molten <- expression %>% as.data.frame() %>%
    rownames_to_column("cell_id") %>%
    gather(feature_id, expression, -cell_id)

  # now do spline smoothing
  # first get the points for each milestone through which each spline will have to pass
  milestone_cells <- traj$milestone_percentages %>% group_by(milestone_id) %>% filter(percentage > quantile(percentage, 0.8)) %>% ungroup()
  milestone_constraints <- left_join(
    milestone_cells,
    molten,
    "cell_id"
  ) %>%
    group_by(milestone_id, feature_id) %>%
    summarise(expression = mean(expression)) %>%
    ungroup()

  edge_constraints <- linearised$milestone_network %>%
    crossing(tibble(feature_id = features_oi)) %>%
    left_join(milestone_constraints %>% select(expression_from = expression, from = milestone_id, feature_id), c("from", "feature_id")) %>%
    left_join(milestone_constraints %>% select(expression_to = expression, to = milestone_id, feature_id), c("to", "feature_id")) %>%
    select(edge_id, feature_id, expression_from, expression_to)

  # now calculate the splines for every edge and gene
  nested <- molten %>%
    left_join(linearised$progressions, "cell_id") %>%
    group_by(feature_id, edge_id) %>%
    nest(cumpercentage, expression) %>%
    left_join(edge_constraints, c("edge_id", "feature_id")) %>%
    left_join(linearised$milestone_network, "edge_id")

  smooth_constrained <- function(data, cumstart, expression_from, cumend, expression_to, ...) {
    cobs::cobs(
      data$cumpercentage,
      data$expression,
      pointwise = rbind(c(0, cumstart, expression_from), c(0, cumend, expression_to)),
      print.warn = FALSE,
      print.mesg = FALSE
    )
  }

  splined <- nested %>%
    mutate(spline = pmap(., smooth_constrained)) %>%
    mutate(
      x = map2(cumstart, cumend, seq, length.out = 100),
      y = map2(spline, x, ~predict(.x, .y)[, "fit"])
    )

  smoothed <- splined %>%
    select(x, y, edge_id, feature_id) %>%
    unnest()

  expression_plot <- smoothed %>%
    ggplot() +
    geom_line(aes(x, y, color = feature_id, group = paste0(feature_id, edge_id))) +
    geom_vline(aes(xintercept = cumstart), data = linearised$milestone_network, alpha = 0.2) +
    geom_vline(aes(xintercept = cumend), data = linearised$milestone_network, alpha = 0.2) +
    # cowplot::theme_cowplot()     theme_clean() +
    scale_x_continuous(NULL, breaks = NULL, expand = c(0, 0)) +
    y_scale +
    theme_clean()

  onedim_plot <- plot_onedim(traj, linearised = linearised, orientation = -1, margin = margin) +
    scale_x_continuous(NULL, breaks = NULL, expand = c(0, 0))

  patchwork::wrap_plots(
    expression_plot,
    onedim_plot,
    ncol = 1,
    heights = c(5, 1)
  )
}
