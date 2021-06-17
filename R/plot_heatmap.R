#' Plot expression data along a trajectory
#'
#' `NOTE:` When using RStudio, the heatmap might not show inside the plot area, but will be visible once you click the 'Zoom' button.
#'
#' @param features_oi The features of interest, either the number of features or a vector giving the names of the different features
#' @param clust The method to cluster the features, or a hclust object
#' @param cell_feature_importances The importances of every feature in every cell, as returned by [dynfeature::calculate_cell_feature_importance()]
#' @param heatmap_type The type of heatmap, either tiled or dotted
#' @param scale Whether to rescale the expression, can be a function or boolean
#'
#' @inheritParams plot_onedim
#'
#' @keywords plot_trajectory
#'
#' @importFrom patchwork wrap_plots
#'
#' @returns A heatmap ggplot of an expression dataset with trajectory.
#'
#' @examples
#' data(example_bifurcating)
#' plot_heatmap(example_bifurcating)
#'
#' @export
plot_heatmap <- function(
  trajectory,
  expression_source = "expression",
  features_oi = 20,
  clust = "ward.D2",
  margin = 0.02,
  color_cells = NULL,
  milestones = NULL,
  milestone_percentages = trajectory$milestone_percentages,
  grouping = NULL,
  groups = NULL,
  cell_feature_importances = NULL,
  heatmap_type = c("tiled", "dotted"),
  scale = dynutils::scale_quantile,
  label_milestones = TRUE
) {
  # make sure a trajectory was provided
  assert_that(dynwrap::is_wrapper_with_trajectory(trajectory))

  heatmap_type <- match.arg(heatmap_type)

  # process expression
  expression <- get_expression(trajectory, expression_source)

  # convert to regular matrix if sparse
  if (dynutils::is_sparse(expression)) {
    expression <- as.matrix(expression)
  }

  if (is.function(scale)) {
    expression <- scale(expression)
  } else if (is.logical(scale) && scale) {
    expression <- dynutils::scale_quantile(expression)
  }

  # check milestones, make sure it's a data_frame
  milestones <- check_milestones(trajectory, milestones)

  # get features oi
  features_oi <- check_features_oi(trajectory, expression, features_oi, cell_feature_importances)
  expression <- expression[, features_oi]

  # cluster features
  if (is.character(clust)) {
    clust <- stats::hclust(stats::as.dist(dynutils::calculate_distance(t(expression), method = "pearson")), method = clust)
  }
  feature_order <- colnames(expression)[clust$order]

  # put cells on one edge with equal width per cell
  linearised <- linearise_cells(
    trajectory = trajectory,
    equal_cell_width = TRUE,
    margin = margin
  )

  # melt expression
  molten <- expression %>%
    reshape2::melt(varnames = c("cell_id", "feature_id"), value.name = "expression") %>%
    mutate_if(is.factor, as.character) %>%
    mutate(feature_position = as.numeric(factor(.data$feature_id, feature_order))) %>%
    left_join(linearised$progressions, "cell_id")

  # check importances
  if (!is.null(cell_feature_importances)) {
    molten <- left_join(
      molten,
      cell_feature_importances,
      c("cell_id", "feature_id")
    )
  }

  # plot heatmap
  x_limits <- c(min(linearised$milestone_network$cumstart) - 1, max(linearised$milestone_network$cumend) + 1)
  y_limits <- c(0.5, length(feature_order) + 0.5)

  heatmap <-
    if (heatmap_type == "tiled") {
      if (is.null(cell_feature_importances)) {
        ggplot(molten) +
          geom_tile(aes(.data$cumpercentage, .data$feature_position, fill = .data$expression)) +
          scale_fill_distiller(palette = "RdBu") +
          scale_x_continuous(NULL, breaks = NULL, expand = c(0, 0), limits = x_limits) +
          scale_y_continuous(NULL, expand = c(0, 0), breaks = seq_along(feature_order), labels = feature_order, position = "left", limits = y_limits) +
          theme(legend.position = "none", plot.margin = margin(), plot.background = element_blank(), panel.background = element_blank())
      } else {
        ggplot(molten) +
          # geom_tile(aes(cumpercentage, feature_position, alpha = importance), fill = "black") +
          geom_rect(aes(
            xmin = .data$cumpercentage-0.5,
            xmax = .data$cumpercentage+0.5,
            ymin = .data$feature_position+scale_minmax(.data$importance)/10*5,
            ymax = .data$feature_position-scale_minmax(.data$importance)/10*5,
            fill = .data$expression
          )) +
          scale_fill_distiller(palette = "RdBu") +
          scale_x_continuous(NULL, breaks = NULL, expand = c(0, 0), limits = x_limits) +
          scale_y_continuous(NULL, expand = c(0, 0), breaks = seq_along(feature_order), labels = feature_order, position = "left", limits = y_limits) +
          scale_alpha_continuous(range = c(0, 1)) +
          theme(legend.position = "none", plot.margin = margin(), plot.background = element_blank(), panel.background = element_blank())
      }
    } else if (heatmap_type == "dotted") {
      if (is.null(cell_feature_importances)) {
        ggplot(molten) +
          geom_point(aes(.data$cumpercentage, .data$feature_position, color = .data$expression, size = .data$expression)) +
          scale_color_distiller(palette = "RdBu") +
          scale_size_continuous(range = c(0, 6)) +
          scale_x_continuous(NULL, breaks = NULL, expand = c(0, 0), limits = x_limits) +
          scale_y_continuous(NULL, expand = c(0, 0), breaks = seq_along(feature_order), labels = feature_order, position = "left", limits = y_limits) +
          theme(legend.position = "none", plot.margin = margin(), plot.background = element_blank(), panel.background = element_blank())
      } else {
        ggplot(molten) +
          geom_point(aes(.data$cumpercentage, .data$feature_position, color = .data$expression, size = .data$importance**2)) +
          scale_color_distiller(palette = "RdBu") +
          scale_size_continuous(range = c(0, 6)) +
          scale_x_continuous(NULL, breaks = NULL, expand = c(0, 0), limits = x_limits) +
          scale_y_continuous(NULL, expand = c(0, 0), breaks = seq_along(feature_order), labels = feature_order, position = "left", limits = y_limits) +
          theme(legend.position = "none", plot.margin = margin(), plot.background = element_blank(), panel.background = element_blank())
      }
    }

  # plot one dim
  onedim <- plot_onedim(
    trajectory,
    linearised = linearised,
    orientation = -1,
    quasirandom_width = 0,
    margin = margin,
    color_cells = color_cells,
    grouping = grouping,
    groups = groups,
    milestone_percentages = milestone_percentages,
    milestones = milestones,
    plot_cells = FALSE,
    label_milestones = label_milestones
  ) +
    scale_x_continuous(expand = c(0, 0), limits = x_limits) +
    theme(plot.margin = margin())

  # plot dendrogram
  dendrogram <- ggraph::ggraph(stats::as.dendrogram(clust), "dendrogram") +
    ggraph::geom_edge_elbow() +
    scale_x_continuous(limits = c(-0.5, length(feature_order)-0.5), expand = c(0, 0)) +
    scale_y_reverse() +
    coord_flip() +
    theme_graph() +
    theme(plot.margin = margin())

  # plot cell information
  # TODO: Allow multiple cell info here, even "external" which does not fit into grouping,  milestone_percentages or pseudotime. The current solution is only temporary and ugly!
  if (!is.null(grouping)) {
    cell_annotation_positions <-
      linearised$progressions %>%
      add_cell_coloring(
        color_cells = "grouping",
        grouping = grouping,
        trajectory = trajectory,
        groups = groups,
        milestones = milestones
      )
  } else if (!is.null(milestone_percentages)) {
    cell_annotation_positions <-
      linearised$progressions %>%
      add_cell_coloring(
        color_cells = "milestone",
        milestone_percentages = milestone_percentages,
        trajectory = trajectory,
        milestones = milestones
      )
    }

  cell_annotation <-
    ggplot(cell_annotation_positions$cell_positions) +
    geom_point(aes(.data$cumpercentage, 1, color = .data$color)) +
    cell_annotation_positions$color_scale +
    scale_x_continuous(expand = c(0, 0), limits = x_limits) +
    theme_graph() +
    theme(legend.position = "top")

  patchwork::wrap_plots(
    empty_plot(),
    cell_annotation,
    dendrogram,
    heatmap,
    empty_plot(),
    onedim,
    ncol = 2,
    widths = c(2, 10),
    heights = c(0.5, 10, 2)
  )
}


