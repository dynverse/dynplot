#' Plot the expression across a trajectory in a heatmap
#'
#' @param features_oi The features of interest, either the number of features or a vector giving the names of the different features
#' @param clust The method to cluster the features, or a hclust object
#' @param scale Whether to rescale the expression, can be a function or boolean
#' @param features_labels The labels for the features. If NULL, will use either the names of the features_oi (if exists) or the features_oi themselves. If not NULL, this should be a named character vector providing the label of a features given its feature_id
#'
#' @keywords plot_trajectory
#'
#' @examples
#' data(example_linear)
#' plot_heatmap(example_linear)
#'
#' data(example_bifurcating)
#' plot_heatmap(example_bifurcating)
#'
#' @include plot_heatmap_annotations.R plot_heatmap_annotations_velocity.R plot_heatmap_annotations_features.R
#'
#' @export
plot_heatmap <- inherit_default_params(
  list(
    annotate_milestone_percentages,
    annotate_milestone_network,
    annotate_velocity,
    annotate_features,
    add_milestone_coloring
  ),
  function(
    dataset,
    trajectory = dataset,
    expression_source = dataset,

    # feature parameters
    features_oi = 20,
    feature_info = NULL,
    highlight_features,
    row_gap = unit(3, "mm"),

    clust = "ward.D2",
    color_cells = NULL,

    # milestones
    milestones = NULL,
    milestone_percentages = trajectory$milestone_percentages,
    color_milestones,

    grouping = NULL,
    groups = NULL,
    scale = dynutils::scale_quantile,

    # cell (column) parameters
    column_gap,
    top_annotation = list(),

    # milestone percentages
    plot_milestone_percentages,

    # milestone network
    plot_milestone_network,
    plot_milestones,
    milestone_network_orientation,
    milestone_network_arrow,

    # velocity
    plot_velocity,
    velocity_each,

    # legends
    heatmap_legend_param = list(direction = "horizontal")
) {
  requireNamespace("ComplexHeatmap")

  # make sure a trajectory was provided
  testthat::expect_true(dynwrap::is_wrapper_with_trajectory(trajectory))

  # linearize cells
  linearised <- linearise_cells(
    trajectory = trajectory,
    equal_cell_width = TRUE,
    margin = 0
  )

  # get expression
  expression <- get_expression(dataset, expression_source = expression_source)
  expression_matrix <- as.matrix(expression[linearised$progressions$cell_id, features_oi])

  # get features oi
  features_oi <- check_features_oi(trajectory, expression, features_oi)

  # cluster features
  clust <- hclust(as.dist(dynutils::correlation_distance(t(expression_matrix))), method = "average")

  cell_ids <- rownames(expression_matrix)

  # create milestones tibble
  milestones <- check_milestones(trajectory, milestones, check_color = TRUE, color_milestones = color_milestones)

  # add milestone network labels based on milestone labels
  linearised$milestone_network <- linearised$milestone_network %>%
    left_join(milestones %>% select(from = milestone_id, label_from = label), "from") %>%
    left_join(milestones %>% select(to = milestone_id, label_to = label), "to") %>%
    mutate(
      label = paste0(label_from, " \U2192 ", label_to)
    )
  linearised$progressions <- linearised$progressions %>% left_join(
    linearised$milestone_network %>% select(from, to, label),
    c("from", "to")
  ) %>% mutate(
    label = factor(label, levels = linearised$milestone_network$label)
  )

  ## setup annotations
  bottom_annotation <- list()
  right_annotation <- list()
  annotation_legends <- list()

  # plot milestone network
  if(first(plot_milestone_network) != "none") {
    c(annotation_milestone_network) %<-% annotate_milestone_network(
      dataset,
      trajectory,
      milestones,
      linearised,
      plot_milestone_network = plot_milestone_network,
      milestone_network_orientation = milestone_network_orientation,
      milestone_network_arrow = milestone_network_arrow,
      plot_milestones = plot_milestones,
      column_gap = column_gap
    )

    if (first(plot_milestone_network) == "top") {
      top_annotation$`Milestone network` <- annotation_milestone_network
    } else {
      bottom_annotation$`Milestone network` <- annotation_milestone_network
    }
  }

  # plot velocity
  if(first(plot_velocity) != "none") {
    c(annotation_velocity, legend_velocity) %<-% annotate_velocity(
      dataset,
      trajectory,
      linearised,
      plot_velocity = plot_velocity,
      velocity_each = velocity_each
    )

    if (first(plot_velocity) == "top") {
      top_annotation$`RNA velocity` <- annotation_velocity
    } else {
      bottom_annotation$`RNA velocity` <- annotation_velocity
    }

    annotation_legends$`RNA velocity` <- legend_velocity
  }

  # plot milestone percentages
  if(first(plot_milestone_percentages) != "none") {
    c(annotation_milestone_percentages, legend_milestone_id) %<-% annotate_milestone_percentages(
      dataset, trajectory, milestones,
      linearised
    )

    if (first(plot_milestone_percentages) == "top") {
      top_annotation$`Milestone percentages` <- annotation_milestone_percentages
    } else {
      bottom_annotation$`Milestone percentages` <- annotation_milestone_percentages
    }

    annotation_legends$Milestones <- legend_milestone_id
  }

  # feature labels and feature annotation
  c(
    annotation_features,
    feature_labels,
    show_row_names,
    row_labels,
    row_names_gp
  ) %<-% annotate_features(
    dataset,
    trajectory,
    features_oi = features_oi,
    feature_info = feature_info,
    highlight_features = highlight_features
  )

  if(!is.null(annotation_features)) {
    right_annotation$Features <- annotation_features
  }

  # wrap up annotations
  top_annotation <- if(length(top_annotation) > 0) {
    invoke(ComplexHeatmap::HeatmapAnnotation, top_annotation, which = "column")
  } else {NULL}
  bottom_annotation <- if(length(bottom_annotation) > 0) {
    invoke(ComplexHeatmap::HeatmapAnnotation, bottom_annotation, which = "column")
  } else {NULL}
  right_annotation <- if(length(right_annotation) > 0) {
    invoke(ComplexHeatmap::HeatmapAnnotation, right_annotation, which = "row")
  } else {NULL}

  browser()

  heatmap <- ComplexHeatmap::Heatmap(
    Matrix::t(dynutils::scale_quantile(expression_matrix)),
    name = "Expression",

    cluster_rows = clust,
    row_split = 4,

    # rows
    row_gap = row_gap,

    # columns
    column_split = linearised$progressions$label,
    column_gap = column_gap,

    # features
    show_row_names = show_row_names,
    row_labels = row_labels,
    row_names_gp = row_names_gp,

    right_annotation = right_annotation,
    cluster_columns = FALSE,
    show_column_names = FALSE,

    # heatmap
    col = rev(RColorBrewer::brewer.pal(10, "RdBu")[2:9]),

    # annotation
    top_annotation = top_annotation,
    bottom_annotation = bottom_annotation,

    # legend
    heatmap_legend_param = heatmap_legend_param
  )

  heatmap_list <- ComplexHeatmap::HeatmapList()
  heatmap_list <- ComplexHeatmap::add_heatmap(heatmap, heatmap_list)

  heatmap <- ComplexHeatmap::make_layout(
    heatmap_list,
    annotation_legend_list = annotation_legends,
    merge_legend = TRUE,
    heatmap_legend_side = "bottom",
    annotation_legend_side = "bottom"
  )
  heatmap
})
