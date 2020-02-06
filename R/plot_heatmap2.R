#' Plot the expression across a trajectory in a heatmap
#'
#' @param features_oi The features of interest, either the number of features or a vector giving the names of the different features
#' @param clust The method to cluster the features, or a hclust object
#' @param scale Whether to rescale the expression, can be a function or boolean
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
#' @include plot_heatmap_annotations.R
#'
#' @export
plot_heatmap <- inherit_default_params(
  list(
    annotate_milestone_percentages,
    annotate_milestone_network
  ),
  function(
    dataset,
    trajectory = dataset,
    expression_source = dataset,
    features_oi = 20,
    clust = "ward.D2",
    color_cells = NULL,
    milestones = NULL,
    milestone_percentages = trajectory$milestone_percentages,
    grouping = NULL,
    groups = NULL,
    scale = dynutils::scale_quantile,

    # cell parameters
    column_gap = unit(1, "mm"),

    # milestone percentages
    plot_milestone_percentages
) {

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
  milestones <- check_milestones(trajectory, milestones)

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
  )

  ## setup annotations
  top_annotation <- list()
  bottom_annotation <- list()
  annotation_legends <- list()

  # plot milestone percentages
  if(first(plot_milestone_percentages) != "none") {
    c(annotation_milestone_percentages, legend_milestone_id) %<-% annotate_milestone_percentages(
      dataset, trajectory, milestones,
      linearised
    )
    top_annotation$`Milestone percentages` <- annotation_milestone_percentages
    annotation_legends$Milestones <- legend_milestone_id
  }

  # plot milestone network
  if(first(plot_milestone_network) != "none") {
    c(annotation_milestone_network, legend_milestone_id) %<-% annotate_milestone_network(
      dataset,
      trajectory,
      milestones,
      linearised,
      plot_milestone_network = plot_milestone_network,
      milestone_network_orientation = milestone_network_orientation,
      plot_milestones = plot_milestones
    )
    bottom_annotation$`Milestone network` <- annotation_milestone_network
    annotation_legends$Milestones <- legend_milestone_id
  }

  # wrap up annotations
  top_annotation <- invoke(HeatmapAnnotation, top_annotation)
  bottom_annotation <- do.call(HeatmapAnnotation, bottom_annotation)

  heatmap <- Heatmap(
    Matrix::t(dynutils::scale_quantile(expression_matrix)),
    row_labels = names(features_oi) %||% features_oi,
    name = "Expression",

    cluster_rows = clust,
    row_split = 4,

    cluster_columns = FALSE,
    show_column_names = FALSE,
    column_split = linearised$progressions$label,
    column_gap = column_gap,
    col = rev(RColorBrewer::brewer.pal(10, "RdBu")[2:9]),
    top_annotation = top_annotation,
    bottom_annotation = bottom_annotation
  )
  draw(
    heatmap,
    annotation_legend_list = annotation_legends,
    merge_legend = TRUE,
    heatmap_legend_side = "right",
    annotation_legend_side = "right"
  )
})
