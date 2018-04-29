#' Order the cells according to their progression,
#'  assign tented cells to the highest percentage
#'
#' @param milestone_network The milestone network
#' @param progressions The progressions
order_cells <- function(milestone_network, progressions) {
  milestone_network <- milestone_network %>%
    mutate(
      cumstart = c(0, cumsum(length)[-length(length)]),
      cumend = c(cumsum(length)),
      edge_id = seq_len(n())
    )
  filtered_progression <- progressions %>% # a cell can only be in one edge (maximum in tents)
    select(cell_id, from, to, percentage) %>%
    left_join(milestone_network, by=c("from", "to")) %>%
    group_by(cell_id) %>% arrange(-percentage) %>% filter(row_number() == 1)

  ordered_progression <- filtered_progression %>%
    mutate(cumpercentage=percentage*length + cumstart) %>%
    arrange(cumpercentage)

  ordered_progression %>% select(cell_id, edge_id) %>% ungroup() %>% mutate(position=seq_len(n()), edge_id=factor(edge_id))
}

#' Plot the task as a heatmap
#'
#' @param task The task
#' @param features_oi features to plot, or the top number of features to select
#' @param clust The method to cluster the features, or a hclust object
#' @param cell_feature_importances The feature importances per cell
#' @param heatmap_type The type of heatmap, either tiled or dotted
#'
#' @inheritParams plot_onedim
#'
#' @import tidygraph
#' @import ggraph
#' @importFrom patchwork wrap_plots
#'
#' @export
plot_heatmap <- function(
  task,
  expression_source = "expression",
  features_oi = 20,
  clust = "ward.D2",
  margin = 0.02,
  color_cells = NULL,
  milestones = NULL,
  milestone_percentages = task$milestone_percentages,
  grouping_assignment = NULL,
  groups = NULL,
  cell_feature_importances = NULL,
  heatmap_type = c("tiled", "dotted")
) {
  heatmap_type <- match.arg(heatmap_type)

  # process expression
  expression <- check_expression_source(task, expression_source)
  expression <- dynutils::scale_quantile(expression)

  # get features oi
  if (length(features_oi) == 1 & is.numeric(features_oi) & features_oi[1] > 0) {
    # make sure features_oi is not larger than the number of features
    if(ncol(expression) < features_oi) {features_oi <- ncol(expression)}

    message("No features of interest provided, selecting the top ", features_oi, " features automatically")

    # choose cell_feauture_importance if give, otherwise choose dynfeature if it is installed, otherwise use more simplistic approach
    if (!is.null(cell_feature_importances)) {
      message("Selecting features with top maximal feature importance across cells")

      features_oi <- cell_feature_importances %>%
        group_by(feature_id) %>%
        summarise(importance=max(importance)) %>%
        top_n(features_oi, importance) %>%
        pull(feature_id)

    } else if ("dynfeature" %in% rownames(installed.packages())) {
      message("Using dynfeature for selecting the top ", features_oi, " features")
      requireNamespace("dynfeature")

      features_oi <- dynfeature::calculate_overall_feature_importance(task, expression=expression)$feature_id[1:features_oi]
    } else {
      features_oi <- apply(expression, 2, sd) %>% sort() %>% names() %>% tail(features_oi)
    }
  }

  expression <- expression[, features_oi]

  # cluster features
  if(is.character(clust)) {
    clust <- hclust(as.dist(correlation_distance(t(expression))), method = clust)
  }
  feature_order <- colnames(expression)[clust$order]

  # put cells on one edge with equal width per cell
  linearised <- linearise_cells(
    task$milestone_network,
    task$progressions,
    equal_cell_width = TRUE,
    margin=margin
  )

  # melt expression
  molten <- expression %>%
    reshape2::melt(varnames=c("cell_id", "feature_id"), value.name="expression") %>%
    mutate_if(is.factor, as.character) %>%
    mutate(feature_position = as.numeric(factor(feature_id, feature_order))) %>%
    left_join(linearised$progressions, "cell_id")

  # check importances
  if(!is.null(cell_feature_importances)) {
    molten <- left_join(
      molten,
      cell_feature_importance,
      c("cell_id", "feature_id")
    )
  }

  # plot heatmap
  x_limits <- c(min(linearised$milestone_network$cumstart) - 1, max(linearised$milestone_network$cumend) + 1)
  y_limits <- c(0.5, length(feature_order) + 0.5)

  if (heatmap_type == "tiled") {
    if (is.null(cell_feature_importances)) {
      heatmap <- ggplot(molten) +
        geom_tile(aes(cumpercentage, feature_position, fill=expression)) +
        scale_fill_distiller(palette = "RdBu") +
        scale_x_continuous(NULL, breaks = NULL, expand=c(0, 0), limits=x_limits) +
        scale_y_continuous(NULL, expand=c(0, 0), breaks = seq_along(feature_order), labels=feature_order, position="left", limits=y_limits) +
        theme(legend.position="none", plot.margin=margin(), plot.background = element_blank(), panel.background = element_blank())
    } else {
      heatmap <- ggplot(molten) +
        # geom_tile(aes(cumpercentage, feature_position, alpha=importance), fill="black") +
        geom_rect(aes(xmin=cumpercentage-0.5, xmax=cumpercentage+0.5, ymin=feature_position+scale_minmax(importance)/10*5, ymax=feature_position-scale_minmax(importance)/10*5, fill=expression)) +
        scale_fill_distiller(palette = "RdBu") +
        scale_x_continuous(NULL, breaks = NULL, expand=c(0, 0), limits=x_limits) +
        scale_y_continuous(NULL, expand=c(0, 0), breaks = seq_along(feature_order), labels=feature_order, position="left", limits=y_limits) +
        scale_alpha_continuous(range=c(0, 1)) +
        theme(legend.position="none", plot.margin=margin(), plot.background = element_blank(), panel.background = element_blank())
    }
  } else if (heatmap_type == "dotted") {
    if (is.null(cell_feature_importances)) {
      heatmap <- ggplot(molten) +
        geom_point(aes(cumpercentage, feature_position, color=expression, size=expression)) +
        scale_color_distiller(palette = "RdBu") +
        scale_size_continuous(range=c(0, 6)) +
        scale_x_continuous(NULL, breaks = NULL, expand=c(0, 0), limits=x_limits) +
        scale_y_continuous(NULL, expand=c(0, 0), breaks = seq_along(feature_order), labels=feature_order, position="left", limits=y_limits) +
        theme(legend.position="none", plot.margin=margin(), plot.background = element_blank(), panel.background = element_blank())
    } else {
      heatmap <- ggplot(molten) +
        geom_point(aes(cumpercentage, feature_position, color=expression, size=importance**2)) +
        scale_color_distiller(palette = "RdBu") +
        scale_size_continuous(range=c(0, 6)) +
        scale_x_continuous(NULL, breaks = NULL, expand=c(0, 0), limits=x_limits) +
        scale_y_continuous(NULL, expand=c(0, 0), breaks = seq_along(feature_order), labels=feature_order, position="left", limits=y_limits) +
        theme(legend.position="none", plot.margin=margin(), plot.background = element_blank(), panel.background = element_blank())
    }
  }



  # plot one dim
  onedim <- plot_onedim(
    task,
    milestone_network = linearised$milestone_network,
    progressions = linearised$progressions %>% mutate(percentage = percentage2) %>% select(from, to, cell_id, percentage),
    orientation = -1,
    quasirandom_width = 0,
    margin = margin,
    color_cells = color_cells,
    grouping_assignment = grouping_assignment,
    groups = groups,
    milestone_percentages = milestone_percentages,
    milestones = milestones,
    plot_cells=FALSE
  ) +
    scale_x_continuous(expand=c(0, 0), limits=x_limits) +
    theme(plot.margin=margin())

  # plot dendrogram
  dendrogram <- ggraph::ggraph(as.dendrogram(clust), "dendrogram") +
    ggraph::geom_edge_elbow() +
    scale_x_continuous(limits=c(-0.5, length(feature_order)-0.5), expand=c(0, 0)) +
    scale_y_reverse() +
    coord_flip() +
    theme_graph() +
    theme(plot.margin=margin())

  # plot cell information
  # TODO: Allow multiple cell info here, even "external" which does not fit into grouping_assignment,  milestone_percentages or pseudotime. The current solution is only temporary and ugly!
  if (!is.null(grouping_assignment)) {
    cell_annotation_positions <- linearised$progressions %>%
      add_cell_coloring(
        "grouping",
        grouping_assignment=grouping_assignment
      )
  } else if (!is.null(milestone_percentages)) {
    cell_annotation_positions <- linearised$progressions %>%
      add_cell_coloring(
        "milestone",
        milestone_percentages=milestone_percentages
      )
  }

  cell_annotation <- ggplot(cell_annotation_positions$cell_positions) +
    geom_tile(aes(cumpercentage, 1, fill=color)) +
    cell_annotation_positions$fill_scale +
    scale_x_continuous(expand=c(0, 0), limits=x_limits) +
    theme_graph() +
    theme(legend.position="top")

  patchwork::wrap_plots(
    empty_plot(),
    cell_annotation,
    dendrogram,
    heatmap,
    empty_plot(),
    onedim,
    ncol = 2,
    widths = c(2, 10),
    heights=c(0.5, 10, 2)
  )
}


