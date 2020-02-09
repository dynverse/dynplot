#' Plot a trajectory on dimensionality reduction
#'
#' @param hex_cells The number of hexes to use, to avoid overplotting points. Default is FALSE if number of cells <= 10000
#' #' @include layer_milestones.R layer_trajectory.R layer_cells.R layer_velocity.R layer_contour.R
plot_dimred <- dynutils::inherit_default_params(
  list(
    layer_cells,
    layer_trajectory,
    layer_milestones,
    layer_velocity,
    layer_contour
  ), function(
    dataset,
    trajectory = dataset,
    color_cells,
    dimred = ifelse(dynwrap::is_wrapper_with_dimred(trajectory), NA, ifelse(length(trajectory$cell_ids) > 500, dimred_pca, dimred_mds)),

    # cells
    size_cells,
    alpha_cells,
    hex_cells,

    # trajectory
    plot_trajectory = "auto",
    plot_milestones = "auto",
    color_milestones,
    color_trajectory,
    shadow_trajectory = dynplot2::shadow_defaults(),
    shadow_milestones = shadow_trajectory,
    size_trajectory,
    size_trajectory_arrow,
    size_milestones,

    # information
    grouping,
    groups,
    feature_oi,
    milestones,
    milestone_percentages,
    pseudotime,
    expression_source = "expression",

    # contour
    plot_contour = "none",
    contour_relative_bandwidth,
    contour_density_cutoff,

    # velocity
    plot_velocity
  ) {

  if(is.null(dataset)) dataset <- trajectory

  dataset <- preprocess_dataset(
    dataset,
    grouping
  )
  trajectory <- preprocess_trajectory(
    trajectory,
    pseudotime
  )

  dimred <- get_dimred(dataset, dimred)

  current_plot <- dynplot_dimred(dataset, trajectory = trajectory, dimred = dimred)

  c(plot_contour, current_plot) %<-% layer_contour(
    current_plot,
    dataset,
    trajectory = trajectory,
    plot_contour_ = plot_contour,
    contour_relative_bandwidth = contour_relative_bandwidth,
    contour_density_cutoff = contour_density_cutoff,
    grouping = grouping,
    groups = groups
  )

  if (first(color_cells) == "auto") {
    color_cells <- case_when(
      first(plot_contour) != "none" ~ "grey",
      TRUE ~ "auto"
    )
  }

  c(color_cells, current_plot, milestones) %<-% layer_cells(
    current_plot,
    dataset,
    trajectory,
    color_cells = color_cells,
    hex_cells = hex_cells,
    size_cells = size_cells,
    alpha_cells = alpha_cells,
    grouping = grouping,
    groups = groups,
    feature_oi = feature_oi,
    expression_source = expression_source,
    pseudotime = pseudotime,
    color_milestones = color_milestones,
    milestones = milestones,
    milestone_percentages = milestone_percentages
  )

  current_plot <- layer_velocity(
    current_plot,
    dataset,
    trajectory = trajectory,
    plot_velocity = plot_velocity
  )

  if (plot_trajectory == "auto") {
    plot_trajectory <- case_when(
      dynwrap::is_wrapper_with_trajectory(trajectory) ~ "segments",
      TRUE ~ "none"
    )
  }

  current_plot <- layer_trajectory(
    current_plot,
    dataset,
    trajectory = trajectory,
    plot_trajectory_ = plot_trajectory,
    color_trajectory = color_trajectory,
    shadow_trajectory = shadow_trajectory,
    size_trajectory = size_trajectory,
    size_trajectory_arrow = size_trajectory_arrow,

    milestones = milestones
  )

  if (plot_milestones == "auto") {
    plot_milestones <- case_when(
      first(plot_contour) != "none" ~ "none",
      !first(color_cells) %in% c("grey", "none", "milestone") ~ "none",
      !dynwrap::is_wrapper_with_trajectory(trajectory) ~ "none",
      dynwrap::is_wrapper_with_milestone_labelling(trajectory) ~ "label",
      TRUE ~ "point"
    )
  }

  c(current_plot, milestones) %<-% layer_milestones(
    current_plot,
    dataset,
    trajectory = trajectory,
    plot_milestones_ = plot_milestones,
    color_milestones = color_milestones,
    shadow_trajectory = shadow_trajectory,
    shadow_milestones = shadow_milestones,
    size_trajectory = size_trajectory,
    size_milestones = size_milestones,

    milestones = milestones
  )

  current_plot <- layer_contour_label(
    current_plot,
    dataset,
    trajectory = trajectory,
    plot_contour_ = plot_contour,
    contour_relative_bandwidth = contour_relative_bandwidth,
    grouping = grouping,
    groups = groups
  )

  current_plot
})
