#' @include layer_milestones.R layer_trajectory.R layer_cells.R layer_velocity.R layer_contour.R
plot_dendro <- dynutils::inherit_default_params(
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
    plot_velocity = case_when(
      !is.null(dataset$velocity) ~ "grid",
      TRUE ~ "none"
    )
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

    current_plot <- dynplot_dendro(dataset, trajectory = trajectory)

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

      connections = TRUE
    )

    if (plot_milestones == "auto") {
      plot_milestones <- case_when(
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

    current_plot
  })
