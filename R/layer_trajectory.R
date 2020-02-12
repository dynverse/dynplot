layer_trajectory <- function(
  current_plot,
  dataset,
  trajectory = dataset,

  plot_trajectory_ = c("none", "segments"),
  color_trajectory = c("none"),
  shadow_trajectory = dynplot2::shadow_defaults(size = 1.5),
  size_trajectory = 1,
  size_trajectory_arrow = 0.5,

  connections = FALSE,

  # info
  grouping = NULL,
  groups = NULL,
  feature_oi = NULL,
  expression_source = "expression",
  pseudotime = NULL,
  color_milestones = NULL,
  milestones = NULL,
  milestone_percentages = NULL
) {
  if(isTRUE(plot_trajectory_)) plot_trajectory_ <- "segments"
  if(isFALSE(plot_trajectory_)) plot_trajectory_ <- "none"
  plot_trajectory_ <- match.arg(plot_trajectory_)

  if(plot_trajectory_ == "segments") {
    current_plot <- current_plot +
      geom_trajectory_segments(shadow = shadow_trajectory, color = "white", size = size_trajectory, arrow_size = size_trajectory_arrow)
  }

  if(connections) {
    current_plot <- current_plot +
      geom_trajectory_connection()
  }
  current_plot
}
