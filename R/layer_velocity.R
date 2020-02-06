layer_velocity <- function(
  current_plot,
  dataset,
  trajectory = dataset,
  plot_velocity_ = c("none", "grid", "stream")
) {
  if(isTRUE(plot_velocity_)) plot_velocity_ <- "grid"
  if(isFALSE(plot_velocity_)) plot_velocity_ <- "none"
  plot_velocity_ <- match.arg(plot_velocity_)

  if(plot_velocity_ == "grid") {
    current_plot <- current_plot +
      geom_velocity_arrow()
  } else if (plot_velocity_ == "stream") {
    current_plot <- current_plot +
      geom_velocity_stream()
  }
  current_plot
}
