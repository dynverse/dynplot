layer_velocity <- function(
  current_plot,
  dataset,
  trajectory = dataset,
  plot_velocity = case_when(
    !is.null(dataset$velocity) && !is.null(dataset$dimred_future) ~ "stream",
    TRUE ~ "none"
  )
) {
  assert_that(plot_velocity %in% c("stream", "grid", "none"))

  if(plot_velocity == "grid") {
    current_plot <- current_plot +
      geom_velocity_arrow()
  } else if (plot_velocity == "stream") {
    current_plot <- current_plot +
      geom_velocity_stream(alpha = 0.5)
  }
  current_plot
}
