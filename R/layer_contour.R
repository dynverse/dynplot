layer_contour <- function(
  current_plot,
  dataset,
  trajectory = dataset,
  plot_contour_ = c("none", "grouping"),
  grouping = NULL,
  groups = NULL,
  contour_relative_bandwidth = 0.2,
  contour_density_cutoff = 0.2,
  contour_background_alpha = 0.3
) {
  if(isTRUE(plot_contour_)) plot_contour_ <- "grouping"
  if(isFALSE(plot_contour_)) plot_contour_ <- "none"
  plot_contour_ <- match.arg(plot_contour_)

  if(plot_contour_ == "grouping") {
    grouping <- get_grouping(dataset, grouping)
    groups <- check_groups(grouping, groups)

    current_plot <- current_plot +
      ggnewscale::new_scale_fill() +
      geom_cell_contour(
        aes(group = group_id, fill = group_id),
        relative_bandwidth = contour_relative_bandwidth,
        relative_density_cutoff = contour_density_cutoff,
        alpha = contour_background_alpha
      ) +
      get_scale_fill_grouping(groups)
  }

  list(plot_contour_, current_plot)
}




layer_contour_label <- function(
  current_plot,
  dataset,
  trajectory = dataset,
  plot_contour_ = c("none", "grouping"),
  grouping,
  groups,
  contour_relative_bandwidth = 0.2
) {
  if(isTRUE(plot_contour_)) plot_contour_ <- "grouping"
  if(isFALSE(plot_contour_)) plot_contour_ <- "none"
  plot_contour_ <- match.arg(plot_contour_)

  if(plot_contour_ == "grouping") {
    grouping <- get_grouping(dataset, grouping)
    groups <- check_groups(grouping, groups)

    current_plot <- current_plot +
      ggnewscale::new_scale_fill() +
      geom_cell_contour_label(aes(group = group_id, label = group_id, fill = group_id), relative_bandwidth = contour_relative_bandwidth, show.legend = FALSE) +
      get_scale_fill_grouping(groups)
  }
  current_plot
}
