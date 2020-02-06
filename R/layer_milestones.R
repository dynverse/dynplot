layer_milestones <- dynutils::inherit_default_params(
  add_milestone_coloring,
  function(
  current_plot,
  dataset,
  trajectory = dataset,

  plot_milestones_ = c("auto", "none", "label", "point"),
  color_milestones,
  shadow_trajectory = dynplot2::shadow_defaults(size = 1.5),
  shadow_milestones = shadow_trajectory,
  size_trajectory = 2,
  size_milestones = size_trajectory * 3,

  milestones = NULL,
  milestone_percentages = NULL
) {
  if(isTRUE(plot_milestones_)) plot_milestones_ <- "point"
  if(isFALSE(plot_milestones_)) plot_milestones_ <- "none"
  plot_milestones_ <- match.arg(plot_milestones_)

  if (plot_milestones_ != "none") {
    milestones <- check_milestones(trajectory, milestones, milestone_percentages, check_color = TRUE, color_milestones = color_milestones)
    if(plot_milestones_ == "label") {
      current_plot <- current_plot +
        ggnewscale::new_scale_fill() +
        geom_milestone_label(aes(fill = milestone_id), color = "black", size = size_milestones) +
        scale_fill_manual("Milestone", values = milestones %>% select(milestone_id, color) %>% deframe())
    } else if (plot_milestones_ == "point") {
      current_plot <- current_plot +
        ggnewscale::new_scale_color() +
        geom_milestone_point(aes(color = milestone_id), shadow = shadow_milestones, size = size_milestones) +
        scale_color_manual("Milestone", values = milestones %>% select(milestone_id, color) %>% deframe())
    }
  }

  list(current_plot, milestones)
})
