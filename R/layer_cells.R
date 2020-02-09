#' @include add_coloring.R
layer_cells <- dynutils::inherit_default_params(
  add_milestone_coloring,
  function(
    current_plot,
    dataset,
    trajectory = dataset,
    color_cells = c("auto", "none", "grey", "grouping", "feature", "milestone", "pseudotime"),

    hex_cells = ifelse(length(dataset$cell_ids) > 10000, 100, FALSE),
    size_cells = log10(min(10000, length(dataset$cell_ids)))/4,
    alpha_cells = 1,

    grouping = NULL,
    groups = NULL,
    feature_oi = NULL,
    expression_source = "expression",
    pseudotime = NULL,
    color_milestones = NULL,
    milestones = NULL,
    milestone_percentages = NULL
  ) {
  if(is.null(hex_cells)) hex_cells <- FALSE

  # get color cells argument if it is auto
  color_cells <- match.arg(color_cells)
  if (color_cells == "auto") {
    if (!is.null(grouping)) {
      message("Coloring by grouping")
      color_cells <- "grouping"
    } else if (!is.null(feature_oi)) {
      message("Coloring by expression")
      color_cells <- "feature"
    } else if (dynwrap::is_wrapper_with_trajectory(trajectory) | !is.null(milestones) | !is.null(milestone_percentages)) {
      message("Coloring by milestone")
      color_cells <- "milestone"
    } else if (!is.null(pseudotime)) {
      message("Coloring by pseudotime")
      color_cells <- "pseudotime"
    } else {
      color_cells <- "grey"
    }
  }

  # perform checks
  if (color_cells == "grouping") {
    grouping <- get_grouping(dataset, grouping)
    groups <- check_groups(grouping, groups)
  } else if (color_cells == "feature") {
    expression <- get_expression(dataset, expression_source)
    check_feature(expression, feature_oi)
  } else if (color_cells == "milestone") {
    # get milestone percentages if not given
    if (is.null(milestone_percentages)) {
      message("Using milestone_percentages from trajectory")

      assert_that(!is.null(trajectory))
      assert_that(dynwrap::is_wrapper_with_trajectory(trajectory))
      milestone_percentages <- trajectory$milestone_percentages
    }

    # create milestones tibble if not existing
    milestones <- check_milestones(trajectory, milestones, milestone_percentages, check_color = TRUE, color_milestones = color_milestones)

  } else if (color_cells == "pseudotime") {
    pseudotime <- check_pseudotime(trajectory, pseudotime)
    pseudotime <- pseudotime[dataset$cell_ids]
  }

  #
  if(color_cells == "grouping") {
    if (isFALSE(hex_cells)) {
      current_plot <- current_plot +
        ggnewscale::new_scale_color() +
        geom_cell_point(aes(color = grouping), size = size_cells, alpha = alpha_cells)

      current_plot <- current_plot +
        ggnewscale::new_scale_color() +
        get_scale_color_grouping(groups)
    } else {
      current_plot <- current_plot +
        geom_cell_hex(aes(fill = grouping), bins = hex_cells)

      current_plot <- current_plot +
        ggnewscale::new_scale_fill() +
        get_scale_fill_grouping(groups)
    }
  } else if(color_cells == "feature") {
    expression_feature <- expression[dataset$cell_id, feature_oi]
    feature_label <- ifelse(!is.null(names(feature_oi)), names(feature_oi), feature_oi)

    if (isFALSE(hex_cells)) {
      current_plot <- current_plot +
        ggnewscale::new_scale_color() +
        geom_cell_point(aes(color = expression_feature), size = size_cells, alpha = alpha_cells)+
        scale_expression_colour(paste0(feature_label, " expression"))
    } else {
      current_plot <- current_plot +
        ggnewscale::new_scale_fill() +
        geom_cell_hex(aes(fill = expression_feature), bins = hex_cells, size = size_cells, alpha = alpha_cells) +
        scale_expression_fill(paste0(feature_label, " expression"))
    }
  } else if (is_colour_vector(color_cells)) {
    if (isFALSE(hex_cells)) {
      current_plot <- current_plot +
        geom_cell_point(color = color_cells, size = size_cells, alpha = alpha_cells)
    } else {
      current_plot <- current_plot +
        geom_cell_hex(fill = color_cells, bins = hex_cells)

    }
  } else if (color_cells == "milestone") {
    milestone_colors <- set_names(milestones$color, milestones$milestone_id) %>% col2rgb %>% t

    # add cells/hexes
    if (isFALSE(hex_cells)) {
      current_plot <- current_plot +
        ggnewscale::new_scale_color() +
        geom_cell_point(aes(colour = milestone_percentages), size = size_cells, alpha = alpha_cells) +
        scale_milestones_color("Milestone", milestone_colors = milestones %>% select(milestone_id, color) %>% deframe())
    } else {
      current_plot <- current_plot +
        ggnewscale::new_scale_fill() +
        geom_cell_hex(aes(fill = milestone_percentages), bins = hex_cells) +
        scale_milestones_fill("Milestone", milestone_colors = milestones %>% select(milestone_id, color) %>% deframe())
    }
  } else if (color_cells == "pseudotime") {
    # add cells/hexes
    if (isFALSE(hex_cells)) {
      current_plot <- current_plot +
        ggnewscale::new_scale_color() +
        geom_cell_point(aes(color = pseudotime[dataset$cell_ids]), size = size_cells, alpha = alpha_cells) +
        viridis::scale_color_viridis("Pseudotime")

    } else {
      if(current_plot$scales$has_scale("fill")) {current_plot <- current_plot + ggnewscale::new_scale_fill()}
      current_plot <- current_plot +
        ggnewscale::new_scale_color() +
        geom_cell_hex(aes(fill = pseudotime[dataset$cell_ids]), bins = hex_cells) +
        viridis::scale_fill_viridis("Pseudotime")
    }
  }

  list(color_cells, current_plot, milestones)
})
