check_pseudotime <- function(trajectory, pseudotime) {
  if (is.null(pseudotime)) {
    if (!"pseudotime" %in% names(trajectory)) {
      message("Pseudotime not provided, will calculate pseudotime from root milestone")
      trajectory$pseudotime <- dynwrap::calculate_pseudotime(trajectory)
    }
    trajectory$pseudotime
  } else {
    pseudotime
  }
}

check_feature <- function(expression, feature_oi) {
  assert_that(!is.null(feature_oi), feature_oi %all_in% colnames(expression))
  feature_oi
}

check_features_oi <- function(trajectory, expression, features_oi, cell_feature_importances = NULL) {
  if (length(features_oi) == 1 && is.numeric(features_oi) && features_oi > 0) {
    # make sure features_oi is not larger than the number of features
    if (ncol(expression) < features_oi) {
      features_oi <- ncol(expression)
    }

    message("No features of interest provided, selecting the top ", features_oi, " features automatically")

    # choose cell_feauture_importance if given, otherwise choose dynfeature if it is installed, otherwise use more simplistic approach
    if (!is.null(cell_feature_importances)) {
      message("Selecting features with top maximal feature importance across cells")

      cell_feature_importances %>%
        group_by(.data$feature_id) %>%
        summarise(importance = max(.data$importance)) %>%
        top_n(features_oi, .data$importance) %>%
        pull(.data$feature_id) %>%
        as.character()

    } else if (requireNamespace("dynfeature", quietly = TRUE)) {
      message("Using dynfeature for selecting the top ", features_oi, " features")
      requireNamespace("dynfeature")

      dynfeature::calculate_overall_feature_importance(trajectory, expression = expression) %>%
        top_n(features_oi, .data$importance) %>%
        pull(.data$feature_id) %>%
        as.character()
    } else {
      apply(expression, 2, stats::sd) %>% sort() %>% names() %>% tail(features_oi)
    }
  } else {
    features_oi
  }
}


check_groups <- function(grouping, groups) {
  if (is.null(groups) || !("color" %in% names(groups))) {
    groups <- tibble(
      group_id = unique(grouping),
      color = milestone_palette("auto", length(.data$group_id))
    )
  }
  groups
}

check_milestones <- function(trajectory, milestones, milestone_percentages = NULL) {
  if (is.null(milestones)) {
    # if the milestones is not defined yet -> create it either based on the trajectory or the milestone percentages
    if (!is.null(milestone_percentages)) {
      tibble(milestone_id = unique(milestone_percentages$milestone_id))
    } else {
      tibble(milestone_id = trajectory$milestone_ids)
    }
  } else if (!is.data.frame(milestones) && is.character(milestones)) {
    tibble(milestone_id = milestones)
  } else {
    milestones
  }
}
