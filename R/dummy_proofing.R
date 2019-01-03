check_pseudotime <- function(traj, pseudotime) {
  if (is.null(pseudotime)) {
    if (!"pseudotime" %in% names(traj)) {
      message("Pseudotime not provided, will calculate pseudotime from root milestone")
      traj$pseudotime <- dynwrap::calculate_pseudotime(traj)
    }
    traj$pseudotime
  } else {
    pseudotime
  }
}

check_feature <- function(expression, feature_oi) {
  if (is.null(feature_oi)) {stop("Provide feature_oi")}
  if (!feature_oi %in% colnames(expression)) {stop("feature not found in expression")}
  feature_oi
}

check_features_oi <- function(traj, expression, features_oi, cell_feature_importances = NULL) {
  if (length(features_oi) == 1 & is.numeric(features_oi) & features_oi[1] > 0) {
    # make sure features_oi is not larger than the number of features
    if (ncol(expression) < features_oi) {features_oi <- ncol(expression)}

    message("No features of interest provided, selecting the top ", features_oi, " features automatically")

    # choose cell_feauture_importance if given, otherwise choose dynfeature if it is installed, otherwise use more simplistic approach
    if (!is.null(cell_feature_importances)) {
      message("Selecting features with top maximal feature importance across cells")

      features_oi <- cell_feature_importances %>%
        group_by(feature_id) %>%
        summarise(importance = max(importance)) %>%
        top_n(features_oi, importance) %>%
        pull(feature_id)

    } else if ("dynfeature" %in% rownames(installed.packages())) {
      message("Using dynfeature for selecting the top ", features_oi, " features")
      requireNamespace("dynfeature")

      features_oi <- dynfeature::calculate_overall_feature_importance(traj, expression = expression)$feature_id[1:features_oi]
    } else {
      features_oi <- apply(expression, 2, sd) %>% sort() %>% names() %>% tail(features_oi)
    }
  }

  features_oi
}


check_groups <- function(grouping, groups) {
  if (is.null(groups) || !("color" %in% names(groups))) {
    groups <- tibble(
      group_id = unique(grouping),
      color = milestone_palette("auto", length(group_id))
    )
  }
  groups
}

check_milestones <- function(traj, milestones, milestone_percentages = NULL) {
  if (is.null(milestones)) {
    # if the milestones is not defined yet -> create it either based on the trajectory or the milestone percentages
    if (!is.null(milestone_percentages)) {
      tibble(milestone_id = unique(milestone_percentages$milestone_id))
    } else {
      tibble(milestone_id = traj$milestone_ids)
    }
  } else if (!is.data.frame(milestones) && is.character(milestones)) {
    tibble(milestone_id = milestones)
  } else {
    milestones
  }
}
