check_pseudotime <- function(traj, pseudotime) {
  if(is.null(pseudotime)) {
    if(!"pseudotime" %in% names(traj)) {
      message("Pseudotime not provided, will calculate pseudotime from root milestone")
      traj$pseudotime <- dynwrap::calculate_pseudotime(traj)
    }
    traj$pseudotime
  } else {
    pseudotime
  }
}

check_dimred_method <- function(dimred_method) {
  if ("function" %in% class(dimred_method)) {
  } else if (any(c("matrix", "data.frame") %in% class(dimred_method))) {
    dimred <- check_dimred(dimred_method)
    dimred_method <- function(...) {dimred}
  }

  dimred_method
}

check_dimred <- function(dimred) {
  if("matrix" %in% class(dimred)) dimred <- as.data.frame(dimred) %>% rownames_to_column("cell_id")
  if(!"data.frame" %in% class(dimred)) stop("Dimred should be a data.frame")
  if(any(!c("Comp1", "Comp2", "cell_id") %in% colnames(dimred))) {stop("The dimensionality reduction should at least contain Comp1, Comp2 and cell_id")}

  dimred
}


check_feature <- function(expression, feature_oi) {
  if(is.null(feature_oi)) {stop("Provide feature_oi")}
  if(!feature_oi %in% colnames(expression)) {stop("feature not found in expression")}
  feature_oi
}

check_expression_source <- function(traj, expression_source) {
  if (is.character(expression_source)) {
    if(!expression_source %in% names(traj)) {stop("Expression source not in traj, did you run add_expression?")}
    expression <- traj[[expression_source]]
  } else if (is.matrix(expression_source)) {
    expression <- expression_source
  } else {
    stop("Invalid expression_source")
  }
  expression
}


check_features_oi <- function(traj, expression, features_oi, cell_feature_importances=NULL) {
  if (length(features_oi) == 1 & is.numeric(features_oi) & features_oi[1] > 0) {
    # make sure features_oi is not larger than the number of features
    if(ncol(expression) < features_oi) {features_oi <- ncol(expression)}

    message("No features of interest provided, selecting the top ", features_oi, " features automatically")

    # choose cell_feauture_importance if givne, otherwise choose dynfeature if it is installed, otherwise use more simplistic approach
    if (!is.null(cell_feature_importances)) {
      message("Selecting features with top maximal feature importance across cells")

      features_oi <- cell_feature_importances %>%
        group_by(feature_id) %>%
        summarise(importance=max(importance)) %>%
        top_n(features_oi, importance) %>%
        pull(feature_id)

    } else if ("dynfeature" %in% rownames(installed.packages())) {
      message("Using dynfeature for selecting the top ", features_oi, " features")
      requireNamespace("dynfeature")

      features_oi <- dynfeature::calculate_overall_feature_importance(traj, expression=expression)$feature_id[1:features_oi]
    } else {
      features_oi <- apply(expression, 2, sd) %>% sort() %>% names() %>% tail(features_oi)
    }
  }
}
