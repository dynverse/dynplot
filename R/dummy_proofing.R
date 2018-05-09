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
    if(!expression_source %in% names(traj)) {stop("Expression source not in traj, did you run add_expression_to_wrapper?")}
    expression <- traj[[expression_source]]
  } else if (is.matrix(expression_source)) {
    expression <- expression_source
  } else {
    stop("Invalid expression_source")
  }
  expression
}
