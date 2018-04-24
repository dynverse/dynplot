check_gene <- function(task, gene_oi, expression_source) {
  if(is.null(gene_oi)) {stop("Provide gene_oi")}
  if(!expression_source %in% names(task)) {stop("Expression source not in task, did you run add_expression_to_wrapper?")}
  if(!gene_oi %in% colnames(task[[expression_source]])) {stop("Gene not found in expression_source")}
}


check_pseudotime <- function(task, pseudotime) {
  if(is.null(pseudotime)) {
    if(!"pseudotime" %in% names(task)) {
      message("Pseudotime not provided, will calculate pseudotime from root milestone")
      task$pseudotime <- dynwrap:::calculate_pseudotime(task)
    }
    task$pseudotime
  } else {
    pseudotime
  }
}
