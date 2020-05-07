rank_features_average <- function(expression_matrix) {
  feature_scores <- sweep(
    expression_matrix,
    2,
    seq_len(ncol(expression_matrix))/ncol(expression_matrix),
    FUN = "*"
  ) %>% rowMeans()
}


rank_features_quantile <- function(expression_matrix, q = 0.9) {
  apply(
    expression_matrix,
    1,
    function(expression) {
      which(expression > quantile(expression, q))[1]
    }
  )
}
