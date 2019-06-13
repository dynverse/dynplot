#' @export
#'
layout_heatmap <- function(
  dataset,
  cell_layout = layout_onedim(dataset),
  feature_layout = layout_modules(dataset, feature_modules = get_features(dataset), cell_layout = cell_layout)
) {
  feature_layout$feature_positions$y <- -feature_layout$feature_positions$y - 10

  cell_layout$connection_positions <- cell_layout$connection_positions %>%
    mutate_at(vars("y_from", "y_to"), ~. * 10)

  purrr::list_modify(
    cell_layout,
    !!!feature_layout
  )
}

#' @export
get_features <- function(dataset, n_modules = 5, num_features = 100) {
  feature_importances <- dynfeature::calculate_overall_feature_importance(dataset)
  features_oi <-
    feature_importances %>%
    top_n(num_features, importance) %>%
    pull(feature_id)

  expression <- get_expression(dataset)[, features_oi]

  # determine modules
  clustering <- kmeans(
    Matrix::t(expression),
    centers = n_modules
  )

  feature_modules <- enframe(clustering$cluster, "feature_id", "module_ix")
  feature_modules
}


#' @export
layout_modules <- function(dataset, feature_modules, cell_layout, margin = 0.02) {
  margin <- margin * nrow(feature_modules)

  cell_positions <- cell_layout$cell_positions

  # order genes according to correlation with x of cell positions
  expression <- get_expression(dataset)
  feature_x_correlations <- cor(as.matrix(expression), cell_positions$x)[, 1] %>%
    enframe("feature_id", "x_correlation")

  # determine order of modules
  module_info <- feature_x_correlations %>%
    left_join(feature_modules, "feature_id") %>%
    group_by(module_ix) %>%
    summarise(x_correlation = mean(x_correlation)) %>%
    mutate(
      order = rank(x_correlation)
    ) %>%
    rename(ix = module_ix)

  feature_positions <- feature_modules %>%
    left_join(feature_x_correlations, "feature_id") %>%
    left_join(module_info %>% rename_all(~paste0("module_", .)), "module_ix") %>%
    arrange(module_order, x_correlation) %>%
    mutate(
      y = seq_len(n()) + c(0, cumsum(diff(module_order) * margin))
    ) %>%
    select(feature_id, y)

  lst(
    feature_positions
  )
}
