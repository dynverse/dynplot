#' @export
select_expression <- function(feature_id) {
  assert_that(!is.null(data))
  assert_that(feature_id %in% attr(data, "data")$dataset$feature_info$feature_id)

  get_expression(attr(data, "data")$dataset, "expression")[attr(data, "data")$cell_info$cell_id, feature_id]
}
