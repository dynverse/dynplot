GeomExpressionRaster <- ggproto(
  "GeomExpressionRaster",
  GeomRaster,
  default_aes = aesIntersect(GeomRaster$default_aes, aes(color = "grey80"))
)

#' Plotting expression
#'
#' @param mapping Set of aesthetic mappings created by aes().
#' @param data A function created by [construct_get_cell_info()].
#' @param show.legend Whether to show a legend for this geom
#'
#' @rdname geom_cell
#'
#' @export
geom_expression_raster <- function(
  mapping = NULL,
  ...,
  show.legend = NA,
  data = construct_get_expression_info()
) {
  assign("mapping", mapping, envir = environment(data)) # place the mapping in the data environment

  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, fill=~expression))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomExpressionRaster,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

construct_get_expression_info <- function() {
  function(data) {
    feature_info <- attr(data, "data")$feature_info
    cell_info <- attr(data, "data")$cell_info

    expression <- get_expression(attr(data, "data")$dataset)[cell_info$cell_id, feature_info$feature_id]

    expression_info <- reshape2::melt(as.matrix(expression), varnames = c("cell_id", "feature_id"), value.name = "expression") %>%
      mutate(cell_id = as.character(cell_id), feature_id = as.character(feature_id))

    expression_info %>%
      left_join(feature_info, "feature_id") %>%
      left_join(cell_info %>% select(-y), "cell_id")
  }
}
