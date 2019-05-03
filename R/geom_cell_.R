GeomCellPoint <- ggproto(
  "GeomCellPoint",
  GeomPoint,
  default_aes = aesIntersect(GeomPoint$default_aes, aes(color = "grey80"))
)

#' Plotting cells
#'
#' @param mapping Set of aesthetic mappings created by aes().
#' @param data A function created by [construct_get_cell_info()].
#' @param show.legend Whether to show a legend for this geom
#'
#' @rdname geom_cell
#'
#' @export
geom_cell_point <- function(
    mapping = NULL,
    data = construct_get_cell_info(),
    ...,
    show.legend = NA
  ) {
  assign("mapping", mapping, envir = environment(data)) # place the mapping in the data environment

  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomCellPoint,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname geom_cell
#' @export
geom_cell_hex <- function() {

}

construct_get_cell_info <- function() {
  function(data) {
    # first parse the mapping to know what to put inside the cell info
    walk(mapping, function(mapping_element) {
      assign("data", data, envir = environment(mapping$colour))
    })

    attr(data, "data")$cell_info
  }
}
