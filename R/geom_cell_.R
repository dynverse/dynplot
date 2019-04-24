GeomCellPoint <- ggproto(
  "GeomCellPoint",
  GeomPoint,
  default_aes = aesIntersect(GeomPoint$default_aes, aes(color = "black"))
)
geom_cell_point <- function(
    mapping = NULL,
    data = get_cell_info_constructor(mapping),
    position = "identity",
    show.legend = NA,
    ...
  ) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomCellPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

geom_cell_hex <- function() {

}

get_cell_info_constructor <- function(mapping) {
  # first parse the mapping to know what to put inside the cell info
  function(data) {
    walk(mapping, function(mapping_element) {
      assign("data", data, envir = environment(mapping$colour))
    })

    attr(data, "data")$cell_info
  }
}
