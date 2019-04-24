GeomMilestoneLabel <- ggproto(
  "GeomMilestoneLabel",
  GeomLabel,
  default_aes = aesIntersect(aes(fill = "#111111CC", fontface = "bold"), GeomLabel$default_aes)
)

#' Plotting milestones
#'
#' @param mapping Set of aesthetic mappings created by aes().
#' @param data A function created by [get_cell_info_constructor()].
#' @param show.legend Whether to show a legend for this geom
#'
#' @rdname geom_milestone
#'
#' @export
geom_milestone_label <- function(
  mapping = NULL,
  data = get_milestone_info,
  ...,
  show.legend = TRUE
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, label=~label))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomMilestoneLabel,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = lst(
      na.rm = FALSE,
      ...
    )
  )
}

#' @export
geom_milestone_point <- function(
  mapping = NULL,
  data = get_milestone_info,
  position = "identity",
  show.legend = NA,
  size = 10,
  ...
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y))
  layer(data = data, mapping = mapping, stat = StatIdentity, geom = GeomPoint,
        position = position, show.legend = show.legend, inherit.aes = FALSE,
        params = lst(na.rm = FALSE, size = size, ...)
  )
}


get_milestone_info <- function(data) {
  attr(data, "data")$milestone_info
}
