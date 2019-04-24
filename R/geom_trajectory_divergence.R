GeomTrajectoryDivergence <- ggproto(
  "GeomTrajectoryDivergence",
  GeomPolygon,
  default_aes = aesIntersect(aes(fill = "grey90", color = "grey70", linetype = "dashed"), GeomPolygon$default_aes)
)

#' Plotting the divergence regions of a trajectory
#'
#' @inheritParams ggplot2::geom_polygon
#' @param data A function to get the information on divergences, typically [get_divergence_info()].
#'
#' @export
geom_trajectory_divergence <- function(
  mapping = NULL,
  ...,
  data = get_divergence_info,
  show.legend = NA
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~triangle_id))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomTrajectoryDivergence,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = lst(...)
  )
}



get_divergence_info <- function(data) {
  divergence_info <- attr(data, "data")$divergence_polygon_info

  assert_that(all(c("x", "y") %in% colnames(divergence_info)))

  divergence_info
}
