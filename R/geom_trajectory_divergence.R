GeomTrajectoryDivergence <- ggproto(
  "GeomTrajectoryDivergence",
  GeomPolygon,
  default_aes = aesIntersect(aes(fill = "grey90", color = "grey70", linetype = "dashed"), GeomPolygon$default_aes)
)


geom_trajectory_divergence <- function(
  mapping = NULL,
  data = get_divergence_info,
  position = "identity",
  show.legend = NA,
  ...
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~triangle_id))
  layer(data = data, mapping = mapping, stat = StatIdentity,
        geom = GeomTrajectoryDivergence, position = position, show.legend = show.legend,
        inherit.aes = FALSE,
        params = lst(...)
  )
}



get_divergence_info <- function(data) {
  divergence_info <- attr(data, "data")$divergence_polygon_info

  assert_that(all(c("x", "y") %in% colnames(divergence_info)))

  divergence_info
}
