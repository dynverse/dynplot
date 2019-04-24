GeomTrajectoryConnection <- ggproto(
  "GeomTrajectoryConnection",
  GeomSegment,
  default_aes = aesIntersect(aes(fill = "grey90", color = "grey70", linetype = "dashed"), GeomPolygon$default_aes)
)


geom_trajectory_connection <- function(
  mapping = NULL,
  data = get_connection_info,
  position = "identity",
  show.legend = NA,
  ...
) {
  mapping <- aesIntersect(mapping, aes_(x=~x_from, y=~y_from, xend=~x_to, yend=~y_to))
  layer(data = data, mapping = mapping, stat = StatIdentity,
        geom = GeomTrajectoryConnection, position = position, show.legend = show.legend,
        inherit.aes = FALSE,
        params = lst(...)
  )
}



get_connection_info <- function(data) {
  connection_info <- attr(data, "data")$connection_info

  assert_that(!is.null(connection_info), msg = "This layout does not contain information on milestone connections")
  assert_that(all(c("x_from", "y_from", "x_to", "y_to") %in% colnames(connection_info)))

  connection_info
}
