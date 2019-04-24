GeomTrajectoryConnection <- ggproto(
  "GeomTrajectoryConnection",
  GeomSegment,
  default_aes = aesIntersect(aes(fill = "grey90", color = "grey70", linetype = "dashed"), GeomSegment$default_aes)
)

#' Plotting connections between the same milestones in a trajectory
#'
#' @inheritParams ggplot2::geom_segment
#' @param data A function to get the information on connections, typically [get_connection_info()].
#'
#' @export
geom_trajectory_connection <- function(
  mapping = NULL,
  ...,
  data = get_connection_info,
  show.legend = NA
) {
  mapping <- aesIntersect(mapping, aes_(x=~x_from, y=~y_from, xend=~x_to, yend=~y_to))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomTrajectoryConnection,
    position = "identity",
    show.legend = show.legend,
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
