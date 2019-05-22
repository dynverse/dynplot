GeomTrajectorySegments <- ggproto(
  "GeomTrajectorySegments",
  GeomPath,
  default_aes = aesIntersect(aes(linejoin = "mitre", lineend = "square"), GeomPath$default_aes),
  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow_size = 1, shadow = dynplot::shadow(),  ...) {
    original_draw_panel <- GeomPath$draw_panel

    # draw path ---------
    grob_path <- original_draw_panel(data = data, panel_params = panel_params, coord = coord, arrow = NULL, lineend = data$lineend[[1]], linejoin = data$linejoin[[1]], ...)

    # draw arrows -------
    # select the rows at draw_arrow and draw_arrow - 1
    data_arrows <- filter(data, draw_arrow | lead(draw_arrow)) %>%
      mutate(group = ceiling(row_number() / 2)) %>%
      mutate(size = size * arrow_size)

    if (nrow(data_arrows) > 1) {
      grob_arrows <- original_draw_panel(data = data_arrows, panel_params = panel_params, coord = coord, arrow = arrow, lineend = "butt", linejoin = "mitre", ...)
    } else {grob_arrows <- grid::grob()}

    # draw shadows ------
    assert_that(is.list(shadow) || isFALSE(shadow), msg = "shadow should be a list created by shadow() or FALSE")
    if (is.list(shadow)) {
      grob_path_shadow <- original_draw_panel(
        data = data %>% mutate(colour = shadow$color, size = size + shadow$size), panel_params = panel_params, coord = coord, arrow = NULL, lineend = data$lineend[[1]], linejoin = data$linejoin[[1]], ...)
      grob_arrows_shadow <- original_draw_panel(data = data_arrows %>% mutate(colour = shadow$color, size = size + shadow$size), panel_params = panel_params, coord = coord, arrow = arrow, lineend = "butt", linejoin = "mitre", ...)
    } else {
      grob_path_shadow <- grid::grob()
      grob_arrows_shadow <- grid::grob()
    }

    # combine grobs
    grid::gList(
      grob_path_shadow,
      grob_arrows_shadow,
      grob_arrows,
      grob_path
    )
  },
  draw_group = function(data, panel_params, coord, arrow = NULL, arrow_size = 1, shadow = "black") {
  },
  draw_key = function(data, params, size) {
    data$linetype[is.na(data$linetype)] <- 0
    grid::segmentsGrob(
      0.1, 0.5, 0.9, 0.5,
      gp = grid::gpar(col = alpha(data$colour,
      data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype,
      lineend = "butt")
    )
  },
  required_aes = union(GeomPath$required_aes, c("draw_arrow"))
)

#' Plotting individual segments of the trajectory
#'
#' @inheritParams ggplot2::geom_segment
#' @param position_arrow Where to place the arrows within the segments. Typically these are functions created by [position_trajectory_arrows_middle()] or [position_trajectory_arrows_boundaries()].
#' @param arrow_size The size of the arrow relative to the line size.
#' @param shadow Shadow specification as created by [shadow()]
#' @param data A function created by [construct_get_segment_info()].
#'
#' @export
geom_trajectory_segments <- function(
  mapping = NULL,
  position_arrow = position_trajectory_arrows_middle(),
  arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm"), type = "closed"),
  arrow_size = 1,
  shadow = if("colour" %in% names(mapping)) {dynplot::shadow()} else {FALSE},
  ...,
  data = construct_get_segment_info(position_arrow),
  show.legend = NA
  ) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, group=~edge_id, draw_arrow=~draw_arrow))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomTrajectorySegments,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = lst(arrow, arrow_size, shadow = shadow, ...)
  )
}




get_edge_info <- function(data) {
  attr(data, "data")$edge_info
}
construct_get_segment_info <- function(position_arrow = position_trajectory_arrows_middle()) {
  function(data) {
    segment_info <- attr(data, "data")$segment_info
    segment_info <- segment_info %>% position_arrow()

    assert_that(all(c("x", "y", "edge_id", "draw_arrow") %in% colnames(segment_info)))

    segment_info
  }
}


# calculates the length of each part of the trajectory segments
# used to position the arrows nicely
calculate_trajectory_segment_length <- function(data) {
  data %>%
    group_by(from, to) %>%
    mutate(
      length = c(
        0,
        sqrt((head(y, -1) - tail(y, -1))**2 + (head(x, -1) - tail(x, -1))**2)
      ),
      cumlength = cumsum(length)
    )
}


#' Plot arrow in middle
#' @rdname position_trajectory_arrows
position_trajectory_arrows_middle <- function() {
  function(data) {
    data %>%
      calculate_trajectory_segment_length() %>%
      group_by(from, to) %>%
      mutate(
        draw_arrow = (row_number() == which.min(abs(cumlength - last(cumlength)/2))) & row_number() > 1
      ) %>%
      ungroup()
  }
}

#' Plot arrow at certain quantiles, be default at the beginning and end
#' @param quantile The quantile at which to plot an arrow
#' @rdname position_trajectory_arrows
position_trajectory_arrows_boundaries <- function(quantile = 0.3) {
  function(data) {
    data %>%
      calculate_trajectory_segment_length() %>%
      group_by(from, to) %>%
      mutate(
        draw_arrow_start = (row_number() == which.min(abs(cumlength - last(cumlength) * quantile))) & row_number() > 1,
        draw_arrow_end = (row_number() == which.min(abs(cumlength - last(cumlength) * (1-quantile)))) & row_number() > 1,
        draw_arrow = draw_arrow_start | draw_arrow_end
      ) %>%
      ungroup()
  }
}



#' @export
shadow <- function(size = 1, color = "#222222") {
  lst(
    size,
    color
  )
}
