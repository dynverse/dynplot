#' Default theme for TI plots
#'
#' @param g A ggplot to modify
#' @param id The title
#' @param expand Whether or not to leave space at the borders
#'
#' @export
process_dynplot <- function(g, id, expand = TRUE) {
  gbl <- ggplot_build(g)$layout
  if ("panel_params" %in% names(gbl)) {
    ranges <- gbl$panel_params[[1]]
  } else if ("panel_ranges" %in% names(gbl)) {
    ranges <- gbl$panel_ranges[[1]]
  }
  yrange <- ranges$y.range
  xrange <- ranges$x.range

  xdiff <- diff(xrange)
  ydiff <- diff(yrange)
  maxdiff <- max(xdiff, ydiff)
  new_xrange <- mean(xrange) + c(-maxdiff/2, maxdiff/2)
  new_yrange <- mean(yrange) + c(-maxdiff/2, maxdiff/2)
  new_xrange2 <- mean(xrange) + c(-maxdiff, maxdiff)
  new_yrange2 <- mean(yrange) + c(-maxdiff, maxdiff)

  g +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = .5),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_rect(colour = "black", fill=NA),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.key = element_blank()
    ) +
    ggtitle(id) +
    coord_equal(expand = expand, xlim = new_xrange, ylim = new_yrange) +
    xlim(new_xrange2[[1]], new_xrange2[[2]]) + ylim(new_yrange2[[1]], new_yrange2[[2]]) +
    expand_limits(x = new_xrange, y = new_yrange)
}
