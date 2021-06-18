#' @importFrom grDevices rgb
mix_colors <- function(milid, milpct, milestone_colors) {
  color_rgb <- apply(milestone_colors[milid,,drop = FALSE], 2, function(x) sum(x * milpct))
  color_rgb[color_rgb < 0] <- 0
  color_rgb[color_rgb > 256] <- 256
  do.call(grDevices::rgb, as.list(c(color_rgb, maxColorValue = 256)))
}
