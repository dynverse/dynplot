#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices rainbow
#' @importFrom shades hue
#' @importFrom rje cubeHelix
milestone_palette_list <- list(
  cubeHelix = function(n) rje::cubeHelix(n = n),
  Set3 = function(n) {
    cols <- RColorBrewer::brewer.pal(max(3, n), "Set3")[seq_len(n)]
    cols[order(shades::hue(cols))]
  },
  rainbow = function(n) grDevices::rainbow(n = n),
  auto = function(n) {
    if (n <= 12) {
      milestone_palette_list$Set3(n)
    } else {
      milestone_palette_list$cubeHelix(n)
    }
  }
)

#' Wrapper for various palettes
#'
#' @param name The name of the palette. Must be one of \code{"cubeHelix"}, \code{"Set3"}, or \code{"rainbow"}.
#' @param n The number of colours to be in the palette.
milestone_palette <- function(name, n) {
  milestone_palette_list[[name]](n)
}
