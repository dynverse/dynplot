sort_by_hue <- function(hex_colors) {
  hues <- grDevices::rgb2hsv(grDevices::col2rgb(hex_colors))[1, ]
  hex_colors[order(hues)]
}




#' @importFrom grDevices rainbow rgb2hsv col2rgb
milestone_palette_list <- list(
  cubeHelix = function(n) {
    requireNamespace("rje")
    rje::cubeHelix(n = n)
  },
  Set3 = function(n) {
    requireNamespace("RColorBrewer")
    cols <- RColorBrewer::brewer.pal(max(3, n), "Set3")[seq_len(n)]
    sort_by_hue(cols)
  },
  rainbow = function(n) {
    grDevices::rainbow(n = n)
  },
  auto = function(n) {
    if (n <= 12) {
      milestone_palette_list$Set3(n)
    } else {
      all_colors <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
      all_colors <- sort_by_hue(all_colors)[-c(1:2)] # sort and remove white/black
      ix <- ceiling(seq(0, length(all_colors), length(all_colors)/(n+1)))
      all_colors[head(ix, -1)]
    }
  }
)

#' @param name The name of the palette. Must be one of \code{"cubeHelix"}, \code{"Set3"}, or \code{"rainbow"}.
#' @param n The number of colours to be in the palette.
#'
#' @rdname get_milestone_palette_names
milestone_palette <- function(name, n) {
  milestone_palette_list[[name]](n)
}

#' Get the names of valid color palettes
#'
#' @keywords plot_helpers
#'
#' @export
get_milestone_palette_names <- function() {
  names(milestone_palette_list)
}
