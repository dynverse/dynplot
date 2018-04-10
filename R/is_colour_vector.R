#' @importFrom grDevices colours
is_colour_vector <- function(col) {
  all(col %in% grDevices::colours() | grepl("^#[0-9a-fA-F]{6}$|^#[0-9a-fA-F]{8}$", col))
}

is_color_vector <- is_colour_vector
