#' @export

new_scale <- ggnewscale::new_scale

#' @export
new_scale_fillcolour <- function() {list(new_scale("colour"), new_scale("fill"))}
