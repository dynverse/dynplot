# TODO: add rescaling, e.g. quantile

ScaleExpressionFillColour <- ggproto(
  "ScaleExpressionFillColour",
  scale_colour_distiller(type = "div", palette = "RdBu"),
  aesthetics = c("fill", "colour"),
  map = function(self, x, limits = self$get_limits()) {
    self$super()$map(x, limits = limits)
  },
  oob = scales::squish
)

scale_expression_fillcolour <- function(...){
  ggproto(
    "",
    ScaleExpressionFillColour,
    ...
  )
}
