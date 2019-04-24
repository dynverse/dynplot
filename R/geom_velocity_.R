GeomVelocityArrow <- ggproto(
  "GeomVelocityArrow",
  GeomSegment,
  default_aes = aesIntersect(GeomSegment$default_aes, aes(color = "black"))
)

geom_velocity_arrow <- function(
  mapping = NULL,
  position = position_velocity_grid(),
  data = construct_get_velocity_info(position),
  show.legend = NA,
  arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
  ...
) {
  mapping <- aesIntersect(mapping, aes_(x=~x, y=~y, xend=~x_projected, yend=~y_project))
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomVelocityArrow,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = lst(
      na.rm = FALSE,
      arrow = arrow,
      ...
    )
  )
}


construct_get_velocity_info <- function(position) {
  get_velocity_info <- function(data) {
    cell_positions <- attr(data, "data")$dataset
    assert_that(
      all(c("x", "y", "x_projected", "y_projected") %in% colnames(cell_positions)),
      msg = "This layout does not contain information on velocity"
    )

    position$data(data)
  }
  get_velocity_info
}


#' @param cell_positions Dataframe contains at least x, y, x_projected and y_projected
embed_arrows_cells <- function(cell_positions) {
  cell_positions %>%
    mutate(
      length = sqrt((y_projected - y)**2 + (x_projected - x)**2),
    )
}



#' @param cell_positions The dimensionality reduction which contains at least x, y, x_projected and y_projected
#' @param grid_n Number of rows and columns in the grid
#' @param grid_sd Standard deviation for smoothing arrows
embed_arrows_grid <- function(
  cell_positions,
  grid_n = c(15, 15),
  grid_sd = NULL,
  min_arrow_length = NULL,
  max_arrow_length = NULL,
  min_cell_mass = 1
) {
  grid_w <- grid_n[1]
  grid_h <- grid_n[2]

  # calculate grid points
  range_x <- range(cell_positions[, c("x", "x_projected")])
  range_y <- range(cell_positions[, c("y", "y_projected")])
  grid_x <- seq(range_x[1],range_x[2],length.out=grid_w)
  grid_y <- seq(range_y[1],range_y[2],length.out=grid_h)

  diff_x <- grid_x[2] - grid_x[1]
  diff_y <- grid_y[2] - grid_y[1]

  if(is.null(grid_sd)) {
    grid_sd <- sqrt((diff_x)^2 + (diff_y)^2)/3
  }
  if(is.null(min_arrow_length)) {
    min_arrow_length <- min(c(diff_x, diff_y)) * 0.05
  }
  if(is.null(max_arrow_length)) {
    max_arrow_length <- min(c(diff_x, diff_y))
  }

  cell_positions_difference <- tibble(
    x = cell_positions[,"x_projected"] - cell_positions[,"x"],
    y = cell_positions[,"y_projected"] - cell_positions[,"y"]
  )

  # calculate for each gaussian the smoothed arrow using a gaussian kernel
  garrows <- map_dfr(grid_x,function(x) {
    # cell distances and weights to each grid point
    cd <- sqrt(outer(cell_positions[, "y"],-grid_y,'+')^2 + (x-cell_positions[, "x"])^2)
    cw <- dnorm(cd,sd=grid_sd)

    # calculate the actual arrow
    gw <- Matrix::colSums(cw)
    cws <- pmax(1,Matrix::colSums(cw))
    gxd <- Matrix::colSums(cw*cell_positions_difference$x)/cws
    gyd <- Matrix::colSums(cw*cell_positions_difference$y)/cws

    arrow_length <- sqrt(gxd^2+gyd^2)
    vg <- gw >= min_cell_mass & arrow_length >= min_arrow_length

    tibble(
      x = x,
      y = grid_y[vg],
      x_difference = gxd[vg],
      y_difference = gyd[vg],
      length = arrow_length[vg],
      angle = atan2(y_difference, x_difference)
    )
  })

  # postprocess arrow lengths
  garrows %>%
    mutate(
      norm = max_arrow_length / max(length),
      length = length * norm,
      x_difference = x_difference * norm,
      y_difference = y_difference * norm,
      x_projected = x + x_difference,
      y_projected = y + y_difference,
    )
}


position_velocity_cells <- dynutils::inherit_default_params(
  list(embed_arrows_cells),
  function(...) {
    list(
      data = function(data) {
        embed_arrows_cells(attr(data, "data")$cell_positions, ...)
      }
    )
  }
)

position_velocity_grid <- dynutils::inherit_default_params(
  list(embed_arrows_grid),
  function(...) {
    list(
      data = function(data) {
        embed_arrows_grid(attr(data, "data")$cell_positions, ...)
      }
    )
  }
)
