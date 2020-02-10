annotate_labels <- function(aes, mappers = list()) {
  # add default mappers
  missing_mappers <- setdiff(names(aes), names(mappers))
  mappers <- c(
    mappers,
    default_mappers[missing_mappers]
  )

  lst(
    aes,
    mappers
  )
}

annotate_simple <- function(aes, mappers = list()) {
  # add default mappers
  missing_mappers <- setdiff(names(aes), names(mappers))
  mappers <- c(
    mappers,
    default_mappers[missing_mappers]
  )

  lst(
    aes,
    mappers
  )
}

mapper_fontface <- function() {
  map_fontface
}
map_fontface <- function(x) {
  case_when(
    x ~ "bold",
    !x ~ "plain"
  )
}


map_fillcolour <- function(x) {
  if(is.character(x)) {
    x <- factor(x)
  }
  if(is.logical(x)) {
    x <- factor(x)
  }

  if(is.factor(x)) {
    n_levels <- length(levels(x))
    palette <- c("black", RColorBrewer::brewer.pal(max(3, n_levels - 1), "Set1"))

    palette[as.integer(x)]
  } else if (is.numeric(x)) {
    x <- dynutils::scale_minmax(x)
    colour_ramp <- colourRamp(viridis::viridis(10))
    colour_ramp(x)
  }
}

map_colour <- map_fillcolour
map_fill <- map_fillcolour
mapper_colour <- function() {
  map_colour
}
mapper_fill <- function() {
  map_fill
}

default_mappers <- list(
  fontface = mapper_fontface(),
  colour = mapper_colour(),
  fill = mapper_fill()
)
