annotate_labels <- function(aes, mappers = list(), legend = TRUE) {
  # add default mappers
  missing_mappers <- setdiff(names(aes), names(mappers))
  mappers <- c(
    mappers,
    default_mappers[missing_mappers]
  )

  lst(
    aes,
    mappers,
    legend
  )
}

annotate_simple <- function(aes, mappers = list(), legend = TRUE) {
  # add default mappers
  missing_mappers <- setdiff(names(aes), names(mappers))
  mappers <- c(
    mappers,
    default_mappers[missing_mappers]
  )

  lst(
    aes,
    mappers,
    legend
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


map_fillcolour <- function(
  x,
  palette = NULL,
  colour_ramp = colorRamp(viridis::viridis(10))
) {
  if(is.character(x)) {
    x <- factor(x)
  }
  if(is.factor(x)) {
    if(is.null(palette)) {
      palette <- RColorBrewer::brewer.pal(9, "Set1")
    }
  }

  if(is.logical(x)) {
    x <- factor(x)
    if(is.null(palette)) {
      palette <- c("#FFFFFF00", "red")
    }
  }

  if(is.factor(x)) {
    assert_that(!is.null(palette))

    n_levels <- length(levels(x))

    # if palette has names, use those as "values" (manual scale), otherwise use the factor ordering
    if(!is.null(names(palette))) {
      palette[as.character(x)]
    } else {
      palette[as.integer(x)]
    }
  } else if (is.numeric(x)) {
    assert_that(!is.null(colour_ramp))

    x <- dynutils::scale_minmax(x)
    colour_ramp(x) %>%
      apply(1, function(x) rgb(x[1]/255, x[2]/255, x[3]/255))
  }
}

map_colour <- map_fillcolour
map_fill <- map_fillcolour
mapper_colour <- function(palette = NULL) {
  purrr::partial(map_fillcolour, !!!lst(palette))
}
mapper_fill <- function(palette = NULL) {
  purrr::partial(map_fillcolour, !!!lst(palette))
}
mapper_show <- function() {
  function(x) {
    map_lgl(x, isTRUE)
  }
}

default_mappers <- list(
  fontface = mapper_fontface(),
  colour = mapper_colour(),
  fill = mapper_fill(),
  show = mapper_show()
)
