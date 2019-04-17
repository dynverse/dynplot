#' @examples
#' toy <- dyntoy::generate_dataset()
#' dataset <- dynwrap::infer_trajectory(toy, dynmethods::ti_monocle_ddrtree(), verbose = TRUE)
#' dynplot(dataset) + geom_cell_point() + geom_edge_link(aes(colour = edge_id))
#'
#' @import dynwrap
#' @import ggplot2
#'
#' @export
dynplot <- function(dataset) {
  # if (!dynwrap::is_wrapper_with_dimred(dataset)) {
  #   dimred <- dyndimred::dimred_landmark_mds(dynwrap::get_expression(dataset), ndim = 2, distance_method = "spearman")
  #   dataset <- dataset %>% dynwrap::add_dimred(dimred)
  #  # TODO: also add trajectory segments etc
  # }

  cell_info <-
    bind_cols(
      dataset$cell_info %||% tibble(cell_id = dataset$cell_ids),
      as.data.frame(dataset$dimred[dataset$cell_ids, , drop = FALSE])
    )

  milestone_info <-
    bind_cols(
      tibble(milestone_id = dataset$milestone_ids),
      as.data.frame(dataset$dimred_milestones[dataset$milestone_ids, , drop = FALSE])
    ) %>%
    mutate(
      label = milestone_id
    )

  edge_info <-
    bind_cols(
      dataset$milestone_network
    ) %>%
    mutate(edge_id = paste0(from, "->", to))

  segment_info <-
    bind_cols(
      dataset$dimred_segment_progressions,
      as.data.frame(dataset$dimred_segment_points)
    ) %>%
    mutate(edge_id = paste0(from, "->", to)) %>%
    arrange(edge_id, percentage)

  attr(cell_info, "dataset") <-
    lst(
      cell_info,
      milestone_info,
      edge_info,
      segment_info
    )

  envir <- parent.frame()
  p <- ggplot(data = cell_info, environment = envir)
  # class(p) <- c("dynplot", class(p))
  p
}

aesIntersect <- function(aes1, aes2) {
  structure(
    c(as.list(aes1), aes2[!names(aes2) %in% names(aes1)]),
    class = 'uneval'
  )
}


geom_cell_point <- function(mapping = NULL, data = NULL, position = "identity",
                            show.legend = NA, ...) {
  mapping <- aesIntersect(mapping, aes_(x=~comp_1, y=~comp_2))
  layer(data = data, mapping = mapping, stat = StatIdentity, geom = GeomPoint,
        position = position, show.legend = show.legend, inherit.aes = FALSE,
        params = list(na.rm = FALSE, ...)
  )
}

# geom_cell_hex <- function() {
#
# }
#
# geom_milestone_circle <- function() {
#
# }
#
geom_milestone_label <- function(mapping = NULL, data = get_milestone_info, position = "identity",
                                 show.legend = NA, ...) {
  mapping <- aesIntersect(mapping, aes_(x=~comp_1, y=~comp_2, label=~label))
  layer(data = data, mapping = mapping, stat = StatIdentity, geom = GeomLabel,
        position = position, show.legend = show.legend, inherit.aes = FALSE,
        params = list(na.rm = FALSE, ...)
  )
}

geom_milestone_point <- function(mapping = NULL, data = get_milestone_info, position = "identity",
                                 show.legend = NA, size = 10, ...) {
  mapping <- aesIntersect(mapping, aes_(x=~comp_1, y=~comp_2))
  layer(data = data, mapping = mapping, stat = StatIdentity, geom = GeomPoint,
        position = position, show.legend = show.legend, inherit.aes = FALSE,
        params = list(na.rm = FALSE, size = size, ...)
  )
}
#
# geom_cell_density <- function() {
#
# }

geom_edge_link <- function(mapping = NULL, data = get_segment_info,
                           position = "identity", arrow = NULL, n = 100,
                           lineend = "butt", linejoin = "round", linemitre = 1,
                           label_colour = 'black',  label_alpha = 1,
                           label_parse = FALSE, check_overlap = FALSE,
                           angle_calc = 'rot', force_flip = TRUE,
                           label_dodge = NULL, label_push = NULL,
                           show.legend = NA, ...) {
  mapping <- aesIntersect(mapping, aes_(x=~comp_1, y=~comp_2, group=~edge_id))
  layer(data = data, mapping = mapping, stat = StatIdentity,
        geom = GeomPath, position = position, show.legend = show.legend,
        inherit.aes = FALSE,
        params = list(...)
  )
}

get_milestone_info <- function(data) {
  attr(data, "dataset")$milestone_info
}
get_edge_info <- function(data) {
  attr(data, "dataset")$edge_info
}
get_segment_info <- function(data) {
  attr(data, "dataset")$segment_info
}

