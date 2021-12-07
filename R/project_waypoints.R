#' Project the waypoints
#'
#' @inheritParams add_cell_coloring
#' @param cell_positions The positions of the cells in 2D. Must be a tibble with character column `cell_id` and numeric columns `comp_1` and `comp_2`.
#' @param waypoints The waypoints to use for projecting. Can by generated using [dynwrap::select_waypoints()].
#' @param trajectory_projection_sd The standard deviation of the gaussian kernel.
#' @param color_trajectory How to color the trajectory, can be "nearest" for coloring to nearest cell, or "none".
#' @param edge_positions The positions of the edges.
#'
#' @returns
#' A named list containing items:
#' * segments: A tibble containing columns `comp_1` (numeric), `comp_2` (numeric), `waypoint_id` (character), `milestone_id` (character), `from` (character), `to` (character)
#'   `percentage` (numeric), `group` (factor), and `arrow` (logical).
#'
# @examples
# pca <- prcomp(example_bifurcating$expression, rank. = 2)$x
# cell_positions <- pca %>% as.matrix %>% as.data.frame %>% magrittr::set_colnames(c("comp_1", "comp_2")) %>% rownames_to_column("cell_id")
# project_waypoints_coloured(example_bifurcating, cell_positions)
project_waypoints_coloured <- function(
  trajectory,
  cell_positions,
  edge_positions = NULL,
  waypoints = dynwrap::select_waypoints(trajectory),
  trajectory_projection_sd = sum(trajectory$milestone_network$length) * 0.05,
  color_trajectory = "none"
) {
  wps <- waypoints
  wps$waypoint_network <- wps$waypoint_network %>%
    rename(
      milestone_id_from = .data$from_milestone_id,
      milestone_id_to = .data$to_milestone_id
    )

  assert_that(color_trajectory %in% c("nearest", "none"))
  assert_that(setequal(cell_positions$cell_id, colnames(wps$geodesic_distances)))

  # calculate positions
  waypoint_positions <-
    if (!is.null(edge_positions)) {
      comp_names <- colnames(edge_positions) %>% keep(function(x) grepl("comp_", x))

      wps$progressions %>%
        select(.data$from, .data$to) %>%
        unique() %>%
        pmap_df(function(from, to) {
          wp_progr <- wps$progressions %>% filter(.data$from == !!from, .data$to == !!to)
          edge_pos <- edge_positions %>% filter(.data$from == !!from, .data$to == !!to)
          for (cn in comp_names) {
            wp_progr[[cn]] <- approx(edge_pos$percentage, edge_pos[[cn]], wp_progr$percentage)$y
          }
          wp_progr
        }) %>%
        select(.data$waypoint_id, !!comp_names) %>%
        left_join(wps$waypoints, "waypoint_id")
    } else {
      # project wps to dimensionality reduction using kernel and geodesic distances
      weights <- wps$geodesic_distances %>% stats::dnorm(sd = trajectory_projection_sd)
      assert_that(all(!is.na(weights)))

      weights <- weights / rowSums(weights)
      positions <- cell_positions %>%
        select(.data$cell_id, .data$comp_1, .data$comp_2) %>%
        slice(match(colnames(weights), .data$cell_id)) %>%
        column_to_rownames("cell_id") %>%
        as.matrix()

      # make sure weights and positions have the same cell_ids in the same order
      assert_that(all.equal(colnames(weights), rownames(positions)))

      (weights %*% positions) %>%
        as.data.frame() %>%
        rownames_to_column("waypoint_id") %>%
        left_join(wps$waypoints, "waypoint_id") %>%
        as_tibble()
    }


  # add color of closest cell
  if (color_trajectory == "nearest") {
    assert_that("color" %in% colnames(cell_positions))

    cpv <- cell_positions %>% select(.data$cell_id, .data$color) %>% deframe()
    waypoint_positions <- waypoint_positions %>%
      mutate(
        closest_cell_ix = (weights %>% apply(1, which.max))[.data$waypoint_id],
        closest_cell_id = colnames(weights)[.data$closest_cell_ix],
        color = cpv[.data$closest_cell_id]
      )
  }

  segments <- left_join(
    waypoint_positions,
    wps$progressions,
    by = "waypoint_id"
  ) %>%
    mutate(group = factor(paste0(.data$from, "---", .data$to))) %>%
    group_by(.data$group) %>%
    mutate(
      closest = which.min(abs(.data$percentage - 0.5)),
      arrow = row_number() == .data$closest | row_number() - 1 == .data$closest
    ) %>%
    ungroup() %>%
    select(-.data$closest)

  lst(
    segments = segments
  )
}
