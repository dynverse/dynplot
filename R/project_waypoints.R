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
  waypoints$waypoint_network <- waypoints$waypoint_network %>%
    rename(
      milestone_id_from = .data$from_milestone_id,
      milestone_id_to = .data$to_milestone_id
    )

  assert_that(color_trajectory %in% c("nearest", "none"))
  assert_that(setequal(cell_positions$cell_id, colnames(waypoints$geodesic_distances)))

  # project waypoints to dimensionality reduction using kernel and geodesic distances
  weights <- waypoints$geodesic_distances %>% stats::dnorm(sd = trajectory_projection_sd)
  assert_that(all(!is.na(weights)))

  weights <- weights / rowSums(weights)
  positions <- cell_positions %>%
    select(.data$cell_id, .data$comp_1, .data$comp_2) %>%
    slice(match(colnames(weights), .data$cell_id)) %>%
    column_to_rownames("cell_id") %>%
    as.matrix()

  # make sure weights and positions have the same cell_ids in the same order
  assert_that(all.equal(colnames(weights), rownames(positions)))

  # calculate positions
  matrix_to_tibble <- function(x, rownames_column) {
    y <- as_tibble(x)
    y[[rownames_column]] <- rownames(x)
    y
  }

  if (!is.null(edge_positions)) {
    approx_funs <-
      edge_positions %>%
      gather(.data$comp_name, .data$comp_value, starts_with("comp_")) %>%
      group_by(.data$from, .data$to, .data$comp_name) %>%
      summarise(
        approx_fun = {
          pct <- .data$percentage
          cv <- .data$comp_value
          list(function(x) stats::approx(pct, cv, x)$y)
        },
        .groups = "drop"
      )

    waypoint_position <-
      waypoints$progressions %>%
        left_join(approx_funs, by = c("from", "to")) %>%
        mutate(
          comp_value = map2_dbl(.data$approx_fun, .data$percentage, function(f, pct) f(pct))
        ) %>%
        spread(.data$comp_name, .data$comp_value) %>%
        select(.data$waypoint_id, starts_with("comp_")) %>%
        left_join(waypoints$waypoints, "waypoint_id")
  } else {
    waypoint_positions <- (weights %*% positions) %>%
      matrix_to_tibble("waypoint_id") %>%
      left_join(waypoints$waypoints, "waypoint_id")
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
    waypoints$progressions,
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
