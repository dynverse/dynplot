#' Add colouring to a set of cells.
#'
#' The cells can be coloured by a grouping (clustering), according to a feature
#'   (gene expression), closest milestone, or pseudotime from the root of the trajectory.
#'
#' @param cell_positions The positions of the cells, represented by a tibble.
#'   Must contain column `cell_id` (character) and may contain columns `from`,
#'   `to`, `pseudotime`, depending on the value of `color_cells`.
#' @param color_cells How to color the cells.
#'   * `"auto"`: Try to figure out how to color cells depending on whether
#'     one of the `grouping`, `feature_io`, `milestones` or `pseudotime` parameters are defined.
#'   * `"none"`: Cells are not coloured.
#'   * `"grouping"`: Cells are coloured according to a grouping (e.g. clustering).
#'     Either the `grouping` parameter or `trajectory$grouping` must be a named character vector.
#'   * `"feature"`: Cells are coloured according to the values of a given feature (e.g. gene expression).
#'     Either the `expression_source` parameter or `get_expression(trajectory)` must be a matrix.
#'     Parameter `feature_oi` must also be defined.
#'   * `"milestone"` (recommended): Cells are coloured according their position in the trajectory. The positioning of the
#'     cells are determined by parameter `milestone_percentages` or else by `trajectory$milestone_percentages`. The colours
#'     of the milestones can be determined automatically or can be specified by passing a tibble containing character columns
#'     `milestone_id` and `color` (See `add_milestone_coloring()` for help in constructing this object).
#'   * `"pseudotime"`: Cells are coloured according to the pseudotime value from the root.
#'
#' @param trajectory A dynwrap trajectory.
#' @param grouping A grouping of the cells (e.g. clustering) as a named character vector.
#' @param groups A tibble containing character columns `group_id` and `color`. If `NULL`, this object is inferred from the `grouping` itself.
#' @param feature_oi The name of a feature to use for colouring the cells.
#' @param expression_source Source of the feature expression, defaults to `get_expression(trajectory)`.
#' @param pseudotime The pseudotime from the root of the trajectory to the cells as a named numeric vector.
#' @param milestone_percentages The milestone percentages.
#'
#' @returns
#' A named list with following objects:
#'  * cell_positions: The `trajectory$progressions` object with a `color` column added.
#'  * color_scale: A ggplot colour scale to add to the downstream ggplot.
#'  * fill_scale: A ggplot fill scale to add to the downstream ggplot.
#'  * color_cells: The input `color_cells` value, except `"auto"` will have been replaced depending
#'    on which other parameters were passed.
#'
# @examples
# add_cell_coloring(example_bifurcating$progressions)
#'
#' @inheritParams add_milestone_coloring
#'
#' @include add_milestone_coloring.R
add_cell_coloring <- dynutils::inherit_default_params(
  add_milestone_coloring,
  function(
    cell_positions,
    color_cells = c("auto", "none", "grouping", "feature", "milestone", "pseudotime"),
    trajectory,
    grouping = NULL,
    groups = NULL,
    feature_oi = NULL,
    expression_source = "expression",
    pseudotime = NULL,
    color_milestones = NULL,
    milestones = NULL,
    milestone_percentages = NULL
  ) {
    # check cell coloration
    color_cells <- match.arg(color_cells)
    if (color_cells == "auto") {
      if (!is.null(grouping)) {
        message("Coloring by grouping")
        color_cells <- "grouping"
      } else if (!is.null(feature_oi)) {
        message("Coloring by expression")
        color_cells <- "feature"
      } else if (!is.null(milestones) | !is.null(milestone_percentages)) {
        message("Coloring by milestone")
        color_cells <- "milestone"
      } else if (!is.null(pseudotime)) {
        message("Coloring by pseudotime")
        color_cells <- "pseudotime"
      } else {
        color_cells <- "grey"
      }
    }
    if (color_cells == "grouping") {
      grouping <- get_grouping(trajectory, grouping)
    } else if (color_cells == "feature") {
      expression <- get_expression(trajectory, expression_source)
      check_feature(expression, feature_oi)
    } else if (color_cells == "milestone") {
      if (is.null(milestone_percentages)) {
        message("Using milestone_percentages from trajectory")
        milestone_percentages <- trajectory$milestone_percentages
      }
      # TODO more checks
    } else if (color_cells == "pseudotime") {
      pseudotime <- check_pseudotime(trajectory, pseudotime)
      cell_positions$pseudotime <- pseudotime[cell_positions$cell_id]
    }

    # now create the actual coloring
    if (color_cells == "grouping") {
      groups <- check_groups(grouping, groups)

      cell_positions$color <- grouping[match(cell_positions$cell_id, names(grouping))]

      color_scale <- scale_color_manual(color_cells, values = set_names(groups$color, groups$group_id), guide = guide_legend(ncol = 5))
      fill_scale <- scale_fill_manual(color_cells, values = set_names(groups$color, groups$group_id), guide = guide_legend(ncol = 5))

    } else if (color_cells == "feature") {
      cell_positions$color <- expression[cell_positions$cell_id, feature_oi]
      color_scale <- scale_color_distiller(paste0(feature_oi, " expression"), palette = "RdYlBu")
      fill_scale <- scale_fill_distiller(paste0(feature_oi, " expression"), palette = "RdYlBu")
    } else if (is_colour_vector(color_cells)) {
      cell_positions$color <- "trajectories_are_awesome"
      color_scale <- scale_color_manual(NULL, values = c("trajectories_are_awesome" = color_cells), guide = "none")
      fill_scale <- scale_fill_manual(NULL, values = c("trajectories_are_awesome" = color_cells), guide = "none")
    } else if (color_cells == "milestone") {
      if (is.null(milestones)) {
        assert_that(
          milestone_percentages$milestone_id %in% trajectory$milestone_ids,
          msg = "Not all milestones were found in milestones tibble. Supply milestones tibble if supplying milestone_percentages separately."
        )
        milestones <- tibble(milestone_id = trajectory$milestone_ids)
      }
      if (!"color" %in% names(milestones)) {
        milestones <- milestones %>% add_milestone_coloring(color_milestones)
      }

      milestone_colors <- set_names(milestones$color, milestones$milestone_id) %>% col2rgb %>% t

      cell_colors <-
        milestone_percentages %>%
        group_by(.data$cell_id) %>%
        summarise(color = mix_colors(.data$milestone_id, .data$percentage, milestone_colors))

      cell_positions <- left_join(cell_positions, cell_colors, "cell_id")

      color_scale <- scale_color_identity(NULL, guide = "none")
      fill_scale <- scale_fill_identity(NULL, guide = "none")
    } else if (color_cells == "pseudotime") {
      cell_positions$color <- cell_positions$pseudotime
      # color_scale <- viridis::scale_color_viridis("pseudotime")
      # fill_scale <- viridis::scale_fill_viridis("pseudotime")
      color_scale <- ggplot2::scale_color_viridis_c("pseudotime")
      fill_scale <- ggplot2::scale_fill_viridis_c("pseudotime")
    } else if (color_cells == "none") {
      cell_positions$color <- "black"
      color_scale <- scale_color_identity()
      fill_scale <- scale_fill_identity()
    }

    lst(cell_positions, color_scale, fill_scale, color_cells)
  }
)
