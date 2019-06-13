#' Create a dynplot with a specified layout
#'
#' @param dataset A dynwrap dataset object, typically containing a trajectory
#' @param layout A `layout_*` function from dynplot, such as [layout_dimred()] or [layout_graph()]
#'
#' @return A ggplot2 object, with the processed data in `plot$data` and `attr(plot$data, "data")`
#'
#' @examples
#' toy <- dyntoy::generate_dataset()
#' dynplot(dataset) +
#'   geom_cell_point(aes(colour = select_expression("G1"))) +
#'   scale_expression_fillcolour() +
#'   new_scale_fillcolour() +
#'   geom_trajectory_segments(aes(colour = edge_id))
#'
#' @import dynwrap
#' @import ggplot2
#'
#' @export
dynplot <- function(
  dataset,
  layout = layout_dimred(dataset)
) {
  data <- list(dataset = dataset)

  # cell info ---------------------------------------------------------------
  milestone_id_levels <- dataset$milestone_ids

  cell_info <-
    bind_cols(
      dataset$cell_info %||% tibble(cell_id = dataset$cell_ids)
    ) %>%
    left_join(layout$cell_positions, "cell_id")

  # trajectory --------------------------------------------------------------
  if (dynwrap::is_wrapper_with_trajectory(dataset)) {
    # add milestone percentages to cell info
    cell_info_milestone_percentages <- dataset$milestone_percentages %>%
      mutate(milestone_id = factor(milestone_id, milestone_id_levels)) %>%
      nest(-cell_id, .key = "milestone_percentages") %>%
      deframe()
    cell_info$milestone_percentages <- unname(cell_info_milestone_percentages[cell_info$cell_id])

    # milestone info
    milestone_info <- tibble(
      milestone_id = dataset$milestone_ids
    ) %>%
      mutate(
        label = milestone_id
      ) %>%
      left_join(layout$milestone_positions, "milestone_id") %>%
      mutate(milestone_id = factor(milestone_id, milestone_id_levels))

    # milestone network
    edge_info <-
      bind_cols(
        dataset$milestone_network
      ) %>%
      mutate(edge_id = paste0(from, "->", to), label = edge_id) %>%
      left_join(layout$edge_positions, c("from", "to"))

    data <- c(data, lst(
      milestone_info,
      edge_info
    ))

    if ("segment_progressions" %in% names(layout)) {
      # segment info (produced by layout)
      segment_info <- layout$segment_progressions %>%
        mutate(edge_id = paste0(from, "->", to)) %>%
        arrange(edge_id, percentage) %>%
        left_join(layout$segment_positions, "point_id")

      # get milestone percentages of segments from progressions
      segment_milestone_percentages <- convert_progressions_to_milestone_percentages(
        cell_ids = dataset$cell_ids,
        milestone_ids = dataset$milestone_ids,
        milestone_network = dataset$milestone_network,
        progressions = segment_info %>% mutate(cell_id = point_id)
      ) %>%
        mutate(
          point_id = cell_id,
          milestone_id = factor(milestone_id, milestone_id_levels)
        ) %>%
        select(-cell_id) %>%
        nest(-point_id, .key = "milestone_percentages") %>%
        deframe()
      segment_info$milestone_percentages <- segment_milestone_percentages[segment_info$point_id]

      data$segment_info <- segment_info
    }

    if (all(c("divergence_edge_positions", "divergence_polygon_positions") %in% names(layout))) {
      data$divergence_edge_info <- layout$divergence_edge_positions
      data$divergence_polygon_info <- layout$divergence_polygon_positions
    }

    if ("connection_positions" %in% names(layout)) {
      data$connection_info <- layout$connection_positions
    }
  }


  # features ----------------------------------------------------------------
  if ("feature_positions" %in% names(layout)) {
    data$feature_info <- layout$feature_positions
  }

  # finalise ----------------------------------------------------------------
  data$cell_info <- cell_info
  attr(cell_info, "data") <- data


  # plot --------------------------------------------------------------------
  envir <- parent.frame()
  p <- ggplot(data = cell_info, environment = envir) +
    theme_graph()
  class(p) <- c("dynplot", class(p))
  p
}

aesIntersect <- function(aes1, aes2) {
  structure(
    c(as.list(aes1), aes2[!names(aes2) %in% names(aes1)]),
    class = 'uneval'
  )
}



#' @importFrom stringr str_detect
#' @export
ggplot_build.dynplot <- function(plot) {
  # do some checks for aesthetics

  # check that milestone_percentages mappings have an associated aesthethic
  milestone_percentage_aesthetics <- plot$layers %>%
    map("mapping") %>%
    purrr::flatten() %>%
    map_chr(rlang::quo_text) %>%
    keep(`==`, "milestone_percentages") %>%
    names()

  milestone_percentage_aesthetics_covered <- plot$scales$scales %>%
    keep(~any(str_detect(class(.), "^ScaleMilestone"))) %>%
    map(~.$aesthetics) %>%
    unlist()

  assert_that(
    all(milestone_percentage_aesthetics %in% milestone_percentage_aesthetics_covered),
    msg = "Some aesthetics that are mapped to milestone_percentages do not have an associated scale. Use the scale_milestone_* scales"
  )

  NextMethod()
}
