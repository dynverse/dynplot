#' Plot a trajectory in a (given) dimensionality reduction
#'
#' @param expression_source Source of the expression
#' @param plot_milestone_network Whether to plot the projected milestone network on the dimensionality reduction
#' @param plot_trajectory Whether to plot the projected trajectory on the dimensionality reduction
#' @param trajectory_projection_sd The standard deviation of the Gaussian kernel to be used for projecting the trajectory.
#'   This is in the order of magnitude as the lengths of the milestone_network.
#'   The lower, the more closely the trajectory will follow the cells.
#' @param alpha_cells The alpha of the cells
#' @param size_cells The size of the cells
#' @param border_radius_percentage The fraction of the radius that is used for the border
#' @param size_milestones The size of the milestones
#' @param size_transitions The size of the trajectory segments
#' @param hex_cells The number of hexes to use, to avoid overplotting points. Default is FALSE if number of cells <= 10000.
#' @param arrow The type and size of arrow in case of directed trajectories. Set to NULL to remove arrow altogether.
#'
#' @inheritParams add_cell_coloring
#' @inheritParams add_milestone_coloring
#' @inheritParams add_density_coloring
#' @inheritParams dynwrap::get_milestone_labelling
#' @inheritParams dynwrap::get_dimred
#' @inheritParams project_waypoints_coloured
#'
#' @importFrom ggforce geom_link2
#'
#' @include add_cell_coloring.R
#' @include add_density_coloring.R
#' @include add_milestone_coloring.R
#' @include project_waypoints.R
#'
#' @keywords plot_trajectory
#'
#' @returns A dimensionality reduction ggplot of the data.
#'
#' @examples
#' data(example_bifurcating)
#' plot_dimred(example_bifurcating)
#'
#' \donttest{
#' # plotting with umap
#' if (requireNamespace("uwot", quietly = TRUE)) {
#'   plot_dimred(example_bifurcating, dimred = dyndimred::dimred_umap)
#' }
#'
#' # using a custom dimred
#' dimred <- dyndimred::dimred_mds(example_bifurcating$expression)
#' plot_dimred(example_bifurcating, dimred = dimred)
#'
#' # coloring cells by pseudotime
#' plot_dimred(example_bifurcating, color_cells = "pseudotime")
#'
#' # coloring cells by cluster
#' plot_dimred(
#'   example_bifurcating,
#'   color_density = "grouping",
#'   grouping = dynwrap::group_onto_nearest_milestones(example_bifurcating)
#' )
#' }
#'
#' @export
plot_dimred <- dynutils::inherit_default_params(
  list(
    add_cell_coloring,
    add_milestone_coloring,
    add_density_coloring,
    project_waypoints_coloured
  ),
  function(
    trajectory,
    color_cells,
    dimred = ifelse(dynwrap::is_wrapper_with_dimred(trajectory), NA, dyndimred::dimred_landmark_mds),
    plot_trajectory = dynwrap::is_wrapper_with_trajectory(trajectory) && !plot_milestone_network,
    plot_milestone_network = FALSE,
    label_milestones = dynwrap::is_wrapper_with_milestone_labelling(trajectory),
    alpha_cells = 1,
    size_cells = 2.5,
    border_radius_percentage = .1,
    size_milestones = 6,
    size_transitions = 2,
    hex_cells = ifelse(length(trajectory$cell_ids) > 10000, 100, FALSE),

    # trajectory information
    grouping,
    groups,
    feature_oi,
    color_milestones,
    milestones,
    milestone_percentages,
    pseudotime,
    expression_source = "expression",
    arrow = grid::arrow(type = "closed", length = unit(0.1, "inches")),

    # density params
    color_density = NULL,
    padding,
    nbins,
    bw,
    density_cutoff,
    density_cutoff_label,

    # project trajectory params
    waypoints,
    trajectory_projection_sd,
    color_trajectory
  ) {
    # make sure a trajectory was provided
    assert_that(dynwrap::is_wrapper_with_trajectory(trajectory))

    color_cells <- match.arg(color_cells)

    # check milestones, make sure it's a data_frame
    milestones <- check_milestones(
      trajectory,
      milestones = milestones,
      milestone_percentages = milestone_percentages
    )

    # get dimensionality reduction from trajectory
    dimred <- get_dimred(
      dataset = trajectory,
      dimred = dimred,
      expression_source = expression_source,
      return_other_dimreds = TRUE
    )
    dimred_extra <- attr(dimred, "extra")
    attr(dimred, "extra") <- NULL

    if (any(is.na(dimred))) {
      dimred[is.na(dimred)] <- mean(dimred, na.rm = TRUE) # replace missing cells with mean position
    }

    # get cell positions
    cell_positions <- dimred %>% as.data.frame() %>% rownames_to_column("cell_id")

    # assign cells to closest milestone
    cell_positions <- left_join(
      cell_positions,
      trajectory$milestone_percentages %>%
        group_by(.data$cell_id) %>%
        arrange(desc(.data$percentage)) %>%
        filter(dplyr::row_number() == 1) %>%
        select(.data$cell_id, .data$milestone_id),
      "cell_id"
    )

    # add milestone colors if necessary
    if ((plot_milestone_network || plot_trajectory) && color_cells == "milestone") {
      if (!"color" %in% milestones) {
        milestones <- add_milestone_coloring(milestones, color_milestones = color_milestones)
      }
    }

    cell_coloring_output <- add_cell_coloring(
      cell_positions = cell_positions,
      color_cells = color_cells,
      trajectory = trajectory,
      grouping = grouping,
      groups = groups,
      feature_oi = feature_oi,
      expression_source = expression_source,
      pseudotime = pseudotime,
      color_milestones = color_milestones,
      milestones = milestones,
      milestone_percentages = milestone_percentages
    )

    cell_positions <- cell_coloring_output$cell_positions

    # calculate density
    density_plots <- add_density_coloring(
      cell_positions = cell_positions,
      color_density = color_density,
      trajectory = trajectory,
      grouping = grouping,
      groups = groups,
      feature_oi = feature_oi,
      expression_source = expression_source,
      padding = padding,
      nbins = nbins,
      bw = bw,
      density_cutoff = density_cutoff,
      density_cutoff_label = density_cutoff_label
    )

    # base plot without cells
    plot <-
      ggplot(cell_positions, aes(.data$comp_1, .data$comp_2)) +
      theme_graph() +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

    # add density polygon
    if (!is.null(density_plots$polygon)) {
      plot <- plot + density_plots$polygon
    }
    if (!is.null(density_plots$scale)) {
      plot <- plot + density_plots$scale
    }

    # add cells
    if (is.numeric(hex_cells)) {
      hex_coordinates <- calculate_hex_coords(cell_positions, hex_cells)

      plot <- plot +
        geom_polygon(
          aes(group = .data$group, fill = .data$color),
          data = hex_coordinates,
        ) +
        cell_coloring_output$fill_scale
    } else {
      if (border_radius_percentage > 0) {
        plot <- plot +
          geom_point(size = size_cells, color = "black")
      }
      if (alpha_cells < 1) {
        plot <- plot +
          geom_point(size = size_cells * (1 - border_radius_percentage), color = "white")
      }
      plot <- plot +
        geom_point(aes(color = .data$color), size = size_cells * (1 - border_radius_percentage), alpha = alpha_cells) +
        cell_coloring_output$color_scale
    }

    # add milestone network if requested
    if (plot_milestone_network) {
      # calculate position of milestones
      milestone_positions <-
        if (!is.null(dimred_extra$dimred_milestones)) {
          dimred_extra$dimred_milestones %>%
            as.data.frame() %>%
            rownames_to_column("milestone_id")
        } else {
          cell_positions %>%
            group_by(.data$milestone_id) %>%
            summarise_at(c("comp_1", "comp_2"), mean)
        }

      # add missing groups (if no cells were added)
      milestone_positions <- bind_rows(
        map_df(
          setdiff(trajectory$milestone_ids, milestone_positions$milestone_id),
          function(milestone_id) {
            close_milestone_ids <-
              c(
                trajectory$milestone_network %>%
                  filter(.data$from == milestone_id) %>%
                  pull(.data$to),
                trajectory$milestone_network %>%
                  filter(.data$to == milestone_id) %>%
                  pull(.data$from) %>%
                  rep(3)
              )

            milestone_positions %>%
              slice(match(close_milestone_ids, .data$milestone_id)) %>%
              summarise_at(c("comp_1", "comp_2"), mean) %>%
              mutate(milestone_oid = !!milestone_id)
          }),
        milestone_positions
      )

      # get milestone network
      milestone_network <-
        trajectory$milestone_network %>%
        left_join(
          milestone_positions %>% rename_all(~paste0(., "_from")),
          by = c("from" = "milestone_id_from")
        ) %>%
        left_join(
          milestone_positions %>% rename_all(~paste0(., "_to")),
          by = c("to" = "milestone_id_to")
        ) %>%
        mutate(
          comp_1_mid = .data$comp_1_from + (.data$comp_1_to - .data$comp_1_from) /2,
          comp_2_mid = .data$comp_2_from + (.data$comp_2_to - .data$comp_2_from) /2
        )

      plot <- plot +
        ggraph::geom_edge_link(
          aes(x = .data$comp_1_from, y = .data$comp_2_from, xend = .data$comp_1_to, yend = .data$comp_2_to),
          data = milestone_network %>% mutate(edge.id = row_number())
        )

      # add arrow if directed
      if (any(trajectory$milestone_network$directed)) {
        plot <- plot +
          ggraph::geom_edge_link(
            aes(x = .data$comp_1_from, y = .data$comp_2_from, xend = .data$comp_1_mid, yend = .data$comp_2_mid),
            data = milestone_network %>% mutate(edge.id = row_number()),
            arrow = arrow
          )
      }

      if (color_cells == "milestone") {
        plot <- plot +
          geom_point(color = "black", data = milestone_positions, size = size_milestones) +
          geom_point(aes(color = .data$color), data = milestone_positions %>% left_join(milestones, "milestone_id"), size = size_milestones*.75)
      } else {
        plot <- plot +
          geom_point(color = "#333333", data = milestone_positions, size = size_milestones, alpha = 1)
      }
    }

    # add trajectory if requested
    if (plot_trajectory) {
      edge_positions <-
        if (!is.null(dimred_extra$dimred_segment_points) && !is.null(dimred_extra$dimred_segment_progressions)) {
          data.frame(dimred_extra$dimred_segment_progressions, dimred_extra$dimred_segment_points)
        } else {
          NULL
        }
      waypoint_projection <- project_waypoints_coloured(
        trajectory = trajectory,
        cell_positions = cell_positions,
        waypoints = waypoints,
        trajectory_projection_sd = trajectory_projection_sd,
        color_trajectory = color_trajectory,
        edge_positions = edge_positions
      )

      wp_segments <- waypoint_projection$segments
      milestone_positions <- wp_segments %>% filter(!is.na(.data$milestone_id))

      # plot milestones
      plot <- plot +
        geom_point(
          data = milestone_positions,
          color = "#333333",
          size = size_milestones
        ) +
        geom_path(
          aes(.data$comp_1, .data$comp_2, group = .data$group),
          data = wp_segments,
          size = size_transitions,
          color = "#333333"
        )

      # add arrow if directed
      if (!is.null(arrow) && any(trajectory$milestone_network$directed)) {
        plot <- plot +
          geom_path(
            aes(.data$comp_1, .data$comp_2, group = .data$group),
            data = wp_segments %>% filter(.data$arrow),
            color = "#333333",
            arrow = arrow,
            size = size_transitions,
            linejoin = "mitre",
            lineend = "butt"
          )

        if (color_trajectory == "none") {
          plot <- plot +
            geom_path(
              aes(.data$comp_1, .data$comp_2, group = .data$group),
              data = wp_segments %>% filter(.data$arrow),
              colour = "#333333",
              arrow = arrow,
              size = size_transitions - 1,
              linejoin = "mitre",
              lineend = "butt"
            )
        } else {
          plot <- plot +
            geom_path(
              aes(.data$comp_1, .data$comp_2, group = .data$group, colour = .data$color),
              data = wp_segments %>% filter(.data$arrow) %>% group_by(.data$group) %>% mutate(color = first(.data$color)) %>% ungroup(),
              arrow = arrow,
              size = size_transitions - 1,
              linejoin = "mitre",
              lineend = "butt"
            )
        }
      }

      # plot segment, depends on whether the trajectory should be colored
      if (color_trajectory == "none") {
        plot <- plot +
          ggforce::geom_link2(
            aes(.data$comp_1, .data$comp_2, group = .data$group),
            colour = "#333333",
            data = wp_segments,
            size = size_transitions - 1,
            alpha = 1
          )+
          geom_point(
            data = milestone_positions,
            color = "#333333",
            size = size_milestones - 1
          )
      } else {
        plot <- plot +
          ggforce::geom_link2(
            aes(.data$comp_1, .data$comp_2, group = .data$group, color = .data$color),
            data = wp_segments,
            size = size_transitions - 1,
            alpha = 1
          ) +
          geom_point(
            aes(color = .data$color),
            data = milestone_positions,
            size = size_milestones - 1
          )
      }
    }

    # add milestone labels
    # the positions of the milestones are calculated in the previous sections
    label_milestones <- get_milestone_labelling(trajectory, label_milestones)
    if ((plot_trajectory || plot_milestone_network) && length(label_milestones)) {
      milestone_labels <- milestone_positions %>%
        mutate(label = label_milestones[.data$milestone_id]) %>%
        filter(!is.na(.data$label))

      plot <- plot + geom_label(aes(label = .data$label), data = milestone_labels)
    }

    # add density labels
    if (!is.null(density_plots$labels)) {
      plot <- plot + density_plots$labels
    }

    plot + coord_equal()
  }
)





# hex_cells = number of hexes in x or y dimensions
calculate_hex_coords <- function(cell_positions, hex_cells) {
  xrange <- range(cell_positions$comp_1)
  yrange <- range(cell_positions$comp_2)

  # expand the smallest range so that both are equal
  shape <- diff(xrange) / diff(yrange) * sqrt(3) / 2 * 1.15
  if (shape > 1) {
    yrange <- c(yrange[1], yrange[2] + diff(yrange) * (shape - 1))
  } else {
    xrange <- c(xrange[1], xrange[2] + diff(xrange) * (1/shape - 1))
  }

  hexbin <- hexbin::hexbin(
    cell_positions$comp_1,
    cell_positions$comp_2,
    IDs = TRUE,
    xbins = hex_cells,
    xbnds = xrange,
    ybnds = yrange,
    shape = 1
  )
  xy <- hexbin::hcell2xy(hexbin, check.erosion = FALSE)

  cell_positions$bin <- hexbin@cID
  bin_positions <- cell_positions %>%
    group_by(.data$bin) %>%
    summarise(color = last(.data$color)) %>%
    mutate(
      comp_1 = xy$x[match(.data$bin, hexbin@cell)],
      comp_2 = xy$y[match(.data$bin, hexbin@cell)]
    )

  hexcoords <- hexbin::hexcoords(
    diff(hexbin@xbnds)/hexbin@xbins / 2,
    diff(hexbin@xbnds)/hexbin@xbins / sqrt(3) / 2
  )

  hex_coords <- tibble(
    comp_1 = rep.int(hexcoords$x, nrow(bin_positions)) + rep(bin_positions$comp_1, each = 6),
    comp_2 = rep.int(hexcoords$y, nrow(bin_positions)) + rep(bin_positions$comp_2, each = 6),
    group = rep(seq_len(nrow(bin_positions)), each = 6),
    color = bin_positions$color[.data$group]
  )

  hex_coords
}
