#' Plot onedim
#'
#' @param linearised The linearised milestone network and progressions
#' @param quasirandom_width The width of the quasirandom cell spreading
#' @param plot_cells Whether to plot the cells
#' @param orientation Whether to plot the connections in the top (1) or bottom (-1)
#' @param alpha_cells The alpha of the cells
#' @param size_cells The size of the cells
#' @param border_radius_percentage The fraction of the radius that is used for the border
#' @param arrow The type and size of arrow in case of directed trajectories. Set to NULL to remove arrow altogether.
#'
#' @inheritParams add_cell_coloring
#' @inheritParams linearise_cells
#' @inheritParams dynwrap::get_milestone_labelling
#'
#' @keywords plot_trajectory
#'
#' @export
#'
#' @importFrom ggrepel geom_label_repel
#'
#' @examples
#' data(example_linear)
#' plot_onedim(example_linear)
#' plot_onedim(example_linear, label_milestones = TRUE)
#'
#' data(example_tree)
#' plot_onedim(example_tree)
plot_onedim <- dynutils::inherit_default_params(
  add_cell_coloring,
  function(
    trajectory,
    color_cells,
    grouping,
    groups,
    feature_oi,
    pseudotime,
    expression_source,
    color_milestones,
    milestones,
    milestone_percentages,
    alpha_cells = 1,
    size_cells = 2.5,
    border_radius_percentage = .1,
    orientation = 1,
    margin = 0.05,
    linearised = linearise_cells(trajectory, margin, one_edge = TRUE),
    quasirandom_width = 0.2,
    plot_cells = TRUE,
    label_milestones = dynwrap::is_wrapper_with_milestone_labelling(trajectory),
    arrow = grid::arrow(type = "closed")
  ) {
    milestone_network <- trajectory$milestone_network
    progressions <- trajectory$progressions

    root <- linearised$milestone_network$from[[1]]

    linearised <- make_connection_plotdata(linearised)

    # cell positions
    cell_positions <- linearised$progressions %>%
      rename(x = cumpercentage) %>%
      mutate(y = vipor::offsetX(x, edge_id, method = "quasirandom", width = quasirandom_width))

    # check milestones, make sure it's a data_frame
    milestones <- check_milestones(trajectory, milestones)

    # add cell coloring
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
    color_scale <- cell_coloring_output$color_scale

    # get x limit
    max_limit <- if (nrow(linearised$connections)) {max(linearised$connections$level)} else {0}

    # create begin and end milestones
    milestones <-
      bind_rows(
        linearised$milestone_network %>% select(milestone_id = from, position = cumstart) %>% mutate(type = "from"),
        linearised$milestone_network %>% select(milestone_id = to, position = cumend) %>% mutate(type = "to")
      ) %>%
      mutate(
        start = milestone_id %in% setdiff(linearised$milestone_network$from, linearised$milestone_network$to),
        end = milestone_id %in% setdiff(linearised$milestone_network$to, linearised$milestone_network$from)
      )

    # add arrow if directed
    my_arrow <-
      if (any(trajectory$milestone_network$directed)) {
        arrow
      } else {
        NULL
      }

    # construct plot
    plot <-
      ggplot() +
      geom_segment(aes(cumstart, 0, xend = cumend, yend = 0), data = linearised$milestone_network, color = "black") +
      theme_graph() +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

    if (any(milestones$start & milestones$type == "from")) {
      plot <- plot +
        geom_segment(aes(position, 0, xend = position+1e-10, yend = 0), data = milestones %>% filter(start, type == "from"), color = "black", arrow = my_arrow)
    }

    if (any(milestones$end & milestones$type == "to")) {
      plot <- plot +
      geom_point(aes(position, 0), data = milestones %>% filter(end, type == "to"), shape = "|", color = "black", size = 10)
    }

    # add connections
    if (nrow(linearised$connections)) {
      plot <- plot +
        geom_segment(aes(x_from, level, xend = x_to, yend = level), data = linearised$connections, linetype = "longdash", color = "#666666") +
        geom_segment(aes(x_from, 0, xend = x_from, yend = level), data = linearised$connections, linetype = "longdash", color = "#666666") +
        geom_segment(aes(x_to, 0, xend = x_to, yend = level), data = linearised$connections, linetype = "longdash", color = "#666666")
    }

    # add the cells
    if (plot_cells) {
      if (border_radius_percentage > 0) {
        plot <- plot +
          geom_point(aes(x, y), size = size_cells, color = "black", data = cell_positions)
      }
      if (alpha_cells < 1) {
        plot <- plot +
          geom_point(aes(x, y), size = size_cells * (1 - border_radius_percentage), color = "white", data = cell_positions)
      }
      plot <- plot +
        geom_point(aes(x, y, color = color), size = size_cells * (1 - border_radius_percentage), alpha = alpha_cells, data = cell_positions) +
        color_scale
    }


    # if (!is.null(cell_progressions)) {
    #   plot <- plot + ggrepel::geom_label_repel(aes(position, 0, label = cell_id, fill = color), data = cell_positions, direction = "x", nudge_y = -orientation, min.segment.length = 0) + scale_fill_identity()
    #   min_limit <- -1
    # } else {
    min_limit <- -0.2
    # }

    # label milestones
    label_milestones <- get_milestone_labelling(trajectory, label_milestones) %>% discard(is.na)

    if (length(label_milestones)) {
      # get for every milestone one position, preferably a "to" position, but if no is available also a "from" position
      milestones_to_label <- milestones %>%
        mutate(label = as.character(label_milestones[milestone_id])) %>%
        filter(!is.na(label)) %>%
        group_by(milestone_id) %>%
        arrange(desc(type)) %>%
        filter(dplyr::row_number() == 1)

      plot <- plot + ggrepel::geom_label_repel(
        aes(position, 0, label = label),
        data = milestones_to_label,
        direction = "x",
        force = 0.8,
        xlim = c(min(milestones$position), max(milestones$position)),
        nudge_y = (max_limit + 0.5) * orientation,
        segment.alpha = 0.3
      )

      max_limit <- max_limit + 1
    }

    if (orientation == -1) {
      plot <- plot + scale_y_reverse(expand = c(0.1, 0), limits = c(max_limit+0.5, min_limit))
    } else {
      plot <- plot + scale_y_continuous(expand = c(0.1, 0), limits = c(min_limit, max_limit+0.5))
    }

    plot
  }
)


make_connection_plotdata <- function(linearised) {
  connections <- crossing(
    linearised$milestone_network %>% select(from, x_from = cumstart),
    linearised$milestone_network %>% select(to, x_to = cumend)
  ) %>% filter(
    from == to,
    x_from != x_to
  ) %>% mutate(
    x_diff = abs(x_to-x_from)
  ) %>% arrange(x_diff) %>%
    mutate(level = NA) %>%
    mutate(direct = near(x_diff, linearised$margin))


  for (i in seq_len(nrow(connections))) {
    connection <- connections %>% extract_row_to_list(i)

    overlapping_connections <- connections %>%
      filter(
        dplyr::row_number() < i,
        pmax(x_from, x_to) > min(connection$x_from, connection$x_to),
        pmin(x_from, x_to) < max(connection$x_from, connection$x_to)
      )

    if (nrow(overlapping_connections)) {
      connections$level[i] <- max(overlapping_connections$level) + 1
    } else {
      if (connections$direct[i]) {
        connections$level[i] <- 0
      } else {
        connections$level[i] <- 1
      }
    }
  }

  list_modify(linearised, connections = connections)
}
