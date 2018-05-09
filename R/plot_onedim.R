#' Plot onedim
#'
#' @param traj A trajectory or milestone_network
#' @param milestone_network Optional, the milestone network
#' @param progressions The progressions used to put the cells on the graph
#' @param linearised The linearised milestone network and progressions
#' @param quasirandom_width The width of the quasirandom cell spreading
#' @param plot_cells Whether to plot the cells
#'
#' @export
#' @inheritParams add_cell_coloring
#'
#' @importFrom ggrepel geom_label_repel
plot_onedim <- dynutils::inherit_default_params(
  add_cell_coloring,
  function(
    traj=NULL,
    color_cells,
    milestone_network = traj$milestone_network,
    progressions = traj$progressions,
    grouping_assignment,
    groups,
    feature_oi,
    pseudotime,
    expression_source,
    color_milestones,
    milestones,
    milestone_percentages,
    orientation = 1,
    margin = 0.05,
    linearised = linearise_cells(milestone_network, progressions, margin, one_edge = TRUE),
    quasirandom_width = 0.2,
    plot_cells = TRUE
  ) {
    root <- traj$milestone_network$from[[1]]

    linearised <- make_connection_plotdata(linearised)

    # cell positions
    cell_positions <- linearised$progressions %>%
      rename(x = cumpercentage) %>%
      mutate(y = vipor::offsetX(x, edge_id, method="quasirandom", width=quasirandom_width))

    # add cell coloring
    cell_coloring_output <- do.call(add_cell_coloring, map(names(formals(add_cell_coloring)), get, envir=environment()))
    cell_positions <- cell_coloring_output$cell_positions
    fill_scale <- cell_coloring_output$fill_scale

    # get x limit
    max_limit <- if(nrow(linearised$connections)) {max(linearised$connections$level)} else {0}

    # create begin and end states
    states <- tibble(
      milestone_id = unique(c(linearised$milestone_network$from, linearised$milestone_network$to))
    ) %>% mutate(
      start = milestone_id %in% setdiff(linearised$milestone_network$from, linearised$milestone_network$to),
      end = milestone_id %in% setdiff(linearised$milestone_network$to, linearised$milestone_network$from)
    )

    plot <- ggplot() +
      geom_segment(aes(cumstart, 0, xend=cumend, yend=0), data=linearised$milestone_network, color="black") +
      # geom_point(aes(from_pos, level), data=states %>% filter(start), color="black") +
      # geom_point(aes(to_pos, level), data=states %>% filter(end), shape=15, color="black") +
      theme_graph() +
      theme(legend.position="bottom")

    # add connections
    if(nrow(linearised$connections)) {
      plot <- plot + geom_segment(aes(x_from, level, xend=x_to, yend=level), data=linearised$connections, linetype="longdash", color="#666666") +
        geom_segment(aes(x_from, 0, xend=x_from, yend=level), data=linearised$connections, linetype="longdash", color="#666666") +
        geom_segment(aes(x_to, 0, xend=x_to, yend=level), data=linearised$connections, linetype="longdash", color="#666666")
    }

    # add the cells
    if (plot_cells) {
      plot <- plot +
        geom_point(aes(x, y, fill=color), data=cell_positions, shape=21, color="#33333388") +
        fill_scale
    }


    # if (!is.null(cell_progressions)) {
    #   plot <- plot + ggrepel::geom_label_repel(aes(position, 0, label=cell_id, fill = color), data=cell_positions, direction="x", nudge_y=-orientation, min.segment.length=0) + scale_fill_identity()
    #   min_limit <- -1
    # } else {
    min_limit <- -0.2
    # }

    if(orientation == -1) {
      plot <- plot + scale_y_reverse(expand=c(0.1, 0), limits=c(max_limit+0.5, min_limit))
    } else {
      plot <- plot + scale_y_continuous(expand=c(0.1, 0), limits=c(min_limit, max_limit+0.5))
    }

    plot
  }
)


make_connection_plotdata <- function(linearised) {
  connections <- crossing(
    linearised$milestone_network %>% select(from, x_from=cumstart),
    linearised$milestone_network %>% select(to, x_to=cumend)
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
        row_number() < i,
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

  list_modify(linearised, connections=connections)
}
