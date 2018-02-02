#' Make connection plotdata
#'
#' @param milestone_network The milestone network
#' @param orientation ?? TODO: Zouter/wouters
#' @param margin ?? TODO: Zouter/wouters
#'
#' @importFrom dynutils extract_row_to_list
#'
#' @export
make_connection_plotdata <- function(milestone_network, orientation = 1, margin=0.05) {
  allmilestones <- unique(c(milestone_network$from, milestone_network$to))

  margin <- sum(milestone_network$length) * margin

  # these four objects will remember the from and to positions and levels for every milestone, to be used for connections
  milestone_from_poss <- map(allmilestones, ~list()) %>% set_names(allmilestones)
  milestone_to_poss <- map(allmilestones, ~list()) %>% set_names(allmilestones)

  milestone_from_levels <- map(allmilestones, ~list()) %>% set_names(allmilestones)
  milestone_to_levels <- map(allmilestones, ~list()) %>% set_names(allmilestones)

  states <- tibble(from_pos=numeric(), to_pos=numeric(),  level=integer(), edge_id=integer(), from=character(), to=character())
  connections <- tibble(from_pos=numeric(), to_pos=numeric(),  level=integer(), edge_id=integer(), from_level=integer(), to_level=integer())

  last_edge_to_pos <- 0

  for (edge_id in seq_len(nrow(milestone_network))) {
    edge <- dynutils::extract_row_to_list(milestone_network, edge_id)

    # STATE EDGE --------------------------------
    edge_from_pos <- if(last_edge_to_pos == 0) 0 else last_edge_to_pos + margin
    edge_to_pos <- edge_from_pos + edge$length
    last_edge_to_pos <- edge_to_pos

    level <- 0

    states <- states %>% add_row(from_pos=edge_from_pos, to_pos=edge_to_pos, level=0, edge_id=edge_id, from=edge$from, to=edge$to)

    ## CONNECTIONS EDGE(S) -----------------------
    # add positions of these milestones
    milestone_from_poss[[edge$from]] %<>% c(edge_from_pos)
    milestone_from_levels[[edge$from]] %<>% c(level)

    milestone_to_poss[[edge$to]] %<>% c(edge_to_pos)
    milestone_to_levels[[edge$to]] %<>% c(level)

    ## CONNECTION FROM FROM
    for (i in seq_along(milestone_to_poss[[edge$from]])) {
      connections <- add_connection(
        connection_from_pos = milestone_to_poss[[edge$from]][[i]],
        connection_to_pos = edge_from_pos,
        connection_from_level = milestone_to_levels[[edge$from]][[i]],
        connection_to_level = level,
        connections = connections,
        edge_id = edge_id,
        margin = margin
      )
    }

    ## CONNECTION FROM TO
    for (i in seq_along(milestone_from_poss[[edge$to]])) {
      connections <- add_connection(
        connection_from_pos = edge_to_pos,
        connection_to_pos = milestone_from_poss[[edge$to]][[i]],
        connection_from_level = level,
        connection_to_level = milestone_from_levels[[edge$to]][[i]],
        connections = connections,
        edge_id = edge_id,
        margin = margin
      )
    }
  }

  all_edge_ids <- states$edge_id
  states$edge_id <- factor(states$edge_id, levels=all_edge_ids)
  connections$edge_id <- factor(connections$edge_id, levels=all_edge_ids)

  tibble::lst(states, connections)
}

#' Plot connections
#'
#' @param milestone_network The milestone network
#' @param orientation ?? TODO: Zouter/wouters
#' @param plotdata ?? TODO: Zouter/wouters
#' @param margin The margin to add
#' @param cell_progressions Progressions for adding individual cells
#'
#' @export
plot_connections <- function(milestone_network, orientation=1, plotdata=NULL, margin=0.05, cell_progressions=NULL, cell_colors = NULL) {
  if (!is.null(milestone_network)) {
    plotdata <- make_connection_plotdata(milestone_network, orientation, margin=margin)
  }

  # add cells
  if(!is.null(cell_progressions)) {
    cell_positions <- cell_progressions %>% left_join(plotdata$states, by=c("from", "to")) %>%
      mutate(position = from_pos + (to_pos - from_pos) * percentage)

    cell_positions$color <- "white"
    if(!is.null(cell_colors)) {
      cell_positions$color = cell_colors[cell_positions$cell_id]
    }
  }

  # get x limit
  max_limit <- if(nrow(plotdata$connections)) {max(plotdata$connections$level)} else {0}

  plot <- ggplot() +
    geom_segment(aes(from_pos, level, xend=to_pos, yend=level), data=plotdata$connections, linetype="longdash") +
    geom_segment(aes(to_pos, level, xend=to_pos+0.0001, yend=to_level), data=plotdata$connections, linetype="longdash", arrow=arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) +
    geom_segment(aes(from_pos, level, xend=to_pos, yend=level, color=edge_id), data=plotdata$states, arrow=arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) +
    geom_segment(aes(from_pos, from_level, xend=from_pos, yend=level), data=plotdata$connections, linetype="longdash") +
    geom_point(aes(from_pos, level, color=edge_id), data=plotdata$states %>% filter(from == first(from))) +
    geom_point(aes(to_pos, level, color=edge_id), data=plotdata$states %>% filter(!(to %in% from)), shape=15) +
    theme_clean()

  if (!is.null(cell_positions)) {
    requireNamespace("ggrepel")
    plot <- plot + ggrepel::geom_label_repel(aes(position, 0, label=cell_id, fill = color), data=cell_positions, direction="x", nudge_y=-orientation, min.segment.length=0) + scale_fill_identity()
    min_limit <- -1
  } else {
    min_limit <- 0
  }

  if(orientation == -1) {
    plot <- plot + scale_y_reverse(expand=c(0.1, 0), limits=c(max_limit+0.5, min_limit))
  } else {
    plot <- plot + scale_y_continuous(expand=c(0.1, 0), limits=c(min_limit, max_limit+0.5))
  }

  plot
}

add_connection <- function(connection_from_pos, connection_to_pos, connection_from_level, connection_to_level, connections, edge_id, margin) {
  max_bound <- max(connection_from_pos, connection_to_pos)
  min_bound <- min(connection_from_pos, connection_to_pos)

  if (nrow(connections) > 0) {
    available_levels <- connections %>%
      group_by(level) %>%
      summarise(
        available = all((pmax(from_pos, to_pos) < min_bound) | (pmin(from_pos, to_pos) > max_bound))
      ) %>%
      filter(available) %>%
      pull(level)
  } else {
    available_levels <- c(1)
  }

  available_levels <- c(0, available_levels)

  # avoid that connections can go through states
  # check whether the distance is equal to the margin ==> direct level-0 connections are allowed
  if (abs(abs(connection_from_pos - connection_to_pos) - margin) > 0.0001) {
    available_levels <- available_levels[available_levels != connection_from_level]
  }

  if(length(available_levels) > 0) {
    level <- min(available_levels)
  } else {
    level <- max(connections$level) + 1
  }

  connections <- connections %>% add_row(
    from_pos = connection_from_pos,
    to_pos = connection_to_pos,
    from_level = connection_from_level,
    to_level = connection_to_level,
    level = level,
    edge_id = edge_id
  )

  connections
}
