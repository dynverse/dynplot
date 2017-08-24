milestone_network <- readRDS("net.rds")


#' @import dplyr ggplot2 purrr magrittr
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
    edge <- dyneval:::extract_row_to_list(milestone_network, edge_id)

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

    # first check whether a connection is needed
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

#' @import dplyr ggplot2 purrr
#' @export
plot_connections <- function(milestone_network, orientation=1, plotdata=NULL) {
  if (!is.null(milestone_network)) {
    plotdata <- make_connection_plotdata(milestone_network, orientation)
  }

  max_limit <- if(nrow(plotdata$connections)) {max(plotdata$connections$level)} else {0}

  plot <- ggplot() +
    geom_segment(aes(from_pos, level, xend=to_pos, yend=level, color=edge_id), data=plotdata$states, arrow=arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) +
    geom_segment(aes(from_pos, from_level, xend=from_pos, yend=level), data=plotdata$connections, linetype="longdash") +
    geom_segment(aes(from_pos, level, xend=to_pos, yend=level), data=plotdata$connections, linetype="longdash") +
    geom_segment(aes(to_pos, level, xend=to_pos+0.0001, yend=to_level), data=plotdata$connections, linetype="longdash", arrow=arrow(length=unit(0.30,"cm"), ends="last", type = "closed")) +
    geom_point(aes(from_pos, level, color=edge_id), data=plotdata$states %>% filter(from == first(from))) +
    theme_clean()

  if(orientation == -1) {
    plot <- plot + scale_y_reverse(expand=c(0.1, 0), limits=c(max_limit+0.5, 0))
  } else {
    plot <- plot + scale_y_continuous(expand=c(0.1, 0), limits=c(0, max_limit+0.5))
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

  print(available_levels)

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
