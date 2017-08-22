#' @export
make_connection_plotdata <- function(milestone_network, orientation = 1) {
  milestone_network <- milestone_network %>% arrange(from, to)

  allmilestones <- unique(c(milestone_network$from, milestone_network$to))

  # these four objects will remember the from and to positions and levels for every milestone, to be used for connections
  milestone_from_poss <- allmilestones %>% {set_names(rep(NA, length(.)), .)}
  milestone_to_poss <- allmilestones %>% {set_names(rep(NA, length(.)), .)}

  milestone_from_levels <- allmilestones %>% {set_names(rep(NA, length(.)), .)}
  milestone_to_levels <- allmilestones %>% {set_names(rep(NA, length(.)), .)}

  states <- tibble(from_pos=numeric(), to_pos=numeric(),  level=integer(), edge_id=integer())
  connections <- tibble(from_pos=numeric(), to_pos=numeric(),  level=integer(), edge_id=integer(), from_level=integer(), to_level=integer())

  margin <- 0.2
  last_edge_to_pos <- 0

  for (edge_id in seq_len(nrow(milestone_network))) {
    edge <- dyneval:::extract_row_to_list(milestone_network, edge_id)

    # STATE EDGE --------------------------------
    edge_from_pos <- if(last_edge_to_pos == 0) 0 else last_edge_to_pos + margin
    edge_to_pos <- edge_from_pos + edge$length
    last_edge_to_pos <- edge_to_pos

    level <- 0

    states <- states %>% add_row(from_pos=edge_from_pos, to_pos=edge_to_pos, level=0, edge_id=edge_id)

    ## CONNECTIONS EDGE(S) -----------------------
    # where will the dashed lines start
    # if the milestones were not already chosen, we will add them
    # otherwise, use the previous positions of the milestones
    if (is.na(milestone_from_poss[[edge$from]])) {
      milestone_from_poss[[edge$from]] <- edge_from_pos
      milestone_from_levels[[edge$from]] <- level
    }
    if (is.na(milestone_to_poss[[edge$to]])) {
      milestone_to_poss[[edge$to]] <- edge_to_pos
      milestone_to_levels[[edge$to]] <- level
    }

    # first check whether a connection is needed
    ## CONNECTION FROM FROM
    if (!is.na(milestone_to_poss[[edge$from]])) {
      connections <- add_connection(
        connection_from_pos = milestone_to_poss[[edge$from]],
        connection_to_pos = edge_from_pos,
        connection_from_level = milestone_to_levels[[edge$from]],
        connection_to_level = level,
        connections = connections
      )
    }

    ## CONNECTION FROM TO
    if (!is.na(milestone_from_poss[[edge$to]])) {
      connections <- add_connection(
        connection_from_pos = edge_to_pos,
        connection_to_pos = milestone_from_poss[[edge$to]],
        connection_from_level = level,
        connection_to_level = milestone_from_levels[[edge$to]],
        connections = connections
      )
    }
  }

  all_edge_ids <- states$edge_id
  states$edge_id <- factor(states$edge_id, levels=all_edge_ids)
  connections$edge_id <- factor(connections$edge_id, levels=all_edge_ids)

  tibble::lst(states, connections)
}

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
    theme_clean()



  if(orientation == -1) {
    plot <- plot + scale_y_reverse(expand=c(0.1, 0), limits=c(max_limit+0.1, 0))
  } else {
    plot <- plot + scale_y_continuous(expand=c(0.1, 0), limits=c(0, max_limit+0.1))
  }

  plot
}
