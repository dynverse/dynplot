create_milestone_legend <- function(milestones) {
  requireNamespace("ComplexHeatmap")
  legend_milestone_id = ComplexHeatmap::Legend(
    title = "Milestones",
    at = milestones$milestone_id,
    legend_gp = grid::gpar(fill=milestones$color),
    labels = milestones$label,
    direction = "horizontal",
    ncol = 3,
    border = "#333333",
    title_position = "topcenter"
  )
}


#' @param plot_milestone_percentages Whether to plot the milestone percentages
annotate_milestone_percentages <- function(
  dataset,
  trajectory = dataset,
  milestones,
  linearised,
  plot_milestone_percentages = c("top", "bottom", "none")
) {
  requireNamespace("ComplexHeatmap")

  plot_milestone_percentages <- match.arg(plot_milestone_percentages)

  milestone_percentages_cellwise <- trajectory$milestone_percentages %>% nest(data = c(milestone_id, percentage)) %>% deframe()

  milestone_colors <- define_milestone_colors(deframe(milestones %>% select(milestone_id, color)), trajectory$milestone_ids)
  col_fun <- function(milestone_percentages) {
    if(is.data.frame(milestone_percentages)) {
      milestone_percentages <- list(milestone_percentages)
    }
    map_chr(milestone_percentages, function(milestone_percentages) {
      color_milestone_percentages(milestone_percentages, milestone_colors)
    })
  }

  annotation_milestone_percentages <- ComplexHeatmap::anno_simple(
    milestone_percentages_cellwise[linearised$progressions$cell_id],
    col = ComplexHeatmap::ColorMapping("Milestone", col_fun = col_fun, breaks = list())
  )

  list(
    annotation_milestone_percentages,
    legend_milestone_id = create_milestone_legend(milestones)
  )
}




get_connections <- function(linearised) {
  milestone_network <- linearised$milestone_network %>%
    mutate(edge_ix = row_number())

  # determine between which nodes a connection should be drawn
  a <- milestone_network %>% select(from, x_from = cumstart, edge_ix_from = edge_ix, n_major_gaps_from = n_major_gaps, n_minor_gaps_from = n_minor_gaps) %>% mutate(edge_end_from = "begin")
  b <- milestone_network %>% select(to, x_to = cumend, edge_ix_to = edge_ix, n_major_gaps_to = n_major_gaps, n_minor_gaps_to = n_minor_gaps) %>% mutate(edge_end_to = "end")
  connections <- crossing(a,b) %>% filter(
    from == to
  ) %>% mutate(
    x_diff = abs(x_to-x_from)
  ) %>% arrange(x_diff) %>%
    mutate(level = NA) %>%
    mutate(direct = near(x_diff, 0))

  # determine level of connections, ordered by x length
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

  connections
}



annotate_milestone_network <- function(
  dataset,
  trajectory = dataset,
  milestones = NULL,
  linearised,
  plot_milestone_network = c("top", "bottom", "none"),
  plot_milestones = c("point", "none", "label"),
  milestone_network_orientation = case_when(
    first(plot_milestone_network) == "bottom" ~ "bottom",
    TRUE ~ "top"
  ),
  milestone_network_arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed"),
  column_major_gap = unit(3, "mm"),
  column_minor_gap = unit(0, "mm")
) {
  requireNamespace("ComplexHeatmap")

  plot_milestone_network <- match.arg(plot_milestone_network)
  plot_milestones <- match.arg(plot_milestones)
  assert_that(all(milestone_network_orientation %in% c("bottom", "top")))
  assert_that(length(milestone_network_orientation) == 1)

  connections <- get_connections(linearised)
  milestone_network <- linearised$milestone_network

  height <- unit(max(connections$level) + 1, "cm")

  annotation_milestone_network = ComplexHeatmap::AnnotationFunction(
    fun = function(index, k, n) {
      ymax <- max(connections$level)

      n = length(index)
      # pushViewport(viewport(xscale = c(0, n), yscale = c(0, ymax + ymax * padding)))
      pushViewport(viewport(xscale = c(0, n), yscale = c(0, ymax), height = height - padding))

      if (milestone_network_orientation == "top") {
        base_y <- unit(0, "native")
        y_multiplier <- 1
      } else {
        base_y <- unit(1, "native")
        y_multiplier <- -1
      }

      # connections
      if (k == 1){
        for(i in seq_len(nrow(connections))) {
          connection <- extract_row_to_list(connections, i)
          x_from <- unit(connection$x_from, "native") + column_major_gap * (connection$n_major_gaps_from) + column_minor_gap * (connection$n_minor_gaps_from)
          x_to <- unit(connection$x_to, "native") + column_major_gap * (connection$n_major_gaps_to) + column_minor_gap * (connection$n_minor_gaps_to)
          y <- unit(connection$level, "native") * y_multiplier + base_y

          connection_gpar <- grid::gpar(lty = "dashed")

          grid.lines(
            unit.c(x_from, x_from, x_to, x_to),
            unit.c(base_y, y, y, base_y),
            default.units = "native",
            gp = connection_gpar
          )
        }
      }

      milestone_edge <- extract_row_to_list(milestone_network, k)

      # edge
      grid.lines(
        unit.c(unit(0, "native"), unit(n, "native")),
        unit.c(base_y, base_y)
      )

      # arrow
      grid.lines(
        unit.c(unit(n*0.49, "native"), unit(n*0.5, "native")),
        unit.c(base_y, base_y),
        arrow = milestone_network_arrow,
        gp = gpar(fill = "#333333")
      )

      # labels of milestones
      if(plot_milestones != "none") {
        milestone_from <- milestones %>% dplyr::filter(milestone_id == milestone_edge$from) %>% extract_row_to_list(1)
        milestone_to <- milestones %>% dplyr::filter(milestone_id == milestone_edge$to) %>% extract_row_to_list(1)

        # draw the from milestone, if there was no minor gap before
        if(milestone_from$label != "") {
          hjust <- ifelse(milestone_edge$minor_gap, 0.5, 0)

          color <- ifelse(sum(col2rgb(milestone_from$color)) < 400, "white", "black")

          grob <- ggplot2:::labelGrob(
            milestone_from$label, unit(0, "native"), base_y, just = c(hjust, 0.5),
            rect.gp = grid::gpar(fill = milestone_from$color),
            text.gp = grid::gpar(col = color)
          )
          grid::grid.draw(grob)
        }

        # draw the to milestone
        if(milestone_to$label != "" && !milestone_edge$minor_gap_next) {
          color <- ifelse(sum(col2rgb(milestone_to$color)) < 400, "white", "black")


          grob <- ggplot2:::labelGrob(
            milestone_to$label, unit(n, "native"), base_y, just= c(1, 0.5),
            rect.gp = grid::gpar(fill = milestone_to$color),
            text.gp = grid::gpar(col = color)
          )
          grid::grid.draw(grob)
        } else {
          grid.circle(
            unit(n, "native"),
            base_y,
            r = unit(1, "mm"),
            gp = gpar(fill = "#333333")
          )
        }
      }

      popViewport()
    },
    var_import = list(
      plot_milestones = plot_milestones,
      milestone_network = milestone_network,
      milestones = milestones,

      connections = connections,

      milestone_network_arrow = milestone_network_arrow,
      milestone_network_orientation = milestone_network_orientation,

      # parameters for positioning
      height = height,
      padding = unit(0.5, "cm"),
      column_major_gap = column_major_gap,
      column_minor_gap = column_minor_gap
    ),
    n = nrow(linearised$progressions),
    subsetable = FALSE,
    height = height
  )

  lst(
    annotation_milestone_network
  )
}
