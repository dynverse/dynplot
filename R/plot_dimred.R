#' Plot trajectory on dimensionality reduction
#'
#' @inheritParams add_cell_coloring
#' @inheritParams add_milestone_coloring
#' @param expression_source Source of the expression
#' @param plot_milestone_network Whether to plot the milestone network
#' @param plot_milestone_labels Whether to overlay the milestone labels over the network
#' @param dimred_method The dimred method or the dimensionality reduction (a dataframe containing at least Comp1, Comp2 and cell_id)
#'
#' @export
plot_dimred <- dynutils::inherit_default_params(
  list(add_cell_coloring,add_milestone_coloring),
  function(
    task,
    color_cells,
    grouping_assignment,
    groups,
    feature_oi,
    color_milestones,
    milestones,
    milestone_percentages,
    pseudotime,
    expression_source = "expression",
    plot_milestone_network = dynwrap::is_wrapper_with_trajectory(task),
    plot_milestone_labels = FALSE,
    dimred_method = ifelse(length(task$cell_ids) > 500, dimred_pca, dimred_mds)
  ) {
    color_cells <- match.arg(color_cells)

    dimred_method <- check_dimred_method(dimred_method)

    expression <- check_expression_source(task, expression_source)

    # get cell positions
    cell_positions <- dimred_method(expression, ndim=2) %>% check_dimred()

    # assign cells to closest milestone
    cell_positions <- left_join(
      cell_positions,
      task$milestone_percentages %>% group_by(cell_id) %>% arrange(desc(percentage)) %>% filter(row_number() == 1) %>% select(cell_id, milestone_id),
      "cell_id"
    )

    # first do milestone coloring, so that these colors can be reused by the cells if necessary
    if (plot_milestone_network) {
      # calculate position of milestones
      milestone_positions <- cell_positions %>%
        group_by(milestone_id) %>%
        summarise_at(c("Comp1", "Comp2"), mean)

      # add missing groups (if no cells were added)
      milestone_positions <- bind_rows(
        map_df(
          setdiff(task$milestone_ids, milestone_positions$milestone_id),
          function(milestone_id) {
            close_milestone_ids <-
              c(
                task$milestone_network %>%
                  filter(from == milestone_id) %>%
                  pull(to),
                task$milestone_network %>%
                  filter(to == milestone_id) %>%
                  pull(from) %>%
                  rep(3)
              )


            milestone_positions %>%
              slice(match(close_milestone_ids, milestone_id)) %>%
              summarise_at(c("Comp1", "Comp2"), mean) %>%
              mutate(milestone_id = !!milestone_id)
          }),
        milestone_positions
      )

      milestone_positions <- add_milestone_coloring(milestone_positions, color_milestones)

      # get milestone network
      milestone_network <- task$milestone_network %>%
        left_join(
          milestone_positions %>% rename_all(~paste0(., "_from")),
          by=c("from" = "milestone_id_from")
        ) %>%
        left_join(
          milestone_positions %>% rename_all(~paste0(., "_to")),
          by=c("to" = "milestone_id_to")
        ) %>%
        mutate(
          Comp1_mid = Comp1_from + (Comp1_to - Comp1_from) /2,
          Comp2_mid = Comp2_from + (Comp2_to - Comp2_from) /2
        )

      milestones <- milestone_positions
    }

    # add cell coloring
    cell_coloring_output <- do.call(add_cell_coloring, map(names(formals(add_cell_coloring)), get, envir=environment()))

    cell_positions <- cell_coloring_output$cell_positions
    fill_scale <- cell_coloring_output$fill_scale

    plot <- ggplot(cell_positions, aes(Comp1, Comp2)) +
      geom_point(aes(fill=color), shape=21, color="#33333388") +
      theme_graph() +
      theme(legend.position="bottom") +
      fill_scale

    if (plot_milestone_network) {
      plot <- plot +
        ggraph::geom_edge_link(aes(x=Comp1_from, y=Comp2_from, xend=Comp1_to, yend=Comp2_to), data=milestone_network) +
        ggraph::geom_edge_link(aes(x=Comp1_from, y=Comp2_from, xend=Comp1_mid, yend=Comp2_mid), data=milestone_network, arrow=arrow(type="closed", length = unit(0.4, "cm")))

      if (plot_milestone_labels) {
        if(color_cells == "milestone") {
          plot <- plot + geom_label(aes(label=milestone_id, fill=color), data=milestone_positions)
        } else {
          plot <- plot + geom_label(aes(label=milestone_id), data=milestone_positions)
        }
      } else {
        if(color_cells == "milestone") {
          plot <- plot +
            geom_point(color="black", data=milestone_positions, size=6) +
            geom_point(aes(fill=color), data=milestone_positions, size=4, shape=21, color="#00000000")
        }
      }

    }

    plot
  }
)
