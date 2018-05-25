#' Plot trajectory on dimensionality reduction
#'
#' @param expression_source Source of the expression
#' @param plot_milestone_network Whether to plot the milestone network
#'
#' @inheritParams add_cell_coloring
#' @inheritParams add_milestone_coloring
#' @inheritParams add_density_coloring
#' @inheritParams dynwrap::get_milestone_labelling
#' @inheritParams dynwrap::get_dimred
#'
#' @export
plot_dimred <- dynutils::inherit_default_params(
  list(
    add_cell_coloring,
    add_milestone_coloring,
    add_density_coloring
  ),
  function(
    traj,
    color_cells,
    color_density = NULL,
    grouping_assignment,
    groups,
    feature_oi,
    color_milestones,
    milestones,
    milestone_percentages,
    pseudotime,
    expression_source = "expression",
    plot_milestone_network = dynwrap::is_wrapper_with_trajectory(traj),
    label_milestones = dynwrap::is_wrapper_with_milestone_labelling(traj),
    dimred = ifelse(dynwrap::is_wrapper_with_dimred(traj), NA, ifelse(length(traj$cell_ids) > 500, dimred_pca, dimred_mds)),
    padding,
    nbins,
    bw,
    density_cutoff,
    density_cutoff_label
  ) {
    color_cells <- match.arg(color_cells)

    expression <- get_expression(traj, expression_source)
    dimred <- get_dimred(traj, dimred, expression)

    # get cell positions
    cell_positions <- dimred %>% as.data.frame() %>% rownames_to_column("cell_id")

    # assign cells to closest milestone
    cell_positions <- left_join(
      cell_positions,
      traj$milestone_percentages %>% group_by(cell_id) %>% arrange(desc(percentage)) %>% filter(row_number() == 1) %>% select(cell_id, milestone_id),
      "cell_id"
    )

    # first do milestone coloring, so that these colors can be reused by the cells if necessary
    if (plot_milestone_network) {
      # calculate position of milestones
      milestone_positions <- cell_positions %>%
        group_by(milestone_id) %>%
        summarise_at(c("comp_1", "comp_2"), mean)

      # add missing groups (if no cells were added)
      milestone_positions <- bind_rows(
        map_df(
          setdiff(traj$milestone_ids, milestone_positions$milestone_id),
          function(milestone_id) {
            close_milestone_ids <-
              c(
                traj$milestone_network %>%
                  filter(from == milestone_id) %>%
                  pull(to),
                traj$milestone_network %>%
                  filter(to == milestone_id) %>%
                  pull(from) %>%
                  rep(3)
              )


            milestone_positions %>%
              slice(match(close_milestone_ids, milestone_id)) %>%
              summarise_at(c("comp_1", "comp_2"), mean) %>%
              mutate(milestone_id = !!milestone_id)
          }),
        milestone_positions
      )

      milestone_positions <- add_milestone_coloring(milestone_positions, color_milestones)

      # get milestone network
      milestone_network <- traj$milestone_network %>%
        left_join(
          milestone_positions %>% rename_all(~paste0(., "_from")),
          by=c("from" = "milestone_id_from")
        ) %>%
        left_join(
          milestone_positions %>% rename_all(~paste0(., "_to")),
          by=c("to" = "milestone_id_to")
        ) %>%
        mutate(
          comp_1_mid = comp_1_from + (comp_1_to - comp_1_from) /2,
          comp_2_mid = comp_2_from + (comp_2_to - comp_2_from) /2
        )

      milestones <- milestone_positions
    }

    cell_coloring_output <- do.call(add_cell_coloring, map(names(formals(add_cell_coloring)), get, envir=environment()))

    cell_positions <- cell_coloring_output$cell_positions
    color_scale <- cell_coloring_output$color_scale

    # base plot without cells
    plot <- ggplot(cell_positions, aes(comp_1, comp_2)) +
      theme_graph() +
      theme(legend.position="bottom")

    # add density
    if (!is.null(color_density)) {
      plot <- do.call(add_density_coloring, map(names(formals(add_density_coloring)), get, envir=environment()))
    }


    # add cells
    plot <- plot +
      geom_point(size=2.5, color="black") +
      geom_point(aes(color=color), size=2) +
      color_scale

    if (plot_milestone_network) {
      # plot milestone network
      plot <- plot +
        ggraph::geom_edge_link(aes(x=comp_1_from, y=comp_2_from, xend=comp_1_to, yend=comp_2_to), data=milestone_network) +
        ggraph::geom_edge_link(aes(x=comp_1_from, y=comp_2_from, xend=comp_1_mid, yend=comp_2_mid), data=milestone_network, arrow=arrow(type="closed", length = unit(0.4, "cm")))

      # plot milestone labels
      label_milestones <- get_milestone_labelling(traj, label_milestones)
      if(length(label_milestones)) {
        milestone_positions <- milestone_positions %>% mutate(label = label_milestones[milestone_id])
        if(color_cells == "milestone") {
          plot <- plot + geom_label(aes(label=label, fill=color), data=milestone_positions %>% filter(!is.na(label)))
        } else {
          plot <- plot + geom_label(aes(label=label), data=milestone_positions %>% filter(!is.na(label)))
        }
      } else {
        if(color_cells == "milestone") {
          plot <- plot +
            geom_point(color="black", data=milestone_positions, size=6) +
            geom_point(aes(color=color), data=milestone_positions, size=4)
        } else {
          plot <- plot +
            geom_point(color="#333333", data=milestone_positions, size=2, alpha=1)
        }
      }
    }

    plot
  }
)
