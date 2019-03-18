#' Plot a trajectory as a dendrogram
#'
#' @param diag_offset The x-offset (percentage of the edge lenghts) between milestones
#' @param y_offset The size of the quasirandom cell spreading in the y-axis
#'
#' @inheritParams add_cell_coloring
#'
#' @keywords plot_trajectory
#'
#' @export
#'
#' @examples
#' dataset <- dyntoy::generate_trajectory(model = "tree")
#' plot_dendro(dataset)
#' plot_dendro(dataset, color_cells = "pseudotime")
#' plot_dendro(
#'   dataset,
#'   color_cells = "grouping",
#'   grouping = dynwrap::group_onto_nearest_milestones(dataset)
#' )
plot_dendro <- dynutils::inherit_default_params(
  add_cell_coloring,
  function(
    traj,
    color_cells,
    grouping,
    groups,
    feature_oi,
    expression_source,
    pseudotime,
    color_milestones,
    milestones,
    milestone_percentages,
    diag_offset = 0.05,
    y_offset = 0.2
  ) {
    # make sure a trajectory was provided
    testthat::expect_true(dynwrap::is_wrapper_with_trajectory(traj))

    # root if necessary
    if ("root_milestone_id" %in% names(traj)) {
      root <- traj$root_milestone_id
    } else {
      traj <- dynwrap::add_root(traj)
      root <- traj$root_milestone_id
    }

    # check milestones, make sure it's a data_frame
    milestones <- check_milestones(traj, milestones)

    # make sure every cell is on only one edge
    traj$progressions <- progressions_one_edge(traj$progressions)

    # convert to graph
    milestone_network <- traj$milestone_network %>% mutate(edge_id = seq_len(n()))
    milestone_graph <- milestone_network %>% tidygraph::as_tbl_graph()

    # determine leaves & position the leaves evenly
    leaves <- setdiff(milestone_network$to, milestone_network$from)
    node_order <- milestone_graph %>% igraph::dfs(root) %>% .$order %>% names # use dfs to find order of final nodes
    leaves <- leaves[order(match(leaves, node_order))]
    leaves_y <- set_names(seq_along(leaves), leaves)

    # get leaves under each node (to get y positions later)
    descendants <- map(traj$milestone_ids, function(milestone_id) {intersect(leaves, names(igraph::dfs(milestone_graph, milestone_id, neimode = "out", unreachable = F)$order))}) %>% set_names(traj$milestone_ids)

    # calculate diag offset based on largest distances between root and leaf
    max_x <- igraph::distances(milestone_graph, root, leaves, weights = igraph::E(milestone_graph)$length) %>% max
    diag_offset <- max_x * diag_offset

    # now recursively go from root to leaves
    # each time adding the x and the y
    search <- function(from, milestone_positions = tibble(node_id = from, x = 0, y = mean(leaves_y[descendants[[from]]]))) {
      milestone_network_to <- milestone_network %>% filter(from %in% !!from, !to %in% milestone_positions$node_id)
      milestone_positions <- bind_rows(
        milestone_positions,
        tibble(
          node_id = milestone_network_to$to,
          x = milestone_positions$x[match(milestone_network_to$from, milestone_positions$node_id)] + milestone_network_to$length + diag_offset,
          y = map_dbl(milestone_network_to$to, ~mean(leaves_y[descendants[[.]]])),
          parent_node_id = milestone_network_to$from,
          edge_id = milestone_network_to$edge_id
        )
      )

      if (nrow(milestone_network_to) > 0) {
        milestone_positions <- search(milestone_network_to$to, milestone_positions)
      }

      milestone_positions
    }

    milestone_positions_to <- search(root)

    # extract positions of fake milestones
    milestone_positions_from <- milestone_positions_to %>%
      filter(!is.na(parent_node_id)) %>%
      mutate(
        child_node_id = node_id,
        x = milestone_positions_to$x[match(parent_node_id, milestone_positions_to$node_id)] + diag_offset,
        node_id = paste0(parent_node_id, "-", node_id)
      )

    # combine positions
    milestone_positions <- bind_rows(
      milestone_positions_to %>% mutate(node_type = "milestone"),
      milestone_positions_from %>% mutate(node_type = "fake_milestone")
    )

    # now generate network between milestones
    milestone_tree_branches <- tibble(
      node_id_from = milestone_positions_from$node_id,
      node_id_to = milestone_positions_from$child_node_id,
      edge_id = milestone_positions_from$edge_id
    )

    milestone_tree_connections <- tibble(
      node_id_from = milestone_positions_from$parent_node_id,
      node_id_to = milestone_positions_from$node_id
    )

    milestone_tree_edges <- bind_rows(
      milestone_tree_branches,
      milestone_tree_connections
    ) %>%
      left_join(
        milestone_positions %>% select(node_id, x, y) %>% rename_all(~paste0(., "_from")),
        "node_id_from"
      ) %>%
      left_join(
        milestone_positions %>% select(node_id, x, y) %>% rename_all(~paste0(., "_to")),
        "node_id_to"
      )

    # create milestone tree graph
    milestone_tree <- tidygraph::tbl_graph(
      milestone_positions %>% select(-x, -y),
      milestone_tree_edges %>% mutate(from = match(node_id_from, milestone_positions$node_id), to = match(node_id_to, milestone_positions$node_id))
    )

    # put cells on tree
    progressions <- traj$progressions %>%
      group_by(cell_id) %>%
      arrange(percentage) %>%
      filter(dplyr::row_number() == 1) %>%
      ungroup()

    cell_positions <- progressions %>%
      left_join(milestone_network %>% select(from, to, edge_id), c("from", "to")) %>% # get edge_ids
      left_join(milestone_tree_edges, "edge_id") %>% # add x and y positions
      mutate(
        x = x_from + (x_to - x_from) * percentage,
        y = y_from
      ) %>%
      mutate(y = y + vipor::offsetX(x, edge_id, method = "quasirandom", width = y_offset))

    # add cell coloring
    cell_coloring_output <- add_cell_coloring(
      cell_positions = cell_positions,
      color_cells = color_cells,
      traj = traj,
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

    # determine arrow
    arrow <-
      if (any(traj$milestone_network$directed)) {
        arrow(type = "closed")
      } else {
        NULL
      }

    # generate layout
    layout <- ggraph::create_layout(milestone_tree, "manual", node.position = milestone_positions)

    # start plotting!
    dendro <- ggplot(layout) +
      # the main edges
      ggraph::geom_edge_link(aes(linetype = node2.node_type, edge_width = node2.node_type), colour = "grey") +
      # the arrows
      ggraph::geom_edge_link(aes(xend = x + (xend-x)/2, alpha = node1.node_type), arrow = arrow, colour = "grey", data = get_edges()(layout) %>% filter(node1.node_type != "milestone")) +
      # the node labels
      # ggraph::geom_node_label(aes(label = node_id)) +
      # the cells
      geom_point(aes(x, y), size = 2.5, color = "black", data = cell_positions) +
      geom_point(aes(x, y, color = color), size = 2, data = cell_positions) +
      color_scale +
      # theme graph
      theme_graph() +
      ggraph::scale_edge_linetype_manual(values = c("milestone" = "solid", "fake_milestone" = "dotted"), guide = "none") +
      ggraph::scale_edge_width_manual(values = c("milestone" = 3, "fake_milestone" = 1), guide = "none") +
      ggraph::scale_edge_alpha_discrete(guide = "none") +

      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

    dendro
  }
)
