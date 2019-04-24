layout_dendro <- function(trajectory, diag_offset = 0.05) {
  # root if necessary
  if ("root_milestone_id" %in% names(trajectory)) {
    root <- trajectory$root_milestone_id
  } else {
    trajectory <- dynwrap::add_root(trajectory)
    root <- trajectory$root_milestone_id
  }

  # make sure every cell is on only one edge
  trajectory$progressions <- progressions_one_edge(trajectory$progressions)

  # convert to graph
  milestone_network <- trajectory$milestone_network %>% mutate(edge_id = seq_len(n()))
  milestone_graph <- milestone_network %>% tidygraph::as_tbl_graph()

  # determine leaves & position the leaves evenly
  leaves <- setdiff(milestone_network$to, milestone_network$from)
  node_order <- milestone_graph %>% igraph::dfs(root) %>% .$order %>% names # use dfs to find order of final nodes
  leaves <- leaves[order(match(leaves, node_order))]
  leaves_comp_2 <- set_names(seq_along(leaves), leaves)

  # get leaves under each node (to get y positions later)
  descendants <- map(trajectory$milestone_ids, function(milestone_id) {intersect(leaves, names(igraph::dfs(milestone_graph, milestone_id, neimode = "out", unreachable = F)$order))}) %>% set_names(trajectory$milestone_ids)

  # calculate diag offset based on largest distances between root and leaf
  max_comp_1 <- igraph::distances(milestone_graph, root, leaves, weights = igraph::E(milestone_graph)$length) %>% max
  diag_offset <- max_comp_1 * diag_offset

  # now recursively go from root to leaves
  # each time adding the comp_1 and the comp_2
  search <- function(from, milestone_positions = tibble(node_id = from, comp_1 = 0, comp_2 = mean(leaves_comp_2[descendants[[from]]]))) {
    milestone_network_to <- milestone_network %>% filter(from %in% !!from, !to %in% milestone_positions$node_id)
    milestone_positions <- bind_rows(
      milestone_positions,
      tibble(
        node_id = milestone_network_to$to,
        comp_1 = milestone_positions$comp_1[match(milestone_network_to$from, milestone_positions$node_id)] + milestone_network_to$length + diag_offset,
        comp_2 = map_dbl(milestone_network_to$to, ~mean(leaves_comp_2[descendants[[.]]])),
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
      comp_1 = milestone_positions_to$comp_1[match(parent_node_id, milestone_positions_to$node_id)] + diag_offset,
      node_id = paste0(parent_node_id, "-", node_id)
    )

  # combine positions
  milestone_positions <- bind_rows(
    milestone_positions_to %>% mutate(node_type = "milestone"),
    milestone_positions_from %>% mutate(node_type = "fake_milestone")
  )

  # now generate network between milestones
  tree_branches <- tibble(
    node_id_from = milestone_positions_from$node_id,
    node_id_to = milestone_positions_from$child_node_id,
    edge_id = milestone_positions_from$edge_id,
    from = milestone_positions_from$parent_node_id,
    to = node_id_to
  )

  connection_positions <- tibble(
    from = milestone_positions_from$parent_node_id,
    to = milestone_positions_from$node_id
  ) %>%
    left_join(
      milestone_positions %>% select(node_id, comp_1, comp_2) %>% rename_all(~paste0(., "_from")),
      c("from" = "node_id_from")
    ) %>%
    left_join(
      milestone_positions %>% select(node_id, comp_1, comp_2) %>% rename_all(~paste0(., "_to")),
      c("to" = "node_id_to")
    ) %>%
    mutate(connection_ix = 1) %>%
    select(milestone_id = from, comp_1_from, comp_2_from, comp_1_to, comp_2_to, -to)

  edge_positions <- bind_rows(
    tree_branches
  ) %>%
    left_join(
      milestone_positions %>% select(node_id, comp_1, comp_2) %>% rename_all(~paste0(., "_from")),
      "node_id_from"
    ) %>%
    left_join(
      milestone_positions %>% select(node_id, comp_1, comp_2) %>% rename_all(~paste0(., "_to")),
      "node_id_to"
    )

  # put cells on tree
  progressions <- trajectory$progressions %>%
    group_by(cell_id) %>%
    arrange(percentage) %>%
    filter(dplyr::row_number() == 1) %>%
    ungroup()

  cell_positions <- progressions %>%
    left_join(milestone_network %>% select(from, to, edge_id), c("from", "to")) %>% # get edge_ids
    left_join(edge_positions, "edge_id") %>% # add comp_1 and comp_2 positions
    mutate(
      comp_1 = comp_1_from + (comp_1_to - comp_1_from) * percentage,
      comp_2 = comp_2_from
    )

  # clean up milestone positions & edges
  milestone_positions <- milestone_positions %>%
    filter(node_type == "milestone") %>%
    select(milestone_id = node_id, comp_1, comp_2)

  edge_positions <- edge_positions %>%
    select(from, to, comp_1_from, comp_2_from, comp_1_to, comp_2_to)

  # get segments
  segments <- calculate_segments_from_edges(edge_positions)

  segment_progressions <- segments$segment_progressions
  segment_positions <- segments$segment_positions

  lst(
    milestone_positions = milestone_positions %>% rename_dimred_xy(),
    edge_positions = edge_positions %>% rename_dimred_xy(),
    cell_positions = cell_positions %>% rename_dimred_xy(),
    segment_progressions = segment_progressions %>% rename_dimred_xy(),
    segment_positions = segment_positions %>% rename_dimred_xy(),
    connection_positions = connection_positions %>% rename_dimred_xy()
  )

}
