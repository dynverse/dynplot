context("Testing dimred_trajectory")

toy_tasks <- dyntoy::toy_tasks %>% group_by(trajectory_type) %>% filter(dplyr::row_number() == 1) %>% ungroup() %>% filter(trajectory_type != "disconnected_directed_graph")

for (taski in seq_len(nrow(toy_tasks))) {
  task <- extract_row_to_list(toy_tasks, taski)

  test_that(paste0("Perform dimred on trajectory with task ", task$id), {
    dimred <- dimred_trajectory(task)

    # check dimred$space_milestones
    milestone_ids <- task$milestone_ids
    space_milestones <- dimred$space_milestones
    expect_equal( nrow(space_milestones), length(milestone_ids) )
    expect_equal( space_milestones$milestone_id, milestone_ids )
    expect_true( all(is.finite(space_milestones$comp_1)) )
    expect_true( all(is.finite(space_milestones$comp_2)) )

    # check dimred$milestone_network
    milestone_network <- task$milestone_network
    space_lines <- dimred$space_lines
    expect_equal( nrow(space_lines), nrow(milestone_network) )
    expect_true( all(is.finite(space_lines$from.comp_1)) )
    expect_true( all(is.finite(space_lines$from.comp_2)) )
    expect_true( all(is.finite(space_lines$to.comp_1)) )
    expect_true( all(is.finite(space_lines$to.comp_2)) )
    expect_equal( sort(unique(c(space_lines$from, space_lines$to))), sort(milestone_ids) )

    cell_ids <- task$cell_ids
    space_samples <- dimred$space_samples
    expect_equal( nrow(space_samples), length(cell_ids) )
    expect_equal( space_samples$cell_id, cell_ids )

    # try on an undirected network
    task$milestone_network$directed <- FALSE
    dimred2 <- dimred_trajectory(task)
  })
}
