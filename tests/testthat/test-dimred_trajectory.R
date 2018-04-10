context("Testing dimred_trajectory")

data("toy_tasks", package="dyntoy")

toy_tasks <- toy_tasks %>% sample_n(10)

for (taski in seq_len(nrow(toy_tasks))) {
  task <- extract_row_to_list(toy_tasks, taski)

  test_that(paste0("Perform dimred on trajectory with task ", task$id), {
    dimred <- dimred_trajectory(task)

    # check dimred$space_milestones
    milestone_ids <- task$milestone_ids
    space_milestones <- dimred$space_milestones
    expect_equal( nrow(space_milestones), length(milestone_ids) )
    expect_equal( space_milestones$milestone_id, milestone_ids )
    expect_true( all(c("milestone_id", "Comp1", "Comp2", "colour") %in% colnames(space_milestones)) )
    expect_true( all(is.finite(space_milestones$Comp1)) )
    expect_true( all(is.finite(space_milestones$Comp2)) )

    # check dimred$milestone_network
    milestone_network <- task$milestone_network
    space_lines <- dimred$space_lines
    expect_equal( nrow(space_lines), nrow(milestone_network) )
    expect_true( all(is.finite(space_lines$from.Comp1)) )
    expect_true( all(is.finite(space_lines$from.Comp2)) )
    expect_true( all(is.finite(space_lines$to.Comp1)) )
    expect_true( all(is.finite(space_lines$to.Comp2)) )
    expect_equal( sort(unique(c(space_lines$from, space_lines$to))), sort(milestone_ids) )

    cell_ids <- task$cell_ids
    space_samples <- dimred$space_samples
    expect_equal( nrow(space_samples), length(cell_ids) )
    expect_equal( space_samples$cell_id, cell_ids )

    # try on an undirected network
    task$milestone_network$directed <- FALSE
    dimred2 <- dimred_trajectory(task)

    # try with custom colourss
    dimred3 <- dimred_trajectory(task, colour_cells = colnames(task$expression)[[1]])

    dimred4 <- dimred_trajectory(task, colour_cells = task$expression[,1])

    n_cells <- length(cell_ids)
    col_cells <- set_names(ifelse(runif(n_cells) < .5, "#aaaaee", "blue"), cell_ids)
    dimred5 <- dimred_trajectory(task, colour_cells = col_cells)

    n_milestones <- length(milestone_ids)
    col_mils <- set_names(ifelse(runif(n_milestones) < .5, "#aaaaee", "blue"), milestone_ids)
    dimred6 <- dimred_trajectory(task, colour_cells = col_cells, colour_milestones = col_mils)

    dimred7 <- dimred_trajectory(task, colour_milestones = col_mils)
  })
}
