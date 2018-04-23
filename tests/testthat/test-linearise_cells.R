context("Test linearisation")

toy_tasks <- dyntoy::toy_tasks %>% group_by(trajectory_type) %>% filter(row_number() == 1) %>% ungroup()

for (taski in seq_len(nrow(toy_tasks))) {
  task <- extract_row_to_list(toy_tasks, taski)

  test_that(paste0("Linearise cells of ", task$id, " works."), {
    linearisation <- linearise_cells(task$milestone_network, task$progressions)

    expect_true(all(task$cell_ids %in% linearisation$progressions$cell_id))
  })

  test_that(paste0("Linearise cells of ", task$id, " on one edge works."), {
    linearisation <- linearise_cells(task$milestone_network, task$progressions, one_edge=TRUE)
    expect_true(all(task$cell_ids %in% linearisation$progressions$cell_id))
    expect_true(all(table(linearisation$progressions$cell_id) == 1))
  })
}
