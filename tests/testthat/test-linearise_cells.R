context("Test linearisation")

test_tasks(load_test_tasks("toy_tasks_connected"), function(task) {

  test_that(paste0("Linearise cells of ", task$id, " works."), {
    linearisation <- linearise_cells(task$milestone_network, task$progressions)

    expect_true(all(task$cell_ids %in% linearisation$progressions$cell_id))
  })

  test_that(paste0("Linearise cells of ", task$id, " on one edge works."), {
    linearisation <- linearise_cells(task$milestone_network, task$progressions, one_edge=TRUE)
    expect_true(all(task$cell_ids %in% linearisation$progressions$cell_id))
    expect_true(all(table(linearisation$progressions$cell_id) == 1))
  })
})
