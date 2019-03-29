context("Test linearisation")

test_datasets(load_test_datasets("toy_datasets_connected"), function(dataset) {

  test_that(paste0("Linearise cells of ", dataset$id, " works."), {
    linearisation <- linearise_cells(dataset)

    expect_true(all(dataset$cell_ids %in% linearisation$progressions$cell_id))
  })

  test_that(paste0("Linearise cells of ", dataset$id, " on one edge works."), {
    linearisation <- linearise_cells(dataset, one_edge = TRUE)
    expect_true(all(dataset$cell_ids %in% linearisation$progressions$cell_id))
    expect_true(all(table(linearisation$progressions$cell_id) == 1))
  })
})
