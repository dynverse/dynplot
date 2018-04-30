context("Test plot_features")

test_tasks(load_test_tasks("toy_tasks_connected"), function(task) {
  test_that(paste0("Plotting features of ", task$id), {
    g <- plot_features(task)
    expect_ggplot(g)
  })
})
