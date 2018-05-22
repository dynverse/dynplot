context("Test plot_linearised_comparison")

test_tasks(load_test_tasks("toy_tasks_connected"), function(task) {
  test_that(paste0("Plot linearised_comparison of ", task$id), {
    pseudotime <- task$counts %>% prcomp() %>% {.$x[, 1]}
    prediction <- dynwrap::wrap_data("dummy_prediction", task$cell_ids) %>%
      dynwrap::add_linear_trajectory(pseudotime)

    g <- plot_linearised_comparison(task, prediction)
    expect_ggplot(g)
  })
})
