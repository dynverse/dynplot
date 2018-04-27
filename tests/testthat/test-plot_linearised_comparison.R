context("Test plot_linearised_comparison")

test_tasks(load_test_tasks("toy_tasks_connected"), function(task) {
  test_that(paste0("Plot linearised_comparison of ", task$id), {
    pseudotime <- task$counts %>% prcomp() %>% {.$x[, 1]}
    prediction <- dynwrap::wrap_data("dummy_prediction", task$cell_ids) %>%
      dynwrap::add_linear_trajectory_to_wrapper(pseudotime)

    g <- plot_strip_onedim(task, prediction)
    expect_ggplot(g)
  })
})
