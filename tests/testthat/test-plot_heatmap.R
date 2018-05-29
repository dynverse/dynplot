context("Test plot_heatmap")

test_tasks(load_test_tasks("toy_tasks_connected"), function(task) {
  test_that(paste0("Plotting heatmap of ", task$id), {
    g <- plot_heatmap(task)
    expect_ggplot(g)
  })

  test_that(paste0("Plotting heatmap of ", task$id), {
    g <- plot_heatmap(task, heatmap_type = "dotted")
    expect_ggplot(g)
  })

  test_that(paste0("Plotting heatmap of ", task$id), {
    g <- plot_heatmap(task, heatmap_type = "dotted")
    expect_ggplot(g)
  })

  test_that(paste0("Plotting heatmap of ", task$id, " with grouping"), {
    g <- plot_heatmap(task, grouping = task$prior_information$grouping_assignment)
    expect_ggplot(g)
  })
})
