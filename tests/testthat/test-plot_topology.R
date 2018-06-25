context("Test plot_topology")

test_tasks(load_test_tasks("toy_tasks"), function(task) {
  test_that(paste0("plot_topology on ", task$id), {
    g <- plot_topology(task)
    expect_ggplot(g)

    g <- plot_topology(task, layout = "kk")
    expect_ggplot(g)
  })
})
