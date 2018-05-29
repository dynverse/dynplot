context("Test plot_graph")

test_tasks(load_test_tasks("toy_tasks_connected"), function(task) {
  test_that(paste0("plot_graph on ", task$id), {
    g <- plot_graph(task)
    expect_ggplot(g)
  })

  test_that(paste0("plot_graph on ", task$id, " with pseudotime"), {
    g <- plot_graph(task, color_cells = "pseudotime")
    expect_ggplot(g)
  })

  test_that(paste0("plot_graph on ", task$id, " with grouping"), {
    g <- plot_graph(task, grouping = task$grouping)
    expect_ggplot(g)
  })

  test_that(paste0("plot_graph on ", task$id, " with milestone"), {
    g <- plot_graph(task, "milestone")
    expect_ggplot(g)
  })

  test_that(paste0("plot_graph on ", task$id, " with plotting of milestones and plotting of labels"), {
    g <- plot_graph(task, plot_milestones = TRUE)
    expect_ggplot(g)
    g <- plot_graph(task, plot_label = "none")
    expect_ggplot(g)
    g <- plot_graph(task, plot_label = "all")
    expect_ggplot(g)
  })
})
