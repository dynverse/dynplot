context("Test plot_onedim")

test_tasks(load_test_tasks("toy_tasks_connected"), function(task) {
  test_that(paste0("plot_onedim on ", task$id), {
    g <- plot_onedim(task)
    expect_ggplot(g)
  })

  test_that(paste0("plot_onedim on ", task$id, " with pseudotime"), {
    g <- plot_onedim(task, color_cells = "pseudotime")
    expect_ggplot(g)
  })

  test_that(paste0("plot_onedim on ", task$id, " with grouping"), {
    g <- plot_onedim(task, grouping_assignment = task$grouping_assignment)
    expect_ggplot(g)
  })

  test_that(paste0("plot_onedim on ", task$id, " with milestone"), {
    g <- plot_onedim(task, "milestone")
    expect_ggplot(g)
  })
})
