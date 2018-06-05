context("Test plot_dendro")

test_tasks(load_test_tasks("toy_tasks_tree"), function(task) {
  test_that(paste0("plot_dendro on ", task$id), {
    g <- plot_dendro(task)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dendro on ", task$id, " with pseudotime"), {
    g <- plot_dendro(task, color_cells = "pseudotime")
    expect_ggplot(g)
  })

  test_that(paste0("plot_dendro on ", task$id, " with grouping"), {
    g <- plot_dendro(task, grouping = task$grouping)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dendro on ", task$id, " with milestone"), {
    g <- plot_dendro(task, "milestone")
    expect_ggplot(g)
  })

  test_that(paste0("plot_dendro on ", task$id, " with milestones from different trajectory"), {
    pred <- dynwrap::infer_trajectory(task, method = "comp1")

    g <- plot_dendro(
      pred,
      color_cells = "milestone",
      milestones = task$milestone_ids,
      milestone_percentages = task$milestone_percentages
    )
    expect_ggplot(g)
  })
})
