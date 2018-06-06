context("Test plot_dimred")

test_tasks(load_test_tasks("toy_tasks_connected"), function(task) {
  test_that(paste0("plot_dimred on ", task$id), {
    g <- plot_dimred(task)
    expect_ggplot(g)
  })

  space <- dimred_pca(task$expression)
  feature_oi <- first(colnames(task$expression))
  grouping <- task$prior_information$grouping_assignment

  test_that(paste0("plot_dimred on ", task$id, "with giving space"), {
    g <- plot_dimred(task, dimred = space)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with pseudotime"), {
    g <- plot_dimred(task, color_cells = "pseudotime", dimred = space)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with grouping"), {
    g <- plot_dimred(task, grouping = grouping, dimred = space)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with milestone"), {
    g <- plot_dimred(task, "milestone", dimred = space)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with feature"), {
    g <- plot_dimred(task, "milestone", dimred = space, feature_oi = feature_oi)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with grouping"), {
    g <- plot_dimred(task, "milestone", dimred = space, color_density = "grouping", grouping = grouping)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with feature density"), {
    g <- plot_dimred(task, "milestone", dimred = space, color_density = "feature", feature_oi = feature_oi)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with milestone network"), {
    g <- plot_dimred(task, "milestone", dimred = space, plot_milestone_network = TRUE)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with trajectory projection"), {
    g <- plot_dimred(task, "milestone", dimred = space, plot_trajectory = TRUE)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with milestones from different trajectory"), {
    pred <- dynwrap::infer_trajectory(task, method = "comp1")

    g <- plot_dimred(
      pred,
      color_cells = "milestone",
      milestones = task$milestone_ids,
      milestone_percentages = task$milestone_percentages
    )
    expect_ggplot(g)
  })
})
