context("Test plot_onedim")

test_datasets(load_test_datasets("toy_datasets_connected"), function(dataset) {
  test_that(paste0("plot_onedim on ", dataset$id), {
    g <- plot_onedim(dataset)
    expect_ggplot(g)
  })

  test_that(paste0("plot_onedim on ", dataset$id, " with pseudotime"), {
    g <- plot_onedim(dataset, color_cells = "pseudotime")
    expect_ggplot(g)
  })

  test_that(paste0("plot_onedim on ", dataset$id, " with grouping"), {
    g <- plot_onedim(dataset, grouping = dataset$grouping)
    expect_ggplot(g)
  })

  test_that(paste0("plot_onedim on ", dataset$id, " with milestone"), {
    g <- plot_onedim(dataset, "milestone")
    expect_ggplot(g)
  })

  test_that(paste0("plot_onedim on ", dataset$id, " with milestones from different trajectory"), {
    pred <- dynwrap::infer_trajectory(dataset, method = "comp1")

    g <- plot_onedim(
      pred,
      color_cells = "milestone",
      milestones = dataset$milestone_ids,
      milestone_percentages = dataset$milestone_percentages
    )
    expect_ggplot(g)
  })
})
