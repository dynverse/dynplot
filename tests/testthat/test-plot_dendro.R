context("Test plot_dendro")

test_datasets(load_test_datasets("toy_datasets_tree"), function(dataset) {
  test_that(paste0("plot_dendro on ", dataset$id), {
    g <- plot_dendro(dataset)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dendro on ", dataset$id, " with pseudotime"), {
    g <- plot_dendro(dataset, color_cells = "pseudotime")
    expect_ggplot(g)
  })

  test_that(paste0("plot_dendro on ", dataset$id, " with grouping"), {
    g <- plot_dendro(dataset, grouping = dataset$grouping)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dendro on ", dataset$id, " with milestone"), {
    g <- plot_dendro(dataset, "milestone")
    expect_ggplot(g)
  })

  test_that(paste0("plot_dendro on ", dataset$id, " with milestones from different trajectory"), {
    pred <- dynwrap::infer_trajectory(dataset, method = "comp1")

    g <- plot_dendro(
      pred,
      color_cells = "milestone",
      milestones = dataset$milestone_ids,
      milestone_percentages = dataset$milestone_percentages
    )
    expect_ggplot(g)
  })
})
