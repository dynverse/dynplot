context("Test plot_onedim")

dataset <- dynplot::example_disconnected

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
  pseudotime <- dataset$counts %>% stats::prcomp() %>% {.$x[, 1]}
  pred <-
    dynwrap::wrap_data("dummy_prediction", dataset$cell_ids) %>%
    dynwrap::add_linear_trajectory(pseudotime) %>%
    dynwrap::add_root()

  g <- plot_onedim(
    pred,
    color_cells = "milestone",
    milestones = dataset$milestone_ids,
    milestone_percentages = dataset$milestone_percentages
  )
  expect_ggplot(g)
})

