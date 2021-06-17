context("Test plot_graph")

dataset <- dynplot::example_disconnected

test_that(paste0("plot_graph on ", dataset$id), {
  g <- plot_graph(dataset)
  expect_ggplot(g)
})

test_that(paste0("plot_graph on ", dataset$id, " with pseudotime"), {
  g <- plot_graph(dataset, color_cells = "pseudotime")
  expect_ggplot(g)
})

test_that(paste0("plot_graph on ", dataset$id, " with grouping"), {
  g <- plot_graph(dataset, grouping = dataset$grouping)
  expect_ggplot(g)
})

test_that(paste0("plot_graph on ", dataset$id, " with milestone"), {
  g <- plot_graph(dataset, "milestone")
  expect_ggplot(g)
})

test_that(paste0("plot_graph on ", dataset$id, " with plotting of milestones and plotting of labels"), {
  g <- plot_graph(dataset, label_milestones = TRUE)
  expect_ggplot(g)

  g <- plot_graph(dataset, label_milestones = FALSE)
  expect_ggplot(g)
})

test_that(paste0("plot_graph on ", dataset$id, " with milestones from different trajectory"), {
  pseudotime <- dataset$counts %>% stats::prcomp() %>% {.$x[, 1]}
  pred <-
    dynwrap::wrap_data("dummy_prediction", dataset$cell_ids) %>%
    dynwrap::add_linear_trajectory(pseudotime) %>%
    dynwrap::add_root()

  g <- plot_graph(
    pred,
    color_cells = "milestone",
    milestones = dataset$milestone_ids,
    milestone_percentages = dataset$milestone_percentages
  )
  expect_ggplot(g)
})
