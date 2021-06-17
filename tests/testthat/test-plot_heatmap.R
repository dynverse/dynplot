context("Test plot_heatmap")

dataset <- dynplot::example_disconnected

test_that(paste0("Plotting heatmap of ", dataset$id), {
  g <- plot_heatmap(dataset)
  expect_ggplot(g)
})

test_that(paste0("Plotting heatmap of ", dataset$id), {
  g <- plot_heatmap(dataset, heatmap_type = "dotted")
  expect_ggplot(g)
})

test_that(paste0("Plotting heatmap of ", dataset$id), {
  g <- plot_heatmap(dataset, heatmap_type = "dotted")
  expect_ggplot(g)
})

test_that(paste0("Plotting heatmap of ", dataset$id, " with grouping"), {
  g <- plot_heatmap(dataset, grouping = dataset$prior_information$grouping_assignment)
  expect_ggplot(g)
})
