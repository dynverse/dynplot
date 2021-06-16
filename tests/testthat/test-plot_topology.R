context("Test plot_topology")

dataset <- dynplot::example_disconnected

test_that(paste0("plot_topology on ", dataset$id), {
  g <- plot_topology(dataset)
  expect_ggplot(g)

  g <- plot_topology(dataset, layout = "kk")
  expect_ggplot(g)
})
