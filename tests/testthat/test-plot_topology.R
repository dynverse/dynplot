context("Test plot_topology")

test_datasets(load_test_datasets("toy_datasets"), function(dataset) {
  test_that(paste0("plot_topology on ", dataset$id), {
    g <- plot_topology(dataset)
    expect_ggplot(g)

    g <- plot_topology(dataset, layout = "kk")
    expect_ggplot(g)
  })
})
