context("Test plot_heatmap")

test_datasets(load_test_datasets("toy_datasets_connected"), function(dataset) {
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
})
