context("Test plot_features")

test_datasets(load_test_datasets("toy_datasets_connected"), function(dataset) {
  test_that(paste0("Plotting features of ", dataset$id), {
    g <- plot_features(dataset)
    expect_ggplot(g)
  })
})
