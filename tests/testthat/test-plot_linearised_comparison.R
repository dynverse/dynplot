context("Test plot_linearised_comparison")

dataset <- dynplot::example_disconnected

test_that(paste0("Plot linearised_comparison of ", dataset$id), {
  pseudotime <- dataset$counts %>% stats::prcomp() %>% {.$x[, 1]}
  prediction <-
    dynwrap::wrap_data("dummy_prediction", dataset$cell_ids) %>%
    dynwrap::add_linear_trajectory(pseudotime) %>%
    dynwrap::add_root()

  g <- plot_linearised_comparison(dataset, prediction)
  expect_ggplot(g)
})
