# context("Test plot_linearised_comparison")
#
# test_datasets(load_test_datasets("toy_datasets_connected"), function(dataset) {
#   test_that(paste0("Plot linearised_comparison of ", dataset$id), {
#     pseudotime <- dataset$counts %>% prcomp() %>% {.$x[, 1]}
#     prediction <- dynwrap::wrap_data("dummy_prediction", dataset$cell_ids) %>%
#       dynwrap::add_linear_trajectory(pseudotime)
#
#     g <- plot_linearised_comparison(dataset, prediction)
#     expect_ggplot(g)
#   })
# })
