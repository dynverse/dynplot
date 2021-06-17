context("Test plot_dimred")

dataset <- dynplot::example_disconnected

test_that(paste0("plot_dimred on ", dataset$id), {
  g <- plot_dimred(dataset)
  expect_ggplot(g)
})

space <- dyndimred::dimred_pca(dataset$expression)
feature_oi <- first(colnames(dataset$expression))
grouping <- dataset$prior_information$grouping_assignment

test_that(paste0("plot_dimred on ", dataset$id, "with giving space"), {
  g <- plot_dimred(dataset, dimred = space)
  expect_ggplot(g)
})

test_that(paste0("plot_dimred on ", dataset$id, " with pseudotime"), {
  g <- plot_dimred(dataset, color_cells = "pseudotime", dimred = space)
  expect_ggplot(g)
})

test_that(paste0("plot_dimred on ", dataset$id, " with grouping"), {
  g <- plot_dimred(dataset, grouping = grouping, dimred = space)
  expect_ggplot(g)
})

test_that(paste0("plot_dimred on ", dataset$id, " with milestone"), {
  g <- plot_dimred(dataset, "milestone", dimred = space)
  expect_ggplot(g)
})

test_that(paste0("plot_dimred on ", dataset$id, " with feature"), {
  g <- plot_dimred(dataset, "milestone", dimred = space, feature_oi = feature_oi)
  expect_ggplot(g)
})

test_that(paste0("plot_dimred on ", dataset$id, " with grouping"), {
  g <- plot_dimred(dataset, "milestone", dimred = space, color_density = "grouping", grouping = grouping)
  expect_ggplot(g)
})

test_that(paste0("plot_dimred on ", dataset$id, " with feature density"), {
  g <- plot_dimred(dataset, "milestone", dimred = space, color_density = "feature", feature_oi = feature_oi)
  expect_ggplot(g)
})

test_that(paste0("plot_dimred on ", dataset$id, " with milestone network"), {
  g <- plot_dimred(dataset, "milestone", dimred = space, plot_milestone_network = TRUE)
  expect_ggplot(g)
})

test_that(paste0("plot_dimred on ", dataset$id, " with trajectory projection"), {
  g <- plot_dimred(dataset, "milestone", dimred = space, plot_trajectory = TRUE)
  expect_ggplot(g)
})

test_that(paste0("plot_dimred on ", dataset$id, " with milestones from different trajectory"), {
  pseudotime <- dataset$counts %>% stats::prcomp() %>% {.$x[, 1]}
  pred <-
    dynwrap::wrap_data("dummy_prediction", dataset$cell_ids) %>%
    dynwrap::add_linear_trajectory(pseudotime) %>%
    dynwrap::add_root()

  g <- plot_dimred(
    pred,
    expression_source = dataset,
    color_cells = "milestone",
    milestones = dataset$milestone_ids,
    milestone_percentages = dataset$milestone_percentages
  )
  expect_ggplot(g)
})
