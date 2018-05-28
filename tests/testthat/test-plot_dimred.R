context("Test plot_dimred")

test_tasks(load_test_tasks("toy_tasks_connected"), function(task) {
  test_that(paste0("plot_dimred on ", task$id), {
    g <- plot_dimred(task)
    expect_ggplot(g)
  })

  space <- dimred_pca(task$expression)
  feature_oi <- first(colnames(task$expression))
  grouping_assignment <- task$prior_information$grouping_assignment

  test_that(paste0("plot_dimred on ", task$id, "with giving space"), {
    g <- plot_dimred(task, dimred=space)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with pseudotime"), {
    g <- plot_dimred(task, color_cells = "pseudotime", dimred = space)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with grouping"), {
    g <- plot_dimred(task, grouping_assignment = grouping_assignment, dimred = space)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with milestone"), {
    g <- plot_dimred(task, "milestone", dimred = space)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with feature"), {
    g <- plot_dimred(task, "milestone", dimred = space, feature_oi = feature_oi)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with grouping"), {
    g <- plot_dimred(task, "milestone", dimred = space, color_density = "grouping", grouping_assignment = grouping_assignment)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with feature density"), {
    g <- plot_dimred(task, "milestone", dimred = space, color_density = "feature", feature_oi = feature_oi)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with milestone network"), {
    g <- plot_dimred(task, "milestone", dimred = space, plot_milestone_network = TRUE)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with trajectory projection"), {
    g <- plot_dimred(task, "milestone", dimred = space, plot_trajectory = TRUE)
    expect_ggplot(g)
  })
})


context("Test plot_dimred and dimensionality_reduction")

task <- load_test_tasks("toy_tasks_connected") %>% filter(trajectory_type == "directed_linear") %>% extract_row_to_list(1)
map(get_dimreds(), function(dimred_name) {
  space <- get(dimred_name)(task$expression)

  test_that(paste0("plot_dimred on ", task$id, " with ", dimred_name), {
    g <- plot_dimred(task, dimred = space)
    expect_ggplot(g)
  })
})
