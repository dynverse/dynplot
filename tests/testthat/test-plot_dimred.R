context("Test plot_dimred")

test_tasks(load_test_tasks("toy_tasks_connected"), function(task) {
  test_that(paste0("plot_dimred on ", task$id), {
    g <- plot_dimred(task)
    expect_ggplot(g)
  })

  space <- dimred_pca(task$expression)

  test_that(paste0("plot_dimred on ", task$id, "with giving space"), {
    g <- plot_dimred(task, dimred_method=space)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with pseudotime"), {
    g <- plot_dimred(task, color_cells = "pseudotime", dimred_method = space)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with grouping"), {
    g <- plot_dimred(task, grouping_assignment = task$grouping_assignment, dimred_method = space)
    expect_ggplot(g)
  })

  test_that(paste0("plot_dimred on ", task$id, " with milestone"), {
    g <- plot_dimred(task, "milestone", dimred_method = space)
    expect_ggplot(g)
  })
})

context("Test plot_dimred and dimensionality_reduction")

task <- load_test_tasks("toy_tasks_connected") %>% filter(trajectory_type == "directed_linear") %>% extract_row_to_list(1)
map(get_dimreds(), function(dimred_name) {
  space <- get(dimred_name)(task$expression)

  test_that(paste0("plot_dimred on ", task$id, " with ", dimred_name), {
    g <- plot_dimred(task, dimred_method = space)
    expect_ggplot(g)
  })
})
