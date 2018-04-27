context("Test plot_genes")

test_tasks(load_test_tasks("toy_tasks_connected"), function(task) {
  test_that(paste0("Plotting genes of ", task$id), {
    g <- plot_genes(task)
    expect_ggplot(g)
  })
})
