context("Test plot_default")

data("toy_tasks", package="dyntoy")

toy_tasks <- toy_tasks %>% sample_n(10)

test_ggplot <- function(g) {
  expect_is(g, "ggplot")
  pdf("/dev/null")
  print(g)
  dev.off()
}

for (taski in seq_len(nrow(toy_tasks))) {
  task <- extract_row_to_list(toy_tasks, taski)

  test_that(paste0("Perform dimred on trajectory with task ", task$id), {
    plot1 <- plot_default(task)
    test_ggplot(plot1)

    # try on an undirected network
    task$milestone_network$directed <- FALSE
    plot2 <- plot_default(task)
    test_ggplot(plot2)

    # try with custom colourss
    plot3 <- plot_default(task, colour_cells = colnames(task$expression)[[1]])
    test_ggplot(plot3)

    plot4 <- plot_default(task, colour_cells = task$expression[,1])
    test_ggplot(plot4)

    cell_ids <- task$cell_ids
    n_cells <- length(cell_ids)
    col_cells <- set_names(ifelse(runif(n_cells) < .5, "#aaaaee", "blue"), cell_ids)
    plot5 <- plot_default(task, colour_cells = col_cells)
    test_ggplot(plot5)

    milestone_ids <- task$milestone_ids
    n_milestones <- length(milestone_ids)
    col_mils <- set_names(ifelse(runif(n_milestones) < .5, "#aaaaee", "blue"), milestone_ids)
    plot6 <- plot_default(task, colour_cells = col_cells, colour_milestones = col_mils)
    test_ggplot(plot6)

    plot7 <- plot_default(task, colour_milestones = col_mils)
    test_ggplot(plot7)
  })
}
