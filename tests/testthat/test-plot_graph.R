context("Test plot_graph")

toy_tasks <- dyntoy::toy_tasks %>% group_by(trajectory_type) %>% filter(row_number() == 1) %>% ungroup()

test_ggplot <- function(g) {
  expect_is(g, "ggplot")
  pdf("/dev/null")
  print(g)
  dev.off()
}

for (taski in seq_len(nrow(toy_tasks))) {
  task <- extract_row_to_list(toy_tasks, taski)

  test_that(paste0("Perform dimred on trajectory with task ", task$id), {
    plot1 <- plot_graph(task)
    test_ggplot(plot1)

    # try on an undirected network
    task$milestone_network$directed <- FALSE
    plot2 <- plot_graph(task)
    test_ggplot(plot2)

    # try with custom colourss
    plot3 <- plot_graph(task, gene_oi = colnames(task$counts)[[1]])
    test_ggplot(plot3)

    plot4 <- plot_graph(task, grouping_assignment = task$prior_information$grouping_assignment)
    test_ggplot(plot4)
  })
}
