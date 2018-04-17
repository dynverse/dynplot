context("Test plot_connections")

data("toy_tasks", package="dyntoy")

toy_tasks <- toy_tasks %>% group_by(toy_tasks$trajectory_type) %>% filter(row_number() == 1) %>% ungroup()

for (taski in seq_len(nrow(toy_tasks))) {
  task <- extract_row_to_list(toy_tasks, taski)

  test_that(paste0("Plot  in ", task$id), {
    pseudotime <- task$counts %>% prcomp() %>% {.$x[, 1]}
    prediction <- dynwrap::wrap_data("dummy_prediction", task$cell_ids) %>%
      dynwrap::add_linear_trajectory_to_wrapper(pseudotime)

    g <- plot_strip_connections(task, prediction)
    expect_is(g, "ggplot")

    pdf("/dev/null")
    print(g)
    dev.off()
  })
}
