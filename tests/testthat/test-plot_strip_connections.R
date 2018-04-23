context("Test plot_onedim")

toy_tasks <- dyntoy::toy_tasks %>% group_by(trajectory_type) %>% filter(row_number() == 1) %>% ungroup()

for (taski in seq_len(nrow(toy_tasks))) {
  task <- extract_row_to_list(toy_tasks, taski)

  test_that(paste0("Plot strip onedim of ", task$id), {
    pseudotime <- task$counts %>% prcomp() %>% {.$x[, 1]}
    prediction <- dynwrap::wrap_data("dummy_prediction", task$cell_ids) %>%
      dynwrap::add_linear_trajectory_to_wrapper(pseudotime)

    g <- plot_strip_onedim(task, prediction)
    expect_is(g, "ggplot")

    pdf("/dev/null")
    print(g)
    dev.off()
  })
}
