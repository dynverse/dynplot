context("Test plot_heatmap")

toy_tasks <- dyntoy::toy_tasks %>% group_by(trajectory_type) %>% filter(row_number() == 1) %>% ungroup()

for (taski in seq_len(nrow(toy_tasks))) {
  task <- extract_row_to_list(toy_tasks, taski)

  test_that(paste0("Plotting heatmap of ", task$id), {
    g <- plot_heatmap(task)
    expect_is(g, "ggplot")

    pdf("/dev/null")
    print(g)
    dev.off()
  })
}