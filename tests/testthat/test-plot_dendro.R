context("Test plot_combined")

toy_tasks <- dyntoy::toy_tasks %>% group_by(trajectory_type) %>% filter(row_number() == 1) %>% filter(trajectory_type %in% c("directed_linear", "bifurcation", "multifurcation", "rooted_tree", "rooted_binary_tree")) %>% ungroup()

for (taski in seq_len(nrow(toy_tasks))) {
  task <- extract_row_to_list(toy_tasks, taski)

  test_that(paste0("Perform dimred on trajectory with task ", task$id), {
    g <- plot_dendro(task)
    expect_is(g, "ggplot")

    pdf("/dev/null")
    print(g)
    dev.off()
  })
}
