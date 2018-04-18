context("Test plot_combined")

toy_tasks <- dyntoy::toy_tasks %>% group_by(trajectory_type) %>% filter(row_number() == 1) %>% ungroup()

for (taski in seq_len(nrow(toy_tasks))) {
  task <- extract_row_to_list(toy_tasks, taski)

  test_that(paste0("Perform dimred on trajectory with task ", task$id), {
    prediction <- task
    cell_id_map <- setNames(sample(prediction$cell_ids), prediction$cell_ids)
    prediction$milestone_percentages$cell_id <- cell_id_map[prediction$milestone_percentages$cell_id]
    prediction$progressions$cell_id <- cell_id_map[prediction$progressions$cell_id]

    g <- plot_combined(task, prediction)
    expect_is(g, "ggplot")

    pdf("/dev/null")
    print(g)
    dev.off()
  })
}
