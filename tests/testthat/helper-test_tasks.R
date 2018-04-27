load_test_tasks <- function(name) {
  readRDS(paste0(name, ".rds"))
}

test_tasks <- function(tasks, func) {
  for (taski in seq_len(nrow(tasks))) {
    task <- dynutils::extract_row_to_list(tasks, taski)

    func(task)
  }
}

expect_ggplot <- function(g) {
  expect_is(g, "ggplot")

  pdf("/dev/null")
  print(g)
  dev.off()
}
