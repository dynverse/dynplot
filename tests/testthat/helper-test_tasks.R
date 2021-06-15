load_test_datasets <- function(name) {
  readRDS(paste0(rprojroot::find_testthat_root_file(), "/", name, ".rds"))
}

test_datasets <- function(datasets, func) {
  for (dataseti in seq_len(nrow(datasets))) {
    dataset <- dynutils::extract_row_to_list(datasets, dataseti)

    func(dataset)
  }
}

expect_ggplot <- function(g) {
  expect_is(g, "ggplot")

  pdf("/dev/null")
  print(g)
  dev.off()
}
