# helper function for tests

expect_ggplot <- function(g) {
  requireNamespace("testthat")
  testthat::expect_is(g, "ggplot")

  ggsave(nullfile(), g, device = "pdf")
}
