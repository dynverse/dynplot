expect_ggplot <- function(g) {
  expect_is(g, "ggplot")

  ggsave(nullfile(), g)
}
