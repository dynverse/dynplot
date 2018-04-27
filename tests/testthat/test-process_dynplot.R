context("Test process_dynplot")

g1 <- ggplot(data.frame(x=1:4, y=2:5)) + geom_point(aes(x, y))
test_that(paste0("process_dynplot"), {
  g <- process_dynplot(g1)
  expect_ggplot(g)

  g <- process_dynplot(g1, "yabba dabba doo")
  expect_ggplot(g)
})
