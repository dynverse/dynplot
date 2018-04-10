context("Test is_colour_vector")

test_that("is_colour_vector correctly returns whether a vector only contains colour information or not", {

  expect_true(is_colour_vector("blue"))
  expect_true(is_colour_vector("#aaaaaa"))
  expect_true(is_colour_vector("#AbCdEf"))
  expect_true(is_colour_vector("#01234567"))
  expect_true(is_colour_vector(c("blue", "#aaaaaa", "#AbCdEf", "#01234567")))

  expect_false(is_colour_vector("#012345PP"))
  expect_false(is_colour_vector("#012345FFFF"))
  expect_false(is_colour_vector("#012345F"))
  expect_false(is_colour_vector("#0123"))
})
