context("Test palette_wrapper")

test_that("palette_wrapper", {
  col <- milestone_palette("cubeHelix", 5)
  expect_length(col, 5)
  expect_true(is_colour_vector(col))

  col <- milestone_palette("Set3", 6)
  expect_length(col, 6)
  expect_true(is_colour_vector(col))

  col <- milestone_palette("rainbow", 7)
  expect_length(col, 7)
  expect_true(is_colour_vector(col))

  col <- milestone_palette("auto", 8)
  expect_length(col, 8)
  expect_true(is_colour_vector(col))
})
