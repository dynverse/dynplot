context("linearise_cells")

id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")

milestone_ids <-  c("man", "in", "possession", "of", "good", "fortune", "must")
milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "man", "in", 1, TRUE,
  "in", "possession", 2, TRUE,
  "in", "of", 3, TRUE,
  "possession", "good", 4, TRUE,
  "of", "fortune", 5, TRUE,
  "good", "must", 6, TRUE,
  "fortune", "must", 7, TRUE
)
milestone_percentages <- tribble(
  ~cell_id, ~milestone_id, ~percentage,
  "truth", "man", .8,
  "truth", "in", .2,
  "universally", "in", .3,
  "universally", "possession", .2,
  "universally", "of", .5,
  "acknowledged", "possession", 0,
  "acknowledged", "good", 1,
  "that", "good", .5,
  "that", "must", .5,
  "a", "good", .9,
  "a", "must", .1,
  "single", "fortune", .6,
  "single", "must", .4
)
divergence_regions <- tribble(
  ~divergence_id, ~milestone_id, ~is_start,
  "be", "in", TRUE,
  "be", "possession", FALSE,
  "be", "of", FALSE
)

trajectory <- wrap_data(
  id = id,
  cell_ids = cell_ids
) %>% add_trajectory(
  milestone_network = milestone_network,
  divergence_regions = divergence_regions,
  milestone_percentages = milestone_percentages
) %>%
  add_root(
    root_milestone_id = first(milestone_ids)
  )

test_that("linearise_trajectory works as expected", {
  linearisation <- linearise_trajectory(trajectory, margin = 0)

  expect_true(all(linearisation$milestone_positions$comp_2 == 0))
  expect_true(all(linearisation$milestone_positions$comp_1 == c(0, 1, 3, 7, 13, 16, 21, 1, 3, 7, 13, 16, 21, 28)))

  expect_equal(
    table(unlist(milestone_network[, c("from", "to")])),
    table(linearisation$milestone_positions$milestone_id),
  )
})

test_that("linearise_trajectory with equal cell width as expected", {
  linearisation <- linearise_trajectory(trajectory, margin = 0, equal_cell_width = TRUE)

  expect_true(all(linearisation$milestone_positions$comp_2 == 0))
  expect_true(all(linearisation$milestone_positions$comp_1 == c(0, 1, 1, 2, 4, 5, 5, 1, 1, 2, 4, 5, 5, 6)))

  expect_equal(
    table(unlist(milestone_network[, c("from", "to")])),
    table(linearisation$milestone_positions$milestone_id),
  )
})
