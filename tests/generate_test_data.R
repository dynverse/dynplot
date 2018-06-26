library(testthat, warn.conflicts = FALSE)
library(dynplot, warn.conflicts = FALSE)
library(dynutils, warn.conflicts = FALSE)
library(dynwrap, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(dyndimred, warn.conflicts = FALSE)

save_test <- function(x, name) {
  write_rds(x, paste0(rprojroot::find_testthat_root_file(), "/", name, ".rds"))
  x
}

toy_tasks_tree_directed <- dyntoy::toy_tasks %>%
  group_by(trajectory_type) %>%
  filter(dplyr::row_number() == 1) %>%
  ungroup() %>%
  filter(trajectory_type %in% c("directed_linear", "bifurcation", "multifurcation", "rooted_tree", "rooted_binary_tree")) %>%
  save_test("toy_tasks_tree_directed")

toy_tasks_tree_undirected <- toy_tasks_tree_directed %>%
  mutate(milestone_network = map(milestone_network, function(x) {x$directed <- FALSE;x})) %>%
  save_test("toy_tasks_tree_undirected")

toy_tasks_tree <- bind_rows(toy_tasks_tree_directed, toy_tasks_tree_undirected) %>%
  save_test("toy_tasks_tree")


toy_tasks_connected_directed <- dyntoy::toy_tasks %>%
  group_by(trajectory_type) %>%
  filter(dplyr::row_number() == 1) %>% ungroup() %>%
  filter(trajectory_type != "disconnected_directed_graph") %>%
  save_test("toy_tasks_connected_directed")

toy_tasks_connected_undirected <- toy_tasks_connected_directed %>%
  mutate(milestone_network = map(milestone_network, function(x) {x$directed <- FALSE;x})) %>%
  save_test("toy_tasks_connected_undirected")

toy_tasks_connected <- bind_rows(toy_tasks_connected_directed, toy_tasks_connected_undirected) %>%
  save_test("toy_tasks_connected")


toy_tasks <- bind_rows(dyntoy::toy_tasks, toy_tasks_connected_undirected) %>%
  save_test("toy_tasks")
