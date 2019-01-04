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
  write_rds(x, paste0(rprojroot::find_testthat_root_file(), "/", name, ".rds"), compress = "xz")
  x
}

one_of_each_directed <-
  dyntoy::toy_datasets %>%
  group_by(trajectory_type) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(id = paste0(trajectory_type, "_directed"))

one_of_each_undirected <-
  one_of_each_directed %>%
  mutate(milestone_network = map(milestone_network, function(x) x %>% mutate(directed = FALSE))) %>%
  mutate(id = paste0(trajectory_type, "_undirected"))

one_of_each <- bind_rows(one_of_each_undirected, one_of_each_directed)

# save tree datasets
trajtype_sel <- c("linear", "bifurcation", "convergence", "multifurcation", "tree")

one_of_each_directed %>%
  filter(trajectory_type %in% trajtype_sel) %>%
  save_test("toy_datasets_tree_directed")

one_of_each_undirected %>%
  filter(trajectory_type %in% trajtype_sel) %>%
  save_test("toy_datasets_tree_undirected")

one_of_each %>%
  filter(trajectory_type %in% trajtype_sel) %>%
  save_test("toy_datasets_tree")

# save connected datasets
trajtype_sel <- c("linear", "bifurcation", "convergence", "multifurcation", "tree", "cycle", "acyclic_graph", "graph")

one_of_each_directed %>%
  filter(trajectory_type %in% trajtype_sel) %>%
  save_test("toy_datasets_connected_directed")

one_of_each_undirected %>%
  filter(trajectory_type %in% trajtype_sel) %>%
  save_test("toy_datasets_connected_undirected")

one_of_each %>%
  filter(trajectory_type %in% trajtype_sel) %>%
  save_test("toy_datasets_connected")

# save all datasets
one_of_each %>%
  save_test("toy_datasets")
