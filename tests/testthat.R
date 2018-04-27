library(dynplot)
library(tidyverse)

toy_tasks_tree_directed <- dyntoy::toy_tasks %>%
  group_by(trajectory_type) %>%
  filter(row_number() == 1) %>% ungroup() %>%
  filter(trajectory_type %in% c("directed_linear", "bifurcation", "multifurcation", "rooted_tree", "rooted_binary_tree"))

lst(toy_tasks_tree_directed) %>% saveRDS("tests/data.rds")

Sys.setenv("R_TESTS" = "")

test_check("dynplot")
