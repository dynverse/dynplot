example_linear <- dyntoy::generate_dataset(model = "linear") %>% add_root()
example_bifurcating <- dyntoy::generate_dataset(model = "bifurcating") %>% add_root()
example_tree <- dyntoy::generate_dataset(model = "tree") %>% add_root()
example_disconnected <- dyntoy::generate_dataset(model = "disconnected") %>% add_root(root_milestone_id = c("T1_M1", "T2_M1"))

usethis::use_data(example_linear, example_bifurcating, example_tree, example_disconnected, compress = "xz", overwrite = TRUE)
