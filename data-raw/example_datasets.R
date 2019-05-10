example_linear <- dyntoy::generate_dataset(model = "linear")
example_bifurcating <- dyntoy::generate_dataset(model = "bifurcating")
example_tree <- dyntoy::generate_dataset(model = "tree")
example_disconnected <- dyntoy::generate_dataset(model = "disconnected")

usethis::use_data(example_linear, example_bifurcating, example_tree, example_disconnected, compress = "xz", overwrite = TRUE)
