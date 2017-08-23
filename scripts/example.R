library(tidyverse)

toys <- readRDS("scripts/toys.rds")

#toy_id <- which(toys$perturbator_id == "hairy")[[1]]
toy_id <- which(toys$perturbator_id == "hairy_large")[[4]]

task1 <- toys$gs[[toy_id]]
task2 <- toys$toy[[toy_id]]

plot_strip_connections(task1, task2, reorder = TRUE)
