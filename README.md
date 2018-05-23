<!-- README.md is generated from README.Rmd. Please edit that file -->
dynplot
=======

**dynplot** provides common functionality for plotting trajectories.

[![Build
Status](https://travis-ci.org/dynverse/dynplot.svg)](https://travis-ci.org/dynverse/dynplot)
[![codecov](https://codecov.io/gh/dynverse/dynplot/branch/master/graph/badge.svg)](https://codecov.io/gh/dynverse/dynplot)

The package provides different ways to plot both the topology and
cellular properties of a trajectory:

    library(dplyr)
    library(dyntoy)
    library(dynplot)
    library(dynutils)
    library(dynwrap)
    library(tidyverse)

    library(patchwork)

    traj <- toy_tasks %>% filter(id == "toy/consecutive_bifurcating_1") %>% extract_row_to_list(1)
    traj <- traj %>% root_trajectory()
    grouping_assignment <- traj$prior_information$grouping_assignment
    groups <- tibble(group_id = unique(grouping_assignment$group_id)) %>% mutate(color=dynplot:::milestone_palette_list$auto(n()))
    features_oi <- apply(traj$counts, 2, sd) %>% sort() %>% names() %>% tail(10)
    feature_oi <- features_oi[[1]]

![](.readme_files/cells-1.png)

And to plot the expression and feature importances of many genes along
the trajectory

![](.readme_files/heatmap-1.png)
