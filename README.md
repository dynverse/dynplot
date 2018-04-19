<!-- README.md is generated from README.Rmd. Please edit that file -->
dynplot
=======

**dynplot** provides common functionality for plotting trajectories.

[![Build
Status](https://travis-ci.org/dynverse/dynplot.svg)](https://travis-ci.org/dynverse/dynplot)
[![codecov](https://codecov.io/gh/dynverse/dynplot/branch/master/graph/badge.svg)](https://codecov.io/gh/dynverse/dynplot)

Load in a toy dataset

    task <- toy_tasks %>% filter(trajectory_type == "bifurcation") %>% extract_row_to_list(1)

Plotting the topology and cellular positions
--------------------------------------------

In 2D

    plot_default(task)

![](.readme_files/unnamed-chunk-3-1.png)

In 1D

    plot_connections(task$milestone_network)

![](.readme_files/unnamed-chunk-4-1.png)

    plot_connections(task$milestone_network, cell_progressions = task$progressions %>% sample_n(10))

![](.readme_files/unnamed-chunk-4-2.png)

Plotting expression
-------------------

In a heatmap

    plot_heatmap(task)

![](.readme_files/unnamed-chunk-5-1.png)

In line plots

    plot_genes(task)

![](.readme_files/unnamed-chunk-6-1.png)

Comparing trajectories
----------------------

    pseudotime <- task$counts %>% prcomp() %>% {.$x[, 1]}
    prediction <- dynwrap::wrap_data("dummy_prediction", task$cell_ids) %>% 
      dynwrap::add_linear_trajectory_to_wrapper(pseudotime)

    plot_strip_connections(task, prediction)

![](.readme_files/unnamed-chunk-7-1.png)
