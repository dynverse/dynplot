<!-- README.md is generated from README.Rmd. Please edit that file -->
dynplot
=======

**dynplot** provides common functionality for plotting trajectories.

[![Build
Status](https://travis-ci.org/dynverse/dynplot.svg)](https://travis-ci.org/dynverse/dynplot)
[![codecov](https://codecov.io/gh/dynverse/dynplot/branch/master/graph/badge.svg)](https://codecov.io/gh/dynverse/dynplot)

Load in a toy dataset

    task <- toy_tasks %>% filter(trajectory_type == "bifurcation") %>% extract_row_to_list(1) %>% root_trajectory()

Plotting the topology and cellular positions
--------------------------------------------

If the topology is very simple (or should be represented in one
dimension)

    plot_onedim(task)

![](.readme_files/onedim-1.png)

If the topology is a tree

    plot_dendro(task)

![](.readme_files/dendro-1.png)

If the topology is more complex

    plot_graph(task)

![](.readme_files/graph-1.png)

    plot_topology(task)

![](.readme_files/unnamed-chunk-3-1.png)

Plotting a grouping
-------------------

    grouping_assignment <- task$prior_information$grouping_assignment

    plot_onedim(task, grouping_assignment=grouping_assignment)

![](.readme_files/grouping-1.png)

    plot_dendro(task, grouping_assignment=grouping_assignment)

![](.readme_files/grouping-2.png)

    plot_graph(task, grouping_assignment=grouping_assignment)

![](.readme_files/grouping-3.png)

Plotting expression of one gene
-------------------------------

    gene_oi <- first(colnames(task$counts))

    plot_onedim(task, gene_oi = gene_oi)

![](.readme_files/expression-1.png)

    plot_dendro(task, gene_oi = gene_oi)

![](.readme_files/expression-2.png)

    plot_graph(task, gene_oi = gene_oi)

![](.readme_files/expression-3.png)

Plotting expression of a lot of genes
-------------------------------------

    plot_heatmap(task)

![](.readme_files/heatmap-1.png)

    # plot_genes(task)

Comparing trajectories
----------------------

    pseudotime <- task$counts %>% prcomp() %>% {.$x[, 1]}
    prediction <- dynwrap::wrap_data("dummy_prediction", task$cell_ids) %>%
      dynwrap::add_linear_trajectory_to_wrapper(pseudotime)

    plot_strip_onedim(task, prediction)

![](.readme_files/unnamed-chunk-6-1.png)
