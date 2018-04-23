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

If the topology is very simple (or should be represented in one
dimension)

    plot_onedim(task)

    ## [1] "M1" "M2" "M3" "M4" "M5" "M6"

![](.readme_files/unnamed-chunk-3-1.png)

If the topology is a tree

    plot_dendro(task)

    ## [1] "M1" "M2" "M3" "M4" "M5" "M6"

![](.readme_files/unnamed-chunk-4-1.png)

If the topology is more complex

    plot_graph(task)

![](.readme_files/unnamed-chunk-5-1.png)

Plotting a grouping
-------------------

    grouping_assignment <- task$prior_information$grouping_assignment

    plot_onedim(task, grouping_assignment=grouping_assignment)

    ## [1] "M1" "M2" "M3" "M4" "M5" "M6"

![](.readme_files/unnamed-chunk-7-1.png)

    plot_dendro(task, grouping_assignment=grouping_assignment)

    ## [1] "M1" "M2" "M3" "M4" "M5" "M6"

![](.readme_files/unnamed-chunk-7-2.png)

    plot_graph(task, grouping_assignment=grouping_assignment)

![](.readme_files/unnamed-chunk-7-3.png)

Plotting expression of one gene
-------------------------------

    gene_oi <- first(colnames(task$counts))

    plot_onedim(task, gene_oi = gene_oi)

    ## [1] "M1" "M2" "M3" "M4" "M5" "M6"

![](.readme_files/unnamed-chunk-9-1.png)

    plot_dendro(task, gene_oi = gene_oi)

    ## [1] "M1" "M2" "M3" "M4" "M5" "M6"

![](.readme_files/unnamed-chunk-9-2.png)

    plot_graph(task, gene_oi = gene_oi)

![](.readme_files/unnamed-chunk-9-3.png)

Plotting expression of a lot of genes
-------------------------------------

    plot_heatmap(task)

    ## [1] "M1" "M2" "M3" "M4" "M5" "M6"

![](.readme_files/unnamed-chunk-10-1.png)

    plot_genes(task)

    ## [1] "M1" "M2" "M3" "M4" "M5" "M6"

![](.readme_files/unnamed-chunk-11-1.png)

Comparing trajectories
----------------------

    pseudotime <- task$counts %>% prcomp() %>% {.$x[, 1]}
    prediction <- dynwrap::wrap_data("dummy_prediction", task$cell_ids) %>% 
      dynwrap::add_linear_trajectory_to_wrapper(pseudotime)

    plot_strip_onedim(task, prediction)

    ## [1] "M1" "M2" "M3" "M4" "M5" "M6"
    ## [1] "milestone_start" "milestone_end"

![](.readme_files/unnamed-chunk-12-1.png)
