#' One-Line Functions for Visualising Trajectories
#'
#' Package that create ggplot2 plots of single-cell trajectories. This can be
#' the trajectory topology itself, projected onto a dimensionality reduction,
#' using a heatmap, or a comparison between two trajectories.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats as.dist hclust as.dendrogram dist runif
#' @import methods
#' @import tibble
#' @import ggplot2
#' @import dynutils
#' @import dynwrap
#' @import dyndimred
#' @import purrr
#' @import dynplot2
#' @import grid
#' @importFrom assertthat assert_that
#' @importFrom magrittr %<>% %$% set_rownames set_colnames
#' @importFrom zeallot %<-%
#'
#' @docType package
#' @name dynplot
NULL
