#' Plot all the trajectories
#'
#' Create ggplot2 plots of single-cell trajectories. This can be
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
#' @importFrom assertthat assert_that
#' @importFrom magrittr %<>% %$% set_rownames set_colnames
#'
#' @docType package
#' @name dynplot
NULL
