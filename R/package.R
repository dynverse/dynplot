#' Plot all the trajectories
#'
#' Create ggplot2 plots of single-cell trajectories. This can be
#' the trajectory topology itself, projected onto a dimensionality reduction,
#' using a heatmap, or a comparison between two trajectories.
#'
#' @importFrom dplyr bind_rows filter group_by mutate mutate_all n pull sample_n select transmute ungroup do left_join last .data
#' @importFrom dplyr row_number bind_cols full_join summarise inner_join slice rename case_when arrange first mutate_at vars n rename_all
#' @importFrom dplyr summarise_if rename_if top_n lag right_join desc mutate_if rename_at summarise_at
#' @importFrom tidyr gather unnest everything one_of crossing spread nest
#' @importFrom stats as.dist hclust as.dendrogram runif
#' @import methods
#' @importFrom tibble as_tibble tibble enframe deframe lst tribble rownames_to_column column_to_rownames
#' @import ggplot2
#' @importFrom dynutils extract_row_to_list inherit_default_params scale_quantile
#' @importFrom dynutils is_sparse list_as_tibble %all_in% calculate_distance scale_minmax
#' @import dynwrap
#' @importFrom dyndimred dimred_mds dimred_landmark_mds list_dimred_methods dimred_umap
#' @importFrom purrr %>% map map_df map_chr keep pmap map2 set_names map_int map_dbl list_modify discard
#' @importFrom purrr map2_df map2_dbl map2_df
#' @importFrom assertthat assert_that
#' @importFrom tidygraph as_tbl_graph tbl_graph
#' @importFrom stats approx
#'
#' @docType package
#' @name dynplot
NULL
