#' dynplot: Plotting Single-Cell Trajectories
#'
#' Visualise a single-cell trajectory as a graph or dendrogram,
#' as a dimensionality reduction or heatmap of the expression data,
#' or a comparison between two trajectories as a pairwise scatterplot
#' or dimensionality reduction projection.
#'
#' @importFrom dplyr bind_rows filter group_by mutate mutate_all n pull sample_n select transmute ungroup do left_join last .data
#' @importFrom dplyr row_number bind_cols full_join summarise inner_join slice rename case_when arrange first mutate_at vars n rename_all
#' @importFrom dplyr summarise_if rename_if top_n lag right_join desc mutate_if rename_at summarise_at
#' @importFrom tidyr gather unnest everything one_of crossing spread nest starts_with
#' @importFrom stats as.dist hclust as.dendrogram runif approx dnorm sd
#' @import methods
#' @importFrom tibble as_tibble tibble enframe deframe lst tribble rownames_to_column column_to_rownames
#' @import ggplot2
#' @importFrom dynutils extract_row_to_list inherit_default_params scale_quantile
#' @importFrom dynutils is_sparse list_as_tibble %all_in% calculate_distance scale_minmax
#' @import dynwrap
#' @importFrom dyndimred dimred_mds dimred_landmark_mds list_dimred_methods dimred_umap
#' @importFrom purrr %>% map map_df map_chr keep pmap map2 set_names map_int map_dbl list_modify discard pmap_df
#' @importFrom purrr map2_df map2_dbl map2_df
#' @importFrom assertthat assert_that
#' @importFrom tidygraph as_tbl_graph tbl_graph
#' @importFrom utils tail head
#' @importFrom ggraph ggraph get_edges create_layout
#' @importFrom ggraph geom_edge_elbow geom_edge_fan geom_edge_loop geom_node_label geom_edge_link
#' @importFrom ggraph scale_edge_linetype_manual scale_edge_width_manual scale_edge_alpha_discrete
#'
#' @docType package
#' @name dynplot
NULL
