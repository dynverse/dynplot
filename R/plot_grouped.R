#' #' @importFrom ggbeeswarm geom_quasirandom
#' plot_grouped <- dynutils::inherit_default_params(
#'   list(add_milestone_coloring),
#'   function(
#'     traj,
#'     color_milestones,
#'     grouping_assignment = traj$milestone_percentages %>% group_by(cell_id) %>% arrange(desc(percentage)) %>% filter(dplyr::row_number() == 1) %>% select(-percentage) %>% rename(group_id = milestone_id),
#'     groups = tibble(group_id = traj$milestone_ids),
#'     order_cells = c("auto", "pseudotime", "feature"),
#'     pseudotime = NULL,
#'     feature_oi = NULL,
#'     expression_source = "expression"
#'   ) {
#'     order_cells <- match.arg(order_cells)
#'     if(order_cells == "auto") {
#'       if(!is.null(feature_oi)) {
#'         order_cells <- "feature"
#'       } else {
#'         order_cells <- "pseudotime"
#'       }
#'     }
#'
#'     if(order_cells == "pseudotime") {
#'       traj <- check_pseudotime(traj, pseudotime)
#'       cell_positions <- tibble(cell_id = names(traj$pseudotime), y = traj$pseudotime)
#'       y_scale <- scale_y_continuous("pseudotime")
#'     } else if (order_cells == "feature") {
#'       expression <- get_expression(traj, expression_source)
#'       check_feature(expression, feature_oi)
#'       cell_positions <- tibble(cell_id = rownames(traj[[expression_source]]), y = traj[[expression_source]][, feature_oi])
#'       y_scale <- scale_y_continuous(paste0(feature_oi, " ", expression_source))
#'     }
#'
#'     cell_positions <- left_join(cell_positions, grouping_assignment, "cell_id")
#'
#'     groups <- add_milestone_coloring(groups, color_milestones)
#'
#'     ggplot(cell_positions, aes(group_id, y)) +
#'       ggbeeswarm::geom_quasirandom(aes(fill=group_id), shape=21) +
#'       scale_fill_manual("grouping", values=set_names(groups$color, groups$group_id)) +
#'       y_scale +
#'       theme_clean()
#'   }
#' )
