#' Add coloring
#' @param color_cells How to color the cells
#' @param grouping_assignment Tibble containing the assignment of cells to groups of cells
#' @param groups Tibble containing information of the cell groups
#' @param gene_oi Gene to plot expression
#' @param expression_source Source of the gene expression, defaults to `expression`
add_cell_coloring <- function(
  cell_positions,
  color_cells = c("auto", "invisible", "positions", "grouping", "gene"),
  task,
  grouping_assignment=NULL,
  groups=NULL,
  gene_oi=NULL,
  expression_source="expression"
) {
  # check cell coloration
  color_cells <- match.arg(color_cells)
  if(color_cells == "auto") {
    if(!is.null(grouping_assignment)) {
      message("Coloring by grouping")
      color_cells <- "grouping"
    } else if (!is.null(gene_oi)) {
      message("Coloring by expression")
      color_cells <- "gene"
    } else {
      color_cells <- "black"
    }
  } else if(color_cells == "grouping") {
    if(is.null(grouping_assignment)) {stop("Provide grouping_assignment")}
  } else if (color_cells == "gene") {
    if(is.null(gene_oi)) {stop("Provide gene_oi")}
    if(!expression_source %in% names(task)) {stop("Expression source not in task, did you run add_expression_to_wrapper?")}
    if(!gene_oi %in% colnames(task[[expression_source]])) {stop("Gene not found in expression_source")}
  }

  # now create the actual coloring
  if (color_cells == "grouping") {
    if (is.null(groups) | !("color" %in% names(groups))) {
      groups <- tibble(group_id = unique(grouping_assignment$group_id)) %>% mutate(color = milestone_palette_list$auto(n()))
    }
    cell_positions$color <- grouping_assignment$group_id[match(cell_positions$cell_id, grouping_assignment$cell_id)]

    fill_scale <- scale_fill_manual(color_cells, values=set_names(groups$color, groups$group_id), guide=guide_legend(ncol=10))

  } else if (color_cells == "gene") {
    cell_positions$color <- task[[expression_source]][cell_positions$cell_id, gene_oi]
    fill_scale <- scale_fill_distiller(paste0(gene_oi, " ", expression_source), palette = "RdYlBu")
  } else if (is_colour_vector(color_cells)) {
    cell_positions$color <- "trajectories_are_awesome"
    fill_scale <- scale_fill_manual(NULL, values=c("trajectories_are_awesome"=color_cells), guide="none")
  } else if (color_cells == "invisible") {
    cell_positions$color <- "trajectories_are_awesome"
    fill_scale <- scale_fill_manual(NULL, values=c("trajectories_are_awesome"="#00000000"), guide="none")
  }

  lst(cell_positions, fill_scale)
}



