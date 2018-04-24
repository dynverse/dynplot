#' Add milestone coloring
#' @param color_milestones How to color the cells
#' @param milestones Tibble containing the `milestone_id` and a `color` for each milestone
add_milestone_coloring <- function(
  milestones=NULL,
  color_milestones=c("auto", "given")
) {
  color_milestones <- match.arg(color_milestones)

  if(color_milestones == "given") {
    if(!"color" %in% names(milestones)) {stop("Milestone colors need to be given")}
  } else if (color_milestones == "auto") {
    milestones <- milestones %>%
      mutate(color = milestone_palette("Set3", n = n()))
  } else if (color_milestones %in% names(milestone_palette_list)) {
    milestones <- milestones %>%
      mutate(color = milestone_palette(color_milestones, n = n()))
  }

  milestones
}



#' Add coloring
#' @param cell_positions The positions of the cells
#' @param color_cells How to color the cells
#' @param task The task
#' @param grouping_assignment Tibble containing the assignment of cells to groups of cells
#' @param groups Tibble containing information of the cell groups
#' @param gene_oi Gene to plot expression
#' @param expression_source Source of the gene expression, defaults to `expression`
#' @param pseudotime The pseudotime
#' @param milestone_percentages The milestone percentages
#' @inheritParams add_milestone_coloring
add_cell_coloring <- dynutils::inherit_default_params(
  add_milestone_coloring,
  function(
    cell_positions,
    color_cells = c("auto", "invisible", "positions", "grouping", "gene", "milestone", "pseudotime"),
    task,
    grouping_assignment=NULL,
    groups=NULL,
    gene_oi=NULL,
    expression_source="expression",
    pseudotime=NULL,
    color_milestones=NULL,
    milestones=NULL,
    milestone_percentages=NULL
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
      } else if (!is.null(milestones) | !is.null(milestone_percentages)) {
        message("Coloring by milestone")
        color_cells <- "milestone"
      } else if (!is.null(pseudotime)) {
        message("Coloring by pseudotime")
        color_cells <- "pseudotime"
      } else {
        color_cells <- "grey"
      }
    } else if(color_cells == "grouping") {
      if(is.null(grouping_assignment)) {stop("Provide grouping_assignment")}
    } else if (color_cells == "gene") {
      check_gene(task, gene_oi, expression_source)
    } else if (color_cells == "milestone") {
      if(is.null(milestone_percentages)) {
        message("Using milestone_percentages from task")
        milestone_percentages <- task$milestone_percentages
      }
      # TODO more checks
    } else if (color_cells == "pseudotime") {
      pseudotime <- check_pseudotime(task, pseudotime)
      cell_positions$pseudotime <- pseudotime[cell_positions$cell_id]
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
    } else if (color_cells == "milestone") {
      if(is.null(milestones) | !"color" %in% names(milestones)) {
        milestones <- tibble(milestone_id = unique(milestone_percentages$milestone_id)) %>%
          add_milestone_coloring(color_milestones)
      }

      milestone_colors <- set_names(milestones$color, milestones$milestone_id) %>% col2rgb %>% t

      mix_colors <- function(milid, milpct) {
        color_rgb <- apply(milestone_colors[milid,,drop=FALSE], 2, function(x) sum(x * milpct))
        color_rgb[color_rgb < 0] <- 0
        color_rgb[color_rgb > 256] <- 256
        do.call(rgb, as.list(c(color_rgb, maxColorValue = 256)))
      }

      cell_colors <- milestone_percentages %>%
        group_by(cell_id) %>%
        summarise(color = mix_colors(milestone_id, percentage))

      cell_positions <- left_join(cell_positions, cell_colors, "cell_id")

      fill_scale <- scale_fill_identity(NULL, guide="none")
    } else if (color_cells == "pseudotime") {
      cell_positions$color <- cell_positions$pseudotime
      fill_scale <- viridis::scale_fill_viridis("pseudotime")
    }

    lst(cell_positions, fill_scale)
  }
)
