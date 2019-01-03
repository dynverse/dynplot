#' Add milestone coloring
#' @param color_milestones How to color the cells
#' @param milestones Tibble containing the `milestone_id` and a `color` for each milestone
#'
#' @include milestone_palette.R
add_milestone_coloring <- function(
  milestones = NULL,
  color_milestones = c("auto", "given", get_milestone_palette_names())
) {
  color_milestones <- match.arg(color_milestones)

  if (color_milestones == "given") {
    if (!"color" %in% names(milestones)) {
      stop("Milestone colors need to be given")
    }
  } else if (color_milestones %in% get_milestone_palette_names()) {
    if (!(color_milestones == "auto" && "color" %in% names(milestones))) {
      milestones <- milestones %>%
        mutate(color = milestone_palette(color_milestones, n = n()))
    }
  }

  milestones
}
formals(add_milestone_coloring)$color_milestones <- unique(c("auto", "given", get_milestone_palette_names()))


#' Add coloring
#' @param cell_positions The positions of the cells
#' @param color_cells How to color the cells
#' @param traj The trajectory
#' @param grouping The grouping of the cells
#' @param groups Tibble containing information of the cell groups
#' @param feature_oi feature to plot expression
#' @param expression_source Source of the feature expression, defaults to `expression`
#' @param pseudotime The pseudotime
#' @param milestone_percentages The milestone percentages
#' @inheritParams add_milestone_coloring
add_cell_coloring <- dynutils::inherit_default_params(
  add_milestone_coloring,
  function(
    cell_positions,
    color_cells = c("auto", "none", "grouping", "feature", "milestone", "pseudotime"),
    traj,
    grouping = NULL,
    groups = NULL,
    feature_oi = NULL,
    expression_source = "expression",
    pseudotime = NULL,
    color_milestones = NULL,
    milestones = NULL,
    milestone_percentages = NULL
  ) {
    # check cell coloration
    color_cells <- match.arg(color_cells)
    if (color_cells == "auto") {
      if (!is.null(grouping)) {
        message("Coloring by grouping")
        color_cells <- "grouping"
      } else if (!is.null(feature_oi)) {
        message("Coloring by expression")
        color_cells <- "feature"
      } else if (!is.null(milestones) | !is.null(milestone_percentages)) {
        message("Coloring by milestone")
        color_cells <- "milestone"
      } else if (!is.null(pseudotime)) {
        message("Coloring by pseudotime")
        color_cells <- "pseudotime"
      } else {
        color_cells <- "grey"
      }
    }
    if (color_cells == "grouping") {
      grouping <- get_grouping(traj, grouping)
    } else if (color_cells == "feature") {
      expression <- get_expression(traj, expression_source)
      check_feature(expression, feature_oi)
    } else if (color_cells == "milestone") {
      if (is.null(milestone_percentages)) {
        message("Using milestone_percentages from traj")
        milestone_percentages <- traj$milestone_percentages
      }
      # TODO more checks
    } else if (color_cells == "pseudotime") {
      pseudotime <- check_pseudotime(traj, pseudotime)
      cell_positions$pseudotime <- pseudotime[cell_positions$cell_id]
    }

    # now create the actual coloring
    if (color_cells == "grouping") {
      groups <- check_groups(grouping, groups)

      cell_positions$color <- grouping[match(cell_positions$cell_id, names(grouping))]

      color_scale <- scale_color_manual(color_cells, values = set_names(groups$color, groups$group_id), guide = guide_legend(ncol = 5))

    } else if (color_cells == "feature") {
      cell_positions$color <- expression[cell_positions$cell_id, feature_oi]
      color_scale <- scale_color_distiller(paste0(feature_oi, " expression"), palette = "RdYlBu")
    } else if (is_colour_vector(color_cells)) {
      cell_positions$color <- "trajectories_are_awesome"
      color_scale <- scale_color_manual(NULL, values = c("trajectories_are_awesome" = color_cells), guide = "none")
    } else if (color_cells == "milestone") {
      if (is.null(milestones)) {
        testthat::expect_true(all(milestone_percentages$milestone_id %in% traj$milestone_ids), "Not all milestones were found in milestones tibble. Supply milestones tibble if supplying milestone_percentages separately.")
        milestones <- tibble(milestone_id = traj$milestone_ids)
      }
      if (!"color" %in% names(milestones)) {
        milestones <- milestones %>% add_milestone_coloring(color_milestones)
      }

      milestone_colors <- set_names(milestones$color, milestones$milestone_id) %>% col2rgb %>% t

      mix_colors <- function(milid, milpct) {
        color_rgb <- apply(milestone_colors[milid,,drop = FALSE], 2, function(x) sum(x * milpct))
        color_rgb[color_rgb < 0] <- 0
        color_rgb[color_rgb > 256] <- 256
        do.call(rgb, as.list(c(color_rgb, maxColorValue = 256)))
      }

      cell_colors <- milestone_percentages %>%
        group_by(cell_id) %>%
        summarise(color = mix_colors(milestone_id, percentage))

      cell_positions <- left_join(cell_positions, cell_colors, "cell_id")

      color_scale <- scale_color_identity(NULL, guide = "none")
    } else if (color_cells == "pseudotime") {
      cell_positions$color <- cell_positions$pseudotime
      color_scale <- viridis::scale_color_viridis("pseudotime")
    } else if (color_cells == "none") {
      cell_positions$color <- "black"
      color_scale <- scale_color_identity()
    }

    lst(cell_positions, color_scale, color_cells)
  }
)

#' Color cells using a background density
#'
#' @param cell_positions The positions of the cells in 2D
#' @param color_density How to color density, can be "none", "grouping", or "feature"
#' @param padding The padding in the edges to the plot, relative to the size of the plot
#' @param nbins Number of bins for calculating the density
#' @param bw Bandwidth, relative to the size of the plot
#' @param density_cutoff Cutoff for density, the lower the larger the areas
#' @param density_cutoff_label Cutoff for density for labelling, the lower the further way from cells
#' @inheritParams add_cell_coloring
add_density_coloring <- function(
  cell_positions,
  color_density = c("none", "grouping", "feature"),
  traj,
  grouping = NULL,
  groups = NULL,
  feature_oi = NULL,
  expression_source = "expression",
  padding = 0.1,
  nbins = 1000,
  bw = 0.2,
  density_cutoff = 0.3,
  density_cutoff_label = density_cutoff / 10
) {
  color_density <- match.arg(color_density)

  if (color_density == "none") return(list())

  if (any(!c("comp_1", "comp_2", "cell_id") %in% colnames(cell_positions))) {stop("Invalid cell positions")}

  xlims <- c(min(cell_positions$comp_1), max(cell_positions$comp_1))
  ylims <- c(min(cell_positions$comp_2), max(cell_positions$comp_2))

  xpad <- diff(xlims) * padding
  ypad <- diff(ylims) * padding

  xlims <- xlims + c(-xpad, xpad)
  ylims <- ylims + c(-ypad, ypad)

  n_bins <- 1000
  xbw <- diff(xlims) * bw
  ybw <- diff(ylims) * bw

  # calculate blank space
  kde <- MASS::kde2d(cell_positions$comp_1, cell_positions$comp_2, h = c(xbw, ybw), lims = c(xlims, ylims), n = 100)
  blank_space <- reshape2::melt(kde$z, value.name = "density") %>%
    filter(density < max(density) * density_cutoff_label) %>%
    mutate(
      comp_1 = kde$x[as.numeric(Var1)],
      comp_2 = kde$y[as.numeric(Var2)]
    ) %>%
    select(comp_1, comp_2)

  # will contain the different plots and scales
  density_plots <- list()

  # calculate specific density
  if (color_density == "grouping") {
    grouping <- get_grouping(traj, grouping)
    groups <- check_groups(grouping, groups)

    # plot density
    group_density <- cell_positions %>%
      mutate(group_id = grouping[cell_id]) %>%
      select(comp_1, comp_2, group_id) %>%
      nest(comp_1, comp_2, .key = "positions") %>%
      mutate(contour = map2(positions, group_id, function(positions, group_id) {
        density <- MASS::kde2d(positions[,1], positions[,2], h = c(xbw, ybw), lims = c(xlims, ylims), n = n_bins)
        level <- max(density$z) * density_cutoff
        contour <- with(density, contourLines(x, y, z, levels = level))
        map2_df(contour, seq_along(contour), function(contour, contour_i) {
          tibble(
            comp_1 = contour$x,
            comp_2 = contour$y,
            contour_id = paste0(group_id, "_", contour_i)
          )
        })
      })) %>%
      unnest(contour)

    density_plots$polygon <- geom_polygon(
      aes(comp_1, comp_2, fill = group_id, group = contour_id),
      data = group_density,
      alpha = 0.4
    )

    density_plots$scale <- scale_fill_manual(color_density, values = set_names(groups$color, groups$group_id), guide = guide_legend(ncol = 5))

    # plot group labels
    centers <- group_density %>% group_by(group_id) %>% summarise_if(is.numeric, mean)

    # find closest empty position to put label
    group_label_positions <- crossing(
        centers %>% rename_if(is.numeric, ~paste0(., "_center")),
        blank_space
      ) %>%
      mutate(
        distance = sqrt((comp_1_center - comp_1)**2 + abs(comp_2_center - comp_2)**2)
      ) %>%
      group_by(group_id) %>%
      top_n(1, -distance)

    # group_label_positions <- group_density %>%
    #   left_join(centers %>% rename_if(is.numeric, ~paste0(., "_center")), "group_id") %>%
    #   mutate(distance = abs(comp_1_center - comp_1) + abs(comp_2_center - comp_2)) %>%
    #   group_by(group_id) %>%
    #   top_n(1, -distance) %>%
    #   ungroup()

    # plot +
    #   geom_label(aes(comp_1, comp_2, label = group_id, fill = group_id), group_label_positions)

    density_plots$labels <-      ggrepel::geom_label_repel(
      aes(comp_1, comp_2, label = group_id, fill = group_id),
      group_label_positions,
      min.segment.length = Inf
    )

  } else if (color_density == "feature") {
    # get expression
    expression <- get_expression(traj, expression_source)
    check_feature(expression, feature_oi)
    expression_oi <- expression[, feature_oi]

    # positions of each cell and their expression
    expression_positions <- cell_positions %>%
      mutate(expression = expression_oi[cell_id])

    # calculate smoothed expression
    smoothed_expression <- smooth_2d(
      expression_positions$comp_1,
      expression_positions$comp_2,
      c(xbw, ybw),
      expression_positions$expression,
      n = 500,
      lims = c(xlims, ylims)
    )

    # fix outer expression, so that the contours become bounded
    minimal <- min(expression_oi) - 100

    smoothed_expression$z[, 1] <- minimal
    smoothed_expression$z[, ncol(smoothed_expression$z)] <- minimal
    smoothed_expression$z[1, ] <- minimal
    smoothed_expression$z[nrow(smoothed_expression$z), ] <- minimal

    # remove expression with no cells in neighboorhood
    smoothed_expression$z[smoothed_expression$density < mean(smoothed_expression$density) * density_cutoff] <- minimal

    # build contour lines
    expression_range <- c(min(expression_oi), max(expression_oi))
    levels <- pretty(expression_range, 10)
    contour <- with(smoothed_expression, contourLines(x, y, z, levels = levels))

    contour_expression <- map2_df(contour, seq_along(contour), function(contour, contour_i) {
      tibble(
        comp_1 = contour$x,
        comp_2 = contour$y,
        contour_id = contour_i,
        expression = contour$level
      )
    }) %>%
      mutate(contour_id = factor(contour_id, levels = unique(contour_id)))


    density_plots$polygon <- geom_polygon(aes(comp_1, comp_2, fill = expression, group = contour_id), contour_expression)
    density_plots$scale <- scale_fill_distiller(palette = "RdBu", limits = expression_range)
  }

  density_plots
}


smooth_2d <- function(x, y, h, e, n, lims) {
  # create points
  gx <- seq(lims[1], lims[2], length = n)
  gy <- seq(lims[3], lims[4], length = n)

  points <- expand.grid(gx, gy) %>% as.matrix()
  colnames(points) <- c("x", "y")

  # distance between points and original points
  dist <- as.matrix(pdist::pdist(points, as.matrix(data.frame(x = x, y = y))))

  # contributions of each point
  contributions <- dnorm(dist, sd = mean(h)/4)

  # calculate the weights based on the data
  weights <- matrix(rep(e, nrow(dist)), nrow = nrow(dist), byrow = TRUE)

  # create values
  z <- rowSums(contributions * weights) / (rowSums(contributions))

  # write points
  list(
    x = gx,
    y = gy,
    z = matrix(z, nrow = length(gy), ncol = length(gx)),
    density = matrix(rowSums(contributions), nrow = length(gy), ncol = length(gx))
  )
}
