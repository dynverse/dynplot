#' Color cells using a background density
#'
#' @param cell_positions The positions of the cells in 2D. Must be a tibble with character column `cell_id` and numeric columns `comp_1` and `comp_2`.
#' @param color_density How to color density, can be "none", "grouping", or "feature".
#' @param padding The padding in the edges to the plot, relative to the size of the plot.
#' @param nbins Number of bins for calculating the density.
#' @param bw Bandwidth, relative to the size of the plot.
#' @param density_cutoff Cutoff for density, the lower the larger the areas.
#' @param density_cutoff_label Cutoff for density for labeling, the lower the further way from cells.
#' @inheritParams add_cell_coloring
#'
#' @returns A named list with objects:
#' * polygon: A layer to add to the ggplot.
#' * scale: A scale to add to the ggplot.
#'
# @examples
# pca <- prcomp(example_bifurcating$expression, rank. = 2)$x
# cell_positions <- pca %>% as.matrix %>% as.data.frame %>% magrittr::set_colnames(c("comp_1", "comp_2")) %>% rownames_to_column("cell_id")
# add_density_coloring(cell_positions, "feature", trajectory = example_bifurcating, feature_oi = "B1_TF1")
add_density_coloring <- function(
  cell_positions,
  color_density = c("none", "grouping", "feature"),
  trajectory,
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

  if (any(!c("comp_1", "comp_2", "cell_id") %in% colnames(cell_positions))) {
    stop("Invalid value for cell_positions. See documentation for more details.")
  }

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
  blank_space <-
    reshape2::melt(kde$z, value.name = "density") %>%
    filter(.data$density < max(.data$density) * density_cutoff_label) %>%
    transmute(
      comp_1 = kde$x[as.numeric(.data$Var1)],
      comp_2 = kde$y[as.numeric(.data$Var2)]
    )

  # will contain the different plots and scales
  density_plots <- list()

  # calculate specific density
  if (color_density == "grouping") {
    grouping <- get_grouping(trajectory, grouping)
    groups <- check_groups(grouping, groups)

    # plot density
    group_density <- cell_positions %>%
      mutate(group_id = grouping[.data$cell_id]) %>%
      select(.data$comp_1, .data$comp_2, .data$group_id) %>%
      nest(positions = c(.data$comp_1, .data$comp_2)) %>%
      mutate(contour = map2(.data$positions, .data$group_id, function(positions, group_id) {
        density <- MASS::kde2d(positions$comp_1, positions$comp_2, h = c(xbw, ybw), lims = c(xlims, ylims), n = n_bins)
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
      unnest(.data$contour)

    density_plots$polygon <- geom_polygon(
      aes(.data$comp_1, .data$comp_2, fill = .data$group_id, group = .data$contour_id),
      data = group_density,
      alpha = 0.4
    )

    density_plots$scale <- scale_fill_manual(color_density, values = set_names(groups$color, groups$group_id), guide = guide_legend(ncol = 5))

    # plot group labels
    centers <- group_density %>% group_by(.data$group_id) %>% summarise_if(is.numeric, mean)

    # find closest empty position to put label
    group_label_positions <- crossing(
      centers %>% rename_if(is.numeric, ~paste0(., "_center")),
      blank_space
    ) %>%
      mutate(
        distance = sqrt((.data$comp_1_center - .data$comp_1)**2 + abs(.data$comp_2_center - .data$comp_2)**2)
      ) %>%
      group_by(.data$group_id) %>%
      top_n(1, -.data$distance)

    density_plots$labels <- ggrepel::geom_label_repel(
      aes(.data$comp_1, .data$comp_2, label = .data$group_id, fill = .data$group_id),
      group_label_positions,
      min.segment.length = Inf,
      show.legend = FALSE
    )

  } else if (color_density == "feature") {
    # get expression
    expression <- get_expression(trajectory, expression_source)
    check_feature(expression, feature_oi)
    expression_oi <- expression[, feature_oi]

    # positions of each cell and their expression
    expression_positions <- cell_positions %>%
      mutate(expression = expression_oi[.data$cell_id])

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
      mutate(contour_id = factor(.data$contour_id, levels = unique(.data$contour_id)))


    density_plots$polygon <- geom_polygon(aes(.data$comp_1, .data$comp_2, fill = .data$expression, group = .data$contour_id), contour_expression)
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
  orig_points <- cbind(x, y)
  dist <- dynutils::calculate_distance(points, orig_points, method = "euclidean")

  # contributions of each point
  contributions <- stats::dnorm(dist, sd = mean(h)/4)

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
