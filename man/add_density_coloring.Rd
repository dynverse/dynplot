% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_density_coloring.R
\name{add_density_coloring}
\alias{add_density_coloring}
\title{Color cells using a background density}
\usage{
add_density_coloring(
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
  density_cutoff_label = density_cutoff/10
)
}
\arguments{
\item{cell_positions}{The positions of the cells in 2D. Must be a tibble with character column \code{cell_id} and numeric columns \code{comp_1} and \code{comp_2}.}

\item{color_density}{How to color density, can be "none", "grouping", or "feature".}

\item{trajectory}{A dynwrap trajectory.}

\item{grouping}{A grouping of the cells (e.g. clustering) as a named character vector.}

\item{groups}{A tibble containing character columns \code{group_id} and \code{color}. If \code{NULL}, this object is inferred from the \code{grouping} itself.}

\item{feature_oi}{The name of a feature to use for colouring the cells.}

\item{expression_source}{Source of the feature expression, defaults to \code{get_expression(trajectory)}.}

\item{padding}{The padding in the edges to the plot, relative to the size of the plot.}

\item{nbins}{Number of bins for calculating the density.}

\item{bw}{Bandwidth, relative to the size of the plot.}

\item{density_cutoff}{Cutoff for density, the lower the larger the areas.}

\item{density_cutoff_label}{Cutoff for density for labeling, the lower the further way from cells.}
}
\value{
A named list with objects:
\itemize{
\item polygon: A layer to add to the ggplot.
\item scale: A scale to add to the ggplot.
}
}
\description{
Color cells using a background density
}
