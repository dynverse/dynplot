% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_waypoints.R
\name{project_waypoints_coloured}
\alias{project_waypoints_coloured}
\title{Project the waypoints}
\usage{
project_waypoints_coloured(
  trajectory,
  cell_positions,
  edge_positions = NULL,
  waypoints = dynwrap::select_waypoints(trajectory),
  trajectory_projection_sd = sum(trajectory$milestone_network$length) * 0.05,
  color_trajectory = "none"
)
}
\arguments{
\item{trajectory}{A dynwrap trajectory.}

\item{cell_positions}{The positions of the cells in 2D. Must be a tibble with character column \code{cell_id} and numeric columns \code{comp_1} and \code{comp_2}.}

\item{edge_positions}{The positions of the edges.}

\item{waypoints}{The waypoints to use for projecting. Can by generated using \code{\link[dynwrap:add_waypoints]{dynwrap::select_waypoints()}}.}

\item{trajectory_projection_sd}{The standard deviation of the gaussian kernel.}

\item{color_trajectory}{How to color the trajectory, can be "nearest" for coloring to nearest cell, or "none".}
}
\value{
A named list containing items:
\itemize{
\item segments: A tibble containing columns \code{comp_1} (numeric), \code{comp_2} (numeric), \code{waypoint_id} (character), \code{milestone_id} (character), \code{from} (character), \code{to} (character)
\code{percentage} (numeric), \code{group} (factor), and \code{arrow} (logical).
}
}
\description{
Project the waypoints
}
