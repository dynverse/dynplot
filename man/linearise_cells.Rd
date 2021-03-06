% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linearise_cells.R
\name{linearise_cells}
\alias{linearise_cells}
\title{Prepare a trajectory for linearised visualisation.}
\usage{
linearise_cells(
  trajectory,
  margin = 0.05,
  no_margin_between_linear = TRUE,
  one_edge = FALSE,
  equal_cell_width = FALSE
)
}
\arguments{
\item{trajectory}{A dynwrap trajectory.}

\item{margin}{A margin between trajectory segments.}

\item{no_margin_between_linear}{Whether to add a margin only when a branch occurs.}

\item{one_edge}{Whether or not to assign each cell to one cell only. This can occur when a
cell is on a branching point, or in between multiple edges.}

\item{equal_cell_width}{Whether or not to space segments according to cell count.}
}
\value{
A named list with values:
\itemize{
\item \code{milestone_network}: A linearised version of \code{trajectory$milestone_network} with extra columns: \code{add_margin}, \code{n_margins}, \code{cumstart}, \code{cumend}, \code{edge_id}.
\item \code{progressions}: A linearised version of \code{trajectory$progressions} with extra columns:
\code{percentage2}, \code{length}, \code{directed}, \code{add_margin}, \code{n_margins}, \code{cumstart}, \code{cumend}, \code{edge_id}, \code{cumpercentage}.
\item \code{margin}: The used margin (numeric).
}
}
\description{
This is an internal function and should probably not be used manually.
}
\examples{
linearise_cells(example_bifurcating)

}
