<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build
Status](https://travis-ci.org/dynverse/dynplot.svg)](https://travis-ci.org/dynverse/dynplot)
[![codecov](https://codecov.io/gh/dynverse/dynplot/branch/master/graph/badge.svg)](https://codecov.io/gh/dynverse/dynplot)
<img src="man/img/logo.png" align="right" />

Common visualisation tools for single-cell trajectories
=======================================================

Installation
------------

    devtools::install_github("dynverse/dynplot")

On linux, udunits2 has to be installed:

-   Debian / Ubuntu / Linux mint: `sudo apt-get install libudunits2-dev`
-   Fedora / CentOS: `sudo dnf install udunits2-devel`

Usage
-----

The package provides different ways to plot both the topology and
cellular properties of a trajectory:

![](.readme_files/cells-1.png)

And to plot the expression and feature importances of many genes along
the trajectory

![](.readme_files/heatmap-1.png)
