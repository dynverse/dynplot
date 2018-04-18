library(testthat)
library(dynplot)
library(dynutils)
library(dynwrap)
library(dplyr)
library(ggplot2)
library(purrr)

Sys.setenv("R_TESTS" = "")

test_check("dynplot")
