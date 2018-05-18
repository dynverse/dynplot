library(tidyverse)
# Because of a conflict between dplyr and tidyverse/ggplot2, we import here everything except "vars" from ggplot2
all <- getNamespaceExports("ggplot2")
readr::read_file("R/package.R") %>% stringr::str_replace("@importFrom ggplot2[^\\n]*", paste("@importFrom ggplot2", paste(all[!(all %in% c("vars"))], collapse = " "))) %>% readr::write_file("R/package.R")
