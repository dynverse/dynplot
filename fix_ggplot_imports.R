all <- getNamespaceExports("ggplot2")
readr::read_file("R/package.R") %>% stringr::str_replace("@importFrom ggplot2[^\\n]*", paste("@importFrom ggplot2", paste(all[!(all %in% c("vars"))], collapse = " "))) %>% readr::write_file("R/package.R")
