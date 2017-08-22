#' @export
theme_clean <- function() {
  theme(line = element_blank(), rect = element_blank(), axis.text = element_blank(), axis.title = element_blank(), legend.text = element_text(size = rel(0.8)), legend.title = element_text(hjust = 0), strip.text = element_text(size = rel(0.8)), plot.margin = unit(c(0, 0, 0, 0), "lines"), legend.position="none")
}
