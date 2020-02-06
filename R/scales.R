get_scale_fill_grouping <- function(groups) {
  scale_fill_manual("Grouping", values = set_names(groups$color, groups$group_id), guide = guide_legend(ncol = 2))
}
get_scale_color_grouping <- function(groups) {
  scale_color_manual("Grouping", values = set_names(groups$color, groups$group_id), guide = guide_legend(ncol = 2))
}
