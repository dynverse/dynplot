#' Add colouring to a set of milestones.
#'
#' @param milestones Tibble containing the column `milestone_id` (character).
#'   If `color_milestones` is set to `"given"`, this tibble should also contain a column
#'   `color` (character), containing colour hex codes (e.g. `"#123456"`).
#' @param color_milestones Which palette to use for colouring the milestones
#'   * `auto`: Determine colours automatically. If `color` is already specified in
#'     milestones tibble, this will be used. Otherwise, the colour scheme is determined by
#'     `milestone_palette_list$auto`.
#'   * `given`: The `milestones` object already contains a column `color`.
#'   * `cubeHelix`: Use the `rje::cubeHelix()` palette.
#'   * `Set3`: Use the `RColorBrewer::brewer.pal(name = "Set3")` palette.
#'   * `rainbow`: Use the `grDevices::rainbow()` palette.
#'
#' @returns A tibble containing the input character column `milestone_id` and a character
#'   column `color` containing colour hex-codes (e.g. `"#123456"`).
#'
#' @include milestone_palette.R
#'
# @examples
# milestones <- data.frame(
#   milestone_id = c("A", "B", "C")
# )
# add_milestone_coloring(milestones)
#
# milestones <- data.frame(
#   milestone_id = c("A", "B", "C"),
#   color = c("#111111", "#222222", "#333333")
# )
# add_milestone_coloring(milestones)
add_milestone_coloring <- function(
  milestones = NULL,
  color_milestones = c("auto", "given", get_milestone_palette_names())
) {
  color_milestones <- match.arg(color_milestones)

  if (color_milestones == "given") {
    if (!"color" %in% names(milestones)) {
      stop("Milestone colors need to be given")
    }
  } else if (color_milestones %in% get_milestone_palette_names()) {
    if (!(color_milestones == "auto" && "color" %in% names(milestones))) {
      milestones <- milestones %>%
        mutate(color = milestone_palette(color_milestones, n = n()))
    }
  }

  milestones
}
formals(add_milestone_coloring)$color_milestones <- unique(c("auto", "given", get_milestone_palette_names()))

