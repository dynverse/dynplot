annotate_cells <- function(
  dataset,
  trajectory = dataset,
  linearised,
  cell_info = dataset$cell_info,

  cell_annotation = NULL
) {
  cell_annotation <- cell_annotation[!names(cell_annotation) == "labels"]

  evaluate_annotations(
    cell_annotation,
    cell_info %>% slice(match(linearised$progressions$cell_id, cell_id)),
    "cell_id",
    which = "column"
  )
}
