
annotate_features <- function(
  dataset,
  trajectory = dataset,
  features_oi,
  feature_info = dataset$feature_info,

  # simple interface
  highlight_features = NULL
) {
  # determine gpars of feature labels
  feature_label_gpars <- tibble(
    feature_id = features_oi
  )
  feature_label_gpars$fontsize <- 12
  if (!is.null(highlight_features)) {
    feature_label_gpars$fontface <- case_when(
      feature_label_gpars$feature_id %in% highlight_features ~ "bold",
      TRUE ~ "plain"
    )
  }

  # determine feature labels and whether they are annotated using marks
  if(is.null(feature_info)) {
    feature_info <- tibble(feature_id = features_oi, label = feature_id)
  } else if (is.null(feature_info$label)) {
    feature_info$label <- feature_info$feature_id
  }
  feature_labels <- feature_info %>% slice(match(features_oi, feature_id)) %>% select(feature_id, label) %>% deframe()
  feature_labels <- feature_labels %>% purrr::discard(is.na)
  feature_labels_at <- match(names(feature_labels), features_oi)
  if(length(feature_labels) == length(features_oi)) {
    row_labels <- feature_labels
    show_row_names <- TRUE
    annotation_features <- NULL

    row_names_gp <- create_rownames_gpars(feature_label_gpars)
  } else {
    labels_gp <- create_rownames_gpars(feature_label_gpars)
    annotation_features <- ComplexHeatmap::anno_mark(
      at = feature_labels_at,
      labels = feature_labels,
      which = "row",
      labels_gp = labels_gp
    )
    show_row_names <- FALSE
    row_labels <- NULL
    row_names_gp <- NULL
  }

  lst(
    annotation_features,
    feature_labels,
    show_row_names,
    row_labels,
    row_names_gp
  )
}



create_rownames_gpars <- function(
  feature_label_gpars,
  features_subset = feature_label_gpars$feature_id
) {
  feature_label_gpars <- feature_label_gpars %>% filter(feature_id %in% features_subset)

  feature_label_gpars_list <- as.list(feature_label_gpars %>% select(-feature_id))
  row_names_gp <- invoke(gpar, feature_label_gpars_list)

  row_names_gp
}
