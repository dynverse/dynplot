annotate_features <- function(
  dataset,
  trajectory = dataset,
  features_oi,
  feature_info = dataset$feature_info,

  feature_annotation = NULL
) {
  feature_annotation <- feature_annotation[!names(feature_annotation) == "labels"]

  evaluate_annotations(
    feature_annotation,
    feature_info %>% slice(match(features_oi, feature_id)),
    "feature_id",
    which = "row"
  )
}


annotate_feature_labels <- function(
  dataset,
  trajectory = dataset,
  features_oi,
  feature_info = dataset$feature_info,

  feature_annotation_labels = NULL
) {
  assert_that(features_oi %all_in% feature_info$feature_id)

  mapped <- tibble(
    feature_id = features_oi,
    fontsize = 12
  )

  # evaluate and map all aesthetics
  c(mapped, mapped_legends) %<-% evaluate_annotation(
    feature_annotation_labels,
    feature_info,
    mapped,
    "feature_id"
  )

  # determine feature labels and whether they are annotated using marks
  # add label column if not given, by using the feature ids
  if(is.null(feature_info)) {
    feature_info <- tibble(feature_id = features_oi, label = feature_id)
  } else if (!"label" %in% names(feature_info)) {
    feature_info$label <- feature_info$feature_id
  }
  feature_labels <- feature_info %>% slice(match(features_oi, feature_id)) %>% select(feature_id, label) %>% deframe()
  # filter labels
  # - have a non-NA label
  feature_labels <- feature_labels %>% purrr::discard(is.na)

  # - are TRUE for show, if applicable
  if("show" %in% names(mapped)) {
    feature_labels <- feature_labels[mapped %>% filter(show) %>% pull(feature_id)]
    mapped <- mapped %>% select(-show)
  }

  feature_labels_at <- match(names(feature_labels), features_oi)
  if(length(feature_labels) == length(features_oi)) {
    row_labels <- feature_labels
    show_row_names <- TRUE
    annotation_features <- NULL

    row_names_gp <- create_rownames_gpars(mapped)
  } else if(length(feature_labels_at) > 0) {
    labels_gp <- create_rownames_gpars(mapped[feature_labels_at, ])
    annotation_features <- ComplexHeatmap::anno_mark(
      at = feature_labels_at,
      labels = feature_labels,
      which = "row",
      labels_gp = labels_gp
    )
    show_row_names <- FALSE
    row_labels <- NULL
    row_names_gp <- gpar()
  } else {
    annotation_features <- NULL
    show_row_names <- FALSE
    row_labels <- NULL
    row_names_gp <- gpar()
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
  mapped,
  features_subset = mapped$feature_id
) {
  mapped <- mapped %>%
    filter(feature_id %in% features_subset) %>%
    rename_to_gpar()

  mapped_list <- as.list(mapped %>% select(-feature_id))
  row_names_gp <- invoke(gpar, mapped)

  row_names_gp
}

