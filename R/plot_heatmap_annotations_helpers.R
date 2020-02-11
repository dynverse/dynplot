
evaluate_annotations <- function(annotations, data, id, which = c("row", "column")) {
  which <- match.arg(which)

  annotation_results <- map2(names(annotations), annotations, function(annotation_title, annotation_element) {
    mapped <- data %>% select(!!id)

    # evaluate and map all aesthetics
    c(mapped, mapped_legends) %<-% evaluate_annotation(
      annotation_element,
      data,
      mapped,
      id
    )

    # create annotation
    gpars <- pmap(mapped, gpar)
    annotation <- anno_supersimple(gpars, which = which)

    # create legend
    mapped_legend <- mapped_legends[[1]]

    if (isTRUE(annotation_element$legend)) {
      legends <-  map2(names(mapped_legends), mapped_legends, function(title, mapped_legend) {
        mapped_legend %>% mutate(col = fill)

        ComplexHeatmap::Legend(
          title = paste0(annotation_title),
          # title = paste0(annotation_title, " \n(", title, ")"),
          labels = mapped_legend$break_label,
          legend_gp = invoke(gpar, rename_to_gpar(mapped_legend)),
          title_gp = gpar(lineheight = 0.9, fontsize = 10, fontface = "bold"),
          border = "#333333"
        )
      })
    } else {
      legends <- list()
    }

    lst(
      annotation,
      legends
    )
  })
  names(annotation_results) <- names(annotations)

  annotations <- map(annotation_results, "annotation")
  legends <- map(annotation_results, "legends") %>% purrr::flatten()

  lst(
    annotations,
    legends
  )
}



evaluate_annotation <- function(annotation, data, mapped, id) {
  # evaluate and map all aesthetics
  assert_that(names(annotation$aes) %all_in% names(annotation$mappers))

  mapped_legends <- list()

  for(aesthetic_id in names(annotation$aes)) {
    # evaluate
    y <- rlang::eval_tidy(annotation$aes[[aesthetic_id]], data)
    names(y) <- data[[id]]

    # summarize by group/weighted?

    # map to aesthetics
    mapped_aesthetic <- annotation$mappers[[aesthetic_id]](y)
    names(mapped_aesthetic) <- data[[id]]

    mapped[[aesthetic_id]] <- mapped_aesthetic[mapped[[id]]]

    # create legend breaks and map those
    if(is.factor(y)) {
      breaks <- levels(y)
      breaks <- breaks[breaks %in% y] # only include breaks that are actually in y
    } else if (is.character(y)) {
      breaks <- unique(y)
    } else if (is.numeric(y)) {
      breaks <- seq(min(y), max(y), length.out = 5)
    } else if (is.logical(y)) {
      breaks <- c(TRUE, FALSE)
    } else {
      stop("Type of annotation not supported: ", class(y))
    }

    aes_id <- as_label(annotation$aes[[aesthetic_id]])
    if(aes_id %in% names(mapped_legends)) {
      mapped_legend <- mapped_legends[[aes_id]]
    } else {
      mapped_legend <- tibble(
        break_label = breaks
      )
    }
    mapped_legend <- mapped_legend %>% mutate(!!aesthetic_id := annotation$mappers[[aesthetic_id]](breaks))
    mapped_legends[[aes_id]] <- mapped_legend
  }

  lst(
    mapped,
    mapped_legends
  )
}





anno_supersimple <- function(gpars, which = c("row", "column")) {
  which = match.arg(which)
  ComplexHeatmap::AnnotationFunction(
    fun = function(index, k, n) {
      n = length(index)

      if(which == "row") {
        pushViewport(viewport(xscale = c(0, 1), yscale = c(n , 0)))
      } else {
        pushViewport(viewport(xscale = c(0, n), yscale = c(0, 1)))
      }

      for (i in seq_along(index)) {
        gpar <- gpars[[index[i]]]
        gpar$col <- '#FFFFFF00'

        if(which == "row") {
          grid.rect(
            x = 0.5,
            y = i-0.5,
            width = 1,
            height = 1,
            gp = gpar,
            default.units = "native"
          )
        } else {
          grid.rect(
            x = i - 0.5,
            y = 0.5,
            width = 1,
            height = 1,
            gp = gpar,
            default.units = "native"
          )
        }
      }

      popViewport()
    },
    var_import = list(
      gpars = gpars
    ),
    n = length(gpars),
    subsetable = TRUE,
    height = unit(0.5, "cm"),
    width = unit(0.5, "cm"),
    which = which
  )
}



# rename from full names (e.g. colour) to gpar names
rename_to_gpar <- function(df) {
  names(df) <-   case_when(
    names(df) == "colour" ~ "col",
    TRUE ~ names(df)
  )

  df
}
