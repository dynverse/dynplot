ScaleMilestoneFillColour <- ggproto(
  "ScaleMilestoneFillColour",
  ggplot2::ScaleDiscrete,
  aesthetics = c("colour", "fill"),
  train_df = function(self, df) {
    # get all milestone id levels
    aesthetics <- intersect(self$aesthetics, names(df))
    milestone_ids <- self$milestone_ids
    for (aesthetic in aesthetics) {
      x <- df[[aesthetic]]
      if (is.list(x)) {
        assert_that(is.data.frame(x[[1]]))
        assert_that(is.factor(x[[1]]$milestone_id))
        new_milestone_ids <- map(x, "milestone_id") %>% map(levels) %>% flatten_chr() %>% unique()
      } else if (is.factor(x)) {
        new_milestone_ids <- levels(x)
      } else {
        stop("Invalid milestone colouring")
      }

      milestone_ids <- unique(c(milestone_ids, new_milestone_ids))
    }

    self$milestone_ids <- milestone_ids
    if (length(milestone_ids) > 0) {
      self$milestone_colors <- milestone_palette(length(milestone_ids)) %>%
        set_names(milestone_ids) %>%
        col2rgb() %>%
        t()
    }
  },
  train = function(self, x) {
    print("train")
    self$range <- scales::train_discrete(factor(c(1, 2, 3)))
  },
  map = function(self, x) {
    # we will always work with a dataframe containing milestone_id and percentage
    # if a character is given, convert it to this representation
    if (is.factor(x)) {
      x <- map(x, function(milestone_id) {
        tibble(milestone_id = milestone_id, percentage = 1)
      })
    }

    if (!is.list(x) || !is.data.frame(x[[1]])) {
      stop("Can't color this to milestones")
    }

    y <- map_chr(x, color_milestone_percentages, milestone_colors = self$milestone_colors)
    y
  },
  get_breaks = function(self) {
    map(self$milestone_ids, function(milestone_id) {
      tibble(milestone_id = milestone_id, percentage = 1)
    })
  },
  get_labels = function(self, breaks) {
    self$milestone_ids
  },
  milestone_ids = character(),
  values = NULL
)

#' @export
scale_milestones_fillcolour <- function(name = "Milestone") {
  ggproto(NULL, ScaleMilestoneFillColour, name = name)
}

color_milestone_percentages <- function(milestone_percentages, milestone_colors) {
  mix_colors <- function(milid, milpct) {
    color_rgb <- apply(milestone_colors[milid,,drop = FALSE], 2, function(x) sum(x * milpct))
    color_rgb[color_rgb < 0] <- 0
    color_rgb[color_rgb > 256] <- 256
    do.call(rgb, as.list(c(color_rgb, maxColorValue = 256)))
  }

  mix_colors(as.character(milestone_percentages$milestone_id), milestone_percentages$percentage)
}
