library(dynopaper)
library(tidyverse)
devtools::load_all('~/thesis/projects/dynverse/dynplot2/')
devtools::load_all('~/thesis/projects/dynverse/libraries/scvelo/')
devtools::load_all('~/thesis/projects/dynverse/dynwrap/')

experiment("01-datasets/adipose_differentiation-merick")
experiment("01-datasets/dyngen-bifurcating")

dataset <- read_rds(output_file("dataset.rds"))
dataset$velocity$scvelo <- reticulate::py_load_object(output_file("scvelo.pkl"))
trajectory <- read_rds(output_file("trajectory.rds"))

# simplify
trajectory <- trajectory %>% remove_root()
trajectory$milestone_network$directed <- FALSE
trajectory <- trajectory %>% simplify_trajectory()

##
feature_info <- read_tsv(raw_file("index/feature_info.tsv"))
mapper <- function(x, from = "symbol", to = "feature_id") {
  feature_info[["oi"]] <- feature_info[[from]]
  feature_info %>% dplyr::slice(base::match(x, oi)) %>% dplyr::select(!!from, !!to) %>% deframe()
}
##
# dataset$feature_info$symbol <- dataset$feature_info$feature_id
##

trajectory <- scvelo::orient_topology_to_velocity(trajectory, expression_source = dataset, velocity = dataset$velocity)

plot_dimred(dataset, trajectory = trajectory, plot_milestone = "label")
plot_dimred(dataset, trajectory = trajectory, plot_contour = "grouping")
dimred <- dyndimred::dimred_umap(dataset$expression)
plot_dimred(dataset, trajectory = trajectory, dimred = dimred, size_trajectory = 0.5)

plot_dendro(dataset, trajectory = trajectory, color_cells = "milestone", plot_milestone = "label")
plot_dendro(dataset, trajectory = trajectory, color_cells = "grouping")

plot_dimred(dataset, trajectory = trajectory)




feature_importances <- dynfeature::calculate_overall_feature_importance(trajectory, dataset, fi_method = dynfeature::fi_ranger_rf_lite())
feature_importances <- feature_importances %>%
  mutate_if(is.factor, as.character) %>%
  left_join(feature_info, "feature_id") %>%
  filter(!is.na(symbol))
features_oi <- feature_importances %>%
  top_n(100, importance) %>%
  pull(feature_id)


##


trajectory <- trajectory %>% label_milestones(c("2" = "progenitor", "4" = "", "1" = "end 1", "3" = "end 2"))

{
  feature_info  <- feature_info %>%
    mutate(label = symbol) %>%
    mutate(label = symbol) %>% mutate(interesting = symbol %in% c("Dpp4"))
  heatmap <- plot_heatmap(
    dataset,
    trajectory,
    features_oi = features_oi,
    feature_info = feature_info,
    plot_velocity = "none",
    feature_annotation = list(
      labels = annotate_labels(aes(fontface = interesting, color = interesting))
    )
  )

  cairo_pdf("~/test.pdf", width = 15, height = 15)
  ComplexHeatmap::draw(heatmap)
  dev.off()
  invisible()
}



map_labels <- function(x, data) {
  y <- rlang::eval_tidy(x, data)
  tibble(
    feature_id = data$feature_id,
    fontface = case_when(
      y ~ "bold",
      !y ~  plain
    )
  )
}
