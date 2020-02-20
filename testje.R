library(dynomanuscript)
library(tidyverse)

# dynverse_folder <- "/home/rcannood/Workspace/dynverse"
dynverse_folder <- "/home/wouters/thesis/projects/dynverse"
devtools::load_all(paste0(dynverse_folder, '/dynplot2/'))
devtools::load_all(paste0(dynverse_folder, '/libraries/scvelo/'))
devtools::load_all(paste0(dynverse_folder, '/dynwrap/'))

Sys.setenv(DYNOMANUSCRIPT_PATH = paste0(dynverse_folder, "/manuscripts/dyno_manuscript/"))

experiment("01-datasets/adipose_differentiation-merick")
experiment("01-datasets/dyngen-bifurcating")

dataset <- read_rds(output_file("dataset.rds"))
# dataset <- dataset %>% add_velocity()
# dataset <- dataset %>% add_velocity(mode = "dynamic")
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
plot_dimred(dataset, trajectory = trajectory, dimred = dimred, size_trajectory = 1)

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

cell_info <- dataset$cell_info

trajectory <- trajectory %>% label_milestones(c("2" = "progenitor", "4" = "", "1" = "end 1", "3" = "end 2"))


dataset$expression

{
  feature_info  <- feature_info %>%
    mutate(label = symbol) %>%
    mutate(
      interesting = symbol %in% c("Dpp4"),
      random = sample(c(TRUE, FALSE), n(), replace = TRUE),
      random2 = runif(n())
    )

  heatmap <- plot_heatmap(
    dataset,
    trajectory,
    features_oi = features_oi,
    feature_info = feature_info,
    cell_info = cell_info,
    plot_velocity = "none",
    feature_annotation = list(
      # labels = annotate_labels(aes(show = interesting)),
      interesting = annotate_simple(aes(fill = interesting))
      # random2 = annotate_simple(aes(fill = random2))
    ),
    cell_annotation = list(
       `Clustering` = annotate_simple(aes(fill = cluster))
    )
  )

  cairo_pdf("~/test.pdf", width = 15, height = 15)
  ComplexHeatmap::draw(heatmap)
  dev.off()
  invisible()
}
