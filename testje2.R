

library(clusterProfiler)
library(org.Mm.eg.db)

clust <- hclust(
  as.dist(
    dynutils::correlation_distance(
      t(as.matrix(dataset$expression[,features_oi]))
    )
  ),
  method = "average"
)
cut <- stats::cutree(clust, k = 4)


cluster_ix <- 1

features_cluster <- names(which(cut == cluster_ix))
print(features_cluster)
features_background <- names(cut)

# first map the ensenble gene versions to entrez ids
feature_info$ensembl_gene <- gsub("(.*)\\..*", "\\1", feature_info$feature_id)
library(org.Mm.eg.db)
feature_info$entrez <- AnnotationDbi::mapIds(org.Mm.eg.db, feature_info$ensembl_gene, "ENTREZID", "ENSEMBL")

clipr::write_clip(feature_info %>% filter(feature_id %in% features_oi) %>% filter(!is.na(entrez)) %>% pull(symbol))

ggo <- enrichGO(
  gene = feature_info %>% filter(feature_id %in% features_cluster) %>% filter(!is.na(entrez)) %>% pull(entrez),
  universe = feature_info %>% filter(feature_id %in% features_background) %>% filter(!is.na(entrez)) %>% pull(entrez),
  OrgDb = org.Mm.eg.db,
  ont= "BP"
)
head(ggo)


feature_info


results <- ggo@result %>% filter(qvalue < 0.05) %>% arrange(desc(Count))
results$genes <- results$geneID %>% map(strsplit, "/") %>% map(dplyr::first)


feature_info <- feature_info %>%
  mutate(
    interesting = symbol %in% "Fos"
    # interesting = entrez %in% as.list(org.Mm.eg.db::org.Mm.egGO2ALLEGS["GO:0030215"])[[1]]
    # interesting = entrez %in% as.list(org.Mm.egGO2ALLEGS[first(results$ID)])[[1]]
  )
first(results$Description)







library(enrichR)
dbs <- listEnrichrDbs()
websiteLive <- !is.null(dbs)
if (websiteLive) head(dbs)
if (websiteLive) enriched <- enrichr(c("Runx1", "Gfi1", "Gfi1b", "Spi1", "Gata1", "Kdr"), dbs)


