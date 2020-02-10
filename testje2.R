

library(clusterProfiler)
library(org.Mm.eg.db)

features_oi <- as.character(feature_importances$feature_id[1:100])

# first map the ensenble gene versions to entrez ids
feature_info$ensembl_gene <- gsub("(.*)\\..*", "\\1", feature_info$feature_id)
feature_info$entrez <- mapIds(org.Mm.eg.db, feature_info$ensembl_gene, "ENTREZID", "ENSEMBL")

ggo <- enrichGO(
  gene = feature_info %>% filter(feature_id %in% features_oi) %>% filter(!is.na(entrez)) %>% pull(entrez),
  universe = feature_info %>% filter(!is.na(entrez)) %>% pull(entrez),
  OrgDb = org.Mm.eg.db,
  ont= "BP"
)

head(ggo)


feature_info


ggo@result %>% filter(qvalue < 0.05) %>% arrange(desc(Count)) %>% head()
