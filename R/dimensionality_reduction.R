#' PCA dimensionality reduction
#' @param x Data matrix
#' @param ndim Number of dimensions
#' @export
dimred_pca = function(x, ndim=3) {
  space = prcomp(t(x))$rotation[,seq_len(ndim)]
  process_dimred(space)
}

dimred_simlr = function(x, ndim=3, nclusters=4) {
  requireNamespace("SIMLR")

  result = SIMLR::SIMLR(t(x), nclusters)
  S = result$S
  space = tsne::tsne(as.dist(max(S)-S), k = ndim)
  process_dimred(space)
}

#' MDS dimensionality reduction
#' @param x Data matrix
#' @param ndim Number of dimensions
#' @export
dimred_mds = function(x, ndim=3) {
  requireNamespace("SCORPIUS")

  space = SCORPIUS::reduce_dimensionality(SCORPIUS::correlation_distance(x),ndim = ndim)
  process_dimred(space)
}

dimred_mds_sammon = function(x, ndim=3) {
  requireNamespace("SCORPIUS")
  requireNamespace("MASS")

  dist = SCORPIUS::correlation_distance(x)
  space <- MASS::sammon(dist, k = ndim)$points
  process_dimred(space)
}

dimred_mds_isomds = function(x, ndim=3) {
  requireNamespace("SCORPIUS")
  requireNamespace("MASS")

  dist = SCORPIUS::correlation_distance(x)
  space <- MASS::isoMDS(dist, k = ndim)$points
  process_dimred(space)
}

dimred_mds_smacof = function(x, ndim=3) {
  requireNamespace("SCORPIUS")
  requireNamespace("smacof")

  dist = SCORPIUS::correlation_distance(x)
  space <- smacof::mds(as.dist(dist), type = "ratio", ndim = ndim)$conf
  process_dimred(space)
}

#' tSNE dimensionality reduction
#' @param x Data matrix
#' @param ndim Number of dimensions
#' @export
dimred_tsne = function(x, ndim=3) {
  requireNamespace("SCORPIUS")
  requireNamespace("Rtsne")

  space = Rtsne::Rtsne(as.dist(SCORPIUS::correlation_distance(x)), dims = ndim, is_distance = TRUE, perplexity=5)$Y
  rownames(space) = rownames(x)
  process_dimred(space)
}

#' DP dimensionality reduction
#' @param x Data matrix
#' @param ndim Number of dimensions
#' @param neigen Number of eigen values
#' @export
dimred_dp = function(x, ndim=3, neigen=NULL) {
  requireNamespace("SCORPIUS")
  requireNamespace("diffusionMap")

  space = diffusionMap::diffuse(as.dist(SCORPIUS::correlation_distance(x)), neigen=neigen, maxdim=ndim, delta=10e-5)
  process_dimred(space$X[,seq_len(ndim)])
}

#' ICA dimensionality reduction
#' @param x Data matrix
#' @param ndim Number of dimensions
#' @export
dimred_ica = function(x, ndim=3) {
  requireNamespace("fastICA")

  space = fastICA::fastICA(t(scale(t(x))), ndim)$S
  process_dimred(space)
}

dimred_lle = function(x, ndim=3) {
  requireNamespace("lle")

  k = lle::calc_k(t(scale(t(x))), ndim)
  k = k$k[which.min(k$rho)]
  space = lle::lle(t(scale(t(x))), ndim, k)$Y
  process_dimred(space)
}

process_dimred = function(space) {
  space = as.matrix(space)
  colnames(space) = paste0("Comp", 1:ncol(space))
  space
}


