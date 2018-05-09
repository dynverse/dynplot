#' @rdname get_dimreds
#' @export
dimred_pca = function(x, ndim=3) {
  space = prcomp(t(x))$rotation[,seq_len(ndim)]
  process_dimred(space)
}

#' @rdname get_dimreds
#' @export
dimred_simlr = function(x, ndim=3, nclusters=4) {
  dynutils::install_packages("SIMLR", "dynplot")

  result = SIMLR::SIMLR(t(x), nclusters)
  S = result$S
  space = tsne::tsne(as.dist(max(S)-S), k = ndim)
  process_dimred(space)
}

#' @rdname get_dimreds
#' @export
dimred_mds = function(x, ndim=3) {
  space = space <- stats::cmdscale(dist, k = ndim)(dynutils::correlation_distance(x),ndim = ndim)
  process_dimred(space)
}

#' @rdname get_dimreds
#' @export
dimred_mds_sammon = function(x, ndim=3) {
  dynutils::install_packages(c("MASS"), "dynplot")

  dist = dynutils::correlation_distance(x)
  space <- MASS::sammon(dist, k = ndim)$points
  process_dimred(space)
}

#' @rdname get_dimreds
#' @export
dimred_mds_isomds = function(x, ndim=3) {
  dynutils::install_packages(c("MASS"), "dynplot")

  dist = dynutils::correlation_distance(x)
  space <- MASS::isoMDS(dist, k = ndim)$points
  process_dimred(space)
}

#' @rdname get_dimreds
#' @export
dimred_mds_smacof = function(x, ndim=3) {
  dynutils::install_packages(c("smacof"), "dynplot")

  dist = dynutils::correlation_distance(x)
  space <- smacof::mds(as.dist(dist), type = "ratio", ndim = ndim)$conf
  process_dimred(space)
}

#' @rdname get_dimreds
#' @export
dimred_tsne = function(x, ndim=3) {
  dynutils::install_packages(c("Rtsne"), "dynplot")

  space = Rtsne::Rtsne(as.dist(dynutils::correlation_distance(x)), dims = ndim, is_distance = TRUE, perplexity=5)$Y
  rownames(space) = rownames(x)
  process_dimred(space)
}

#' @rdname get_dimreds
#' @export
dimred_dp = function(x, ndim=3, neigen=NULL) {
  dynutils::install_packages(c("diffusionMap"), "dynplot")

  space = diffusionMap::diffuse(as.dist(dynutils::correlation_distance(x)), neigen=neigen, maxdim=ndim, delta=10e-5)
  process_dimred(space$X[,seq_len(ndim)])
}

#' @rdname get_dimreds
#' @export
dimred_ica = function(x, ndim=3) {
  dynutils::install_packages(c("fastICA"), "dynplot")

  space = fastICA::fastICA(t(scale(t(x))), ndim)$S
  process_dimred(space)
}

#' @rdname get_dimreds
#' @export
dimred_lle = function(x, ndim=3) {
  dynutils::install_packages(c("lle"), "dynplot")

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

#' Dimensionality reduction functions
#' @param x Data matrix
#' @param ndim Number of dimensions
#' @param neigen Number of eigenvalues for diffusionMap
#' @param nclusters Number of clusters for simlr
#' @export
get_dimreds <- function() {
  dimreds <- lsf.str(asNamespace("dynplot"), pattern = "^dimred_")

  dimreds[!dimreds %in% c("dimred_trajectory")]
}
