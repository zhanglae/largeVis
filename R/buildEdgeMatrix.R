
#' Build an edge-weight matrix for the LargeVis algorithm.
#'
#'
#' @export
buildEdgeMatrix <- function(x, ...) UseMethod("buildEdgeMatrix")

#' @param i Indices of one node of the nearest-neighbor graph.
#' @param j Indices of the other node.
#' @param p Integer vector of pointers to the initial index of elements of each column. See \code{\link{Matrix::CsparseMatrix}}.
#' @param d The distances between the nodes identified in parameters \code{i} and \code{j}.
#' @param perplexity See the paper for discussion.
#' @param verbose Verbosity
#'
#' @details Implements the portion of the LargeVis algorithm that converts distances between nearest neighbors to an
#' edge-weight graph.
#'
#' @return A list containing: \describe {
#' \item{"sigmas"} {A vector of \eqn{2 \dot \sigma^2} calculated for each node.}
#' \item{"wij"} {A symmetric, sparse matrix of the weights for each edge between nearest neighbors.}
#' }
#'
#' @export
#' @rdname buildEdgeMatrix

buildEdgeMatrix.default <- function(i,
                                    j,
                                    p,
                                    d,
                                    perplexity = 50,
                                    verbose = TRUE) {
  N <- max(max(i), max(j)) + 1

  if (verbose) {
    progress <- txtProgressBar(max = N, title = "sigmas")
    cat("Estimating sigmas\n")
  }

  perplexity = log2(perplexity)
  sigmas <- parallel::mclapply(1:N, FUN = function(idx) {
    if (verbose) setTxtProgressBar(progress, idx)
    x_i <- d[(p[idx] + 1):(p[idx + 1])]
    ret <- optimize(f = sigFunc,
                    x = x_i,
                    perplexity = perplexity,
                    interval = c(0, 10000))
  })
  sigmas <- sapply(sigmas, `[[`, 1)

  if (verbose) close(progress)

  if (any(is.na(sigmas)) + any(is.infinite(sigmas)) + any(is.nan(sigmas)) + any( (sigmas == 0)) > 0)
    stop("An error has propogated into the sigma vector.")

  if (! requireNamespace("Matrix", quietly = T)) stop("The Matrix package must be available.")

  if (verbose) cat("Calculating w_{ij}.\n")
  wij <- distMatrixTowij(i, j, d, sigmas, N, verbose)

  if (any(is.na(wij@x)) || any(is.infinite(wij@x)) || any(is.nan(wij@x)) || any( (wij@x == 0)) > 0)
    stop("An error has propogated into the w_{ij} vector.  This probably means the input data wasn't scaled.")

  return(list(sigmas = sigmas, wij = wij))
}

#' @param x A sparseMatrix
#' @param ... Additional parameters passed on to \code{buildEdgeMatrix.default}.
#' @export
#' @rdname buildEdgeMatrix
buildEdgeMatrix.CsparseMatrix <- function(x, ...) {
  # Will have x@i, which is quickly varying, and x@p, and x@x
  is <- rep(nrow(x), diff(x@p))
  buildEdgeMatrix.default(i = is,
                          j = x@i,
                          p = x@p,
                          d = x@x,
                          ...)
}

#' @inheritParams buildEdgeMatrix.CsparseMatrix
#' @rdname buildEdgeMatrix

buildEdgeMatrix.dgTMatrix <- function(x, ...) {
  # NOT FULLY IMPLEMENTED - THE TRIPLET HAS TO BE SORTED
  ps <- i2p(x@i - 1)
  buildEdgeMatrix.default(i = x@i - 1,
                          j = x@j -1,
                          p = ps,
                          d = x@x,
                          ...)
}

i2p <- function(is) {
  N <- max(is)
  ps <- rep(NA, N + 1)
  diffs <- diff(is)
  ps[is[which(diffs > 0)] + 2] <- which(diffs > 0) + 1
  good <- cumsum(!is.na(ps))
  ps <- ps[good + 1]
  ps[1] <- 1
  ps[length(ps) + 1] <- length(is)
  ps <- ps - 1
  return(ps)
}
