#' Hash Trick Functions for Model Building
#'
#' \code{hashtrick} builds a model matrix using the hash trick, \code{hash_col} hashes a single column.
#'
#' @param ... a list of variables
#' @param x character vector to hash
#' @param m modulo - an integer
#' @param algo \code{'md5'} and others provided by OpenSSL
#' @param key a raw vector. If provided, the hash function generates a different mapping
#'
#' @export
#' @importFrom Matrix sparseMatrix t
#' @rdname hashtrick
hashtrick <- function(..., m=256L, algo='md5', key) {
  res <- list(...)
  n <- length(res[[1]])

  res <- lapply(res, hash_col, m, algo, key)

  res <- lapply(res, sparseMatrix, j=0:(n-1), dims=c(m,n), index1=FALSE)

  t(Reduce(`+`, res))
}

#' @export
#' @rdname hashtrick
hash_col <- function(x, m, algo, key) {
  UseMethod('hash_col', x)
}

#' @export
#' @rdname hashtrick
hash_col.default <- function(x, m, algo, key) {
  if(missing(key))
    digest(x, m, algo, key)
  else
    hmac(x, m, algo, key)
}

#' @export
#' @rdname hashtrick
hash_col.factor <- function(x, m, algo, key) {
   hash_col(levels(x), m, algo, key)[x]
}

#' Raw hash functions
#'
#' These are C functions for use when sensitive to performance.
#'
#' @param x     character vector to hash
#' @param m     modulo - an integer
#' @param algo \code{'md5'} and others provided by OpenSSL
#' @param key   a raw vector. If provided, generates a different mapping
#'
#' @useDynLib hashtrick R_digest
#' @export
#' @rdname digest
digest <- function(x, m, algo, ...) .Call(R_digest, x, m, algo)

#' @useDynLib hashtrick R_hmac
#' @export
#' @rdname digest
hmac <- function(x, m, algo, key) .Call(R_hmac, x, m, algo, key)

