#' Hash Trick Functions for Model Building
#'
#' \code{hashtrick} builds a matrix using the hash trick, \code{hash_col} hashes a single column.
#'
#' @param ... a list of variables
#' @param x character vector to hash
#' @param m modulo - number of columns for result
#' @param algo \code{'md5'} and others provided by OpenSSL
#' @param key a list of raws. If provided, the hash function generates a different mapping
#'
#' @return for \code{hashtrick}, a n-by-m matrix of the dummy coded features. For \code{hash_col},
#'   a matrix containing hashes, one column per key provided.
#'
#' @export
#' @importFrom Matrix sparseMatrix t
#' @rdname hashtrick
hashtrick <- function(..., m=256L, algo='md5', key=DEFAULT_KEY) {
  stopifnot(is.integer(m))
  vars <- list(...)
  n <- length(vars[[1]])
  out <- matrix(0L, n, m, dimnames = list(names(vars[[1]]), 1:m))

  for(v in vars)
    mark(hash_col(v, m, algo, key), out)

  structure(out, m=m, algo=algo, key=key, class='hashtrick')

}

DEFAULT_KEY=list(charToRaw('package:hashtrick'))

#' @export
#' @rdname hashtrick
hash_col <- function(x, m, algo, key) {
  UseMethod('hash_col', x)
}

#' @export
#' @rdname hashtrick
hash_col.character <- function(x, m, algo, key) {
    hmac(x, m, algo, key)
}

#' @export
#' @rdname hashtrick
hash_col.numeric <- function(x, m, algo, key) {
    hmac(as.character(x), m, algo, key)
}


#' @export
#' @rdname hashtrick
hash_col.factor <- function(x, m, algo, key) {
   hmac(levels(x), m, algo, key)[x,]
}

#' Raw hash functions
#'
#' These are C functions for use when sensitive to performance.
#'
#' @param x     character vector to hash
#' @param m     modulo - an integer
#' @param algo \code{'md5'} and others provided by OpenSSL
#' @param key   a list of raw vectors. Each specifies a different mapping
#' @useDynLib hashtrick R_hmac
hmac <- function(x, m, algo, key) .Call(R_hmac, x, m, algo, key)


#' @useDynLib hashtrick R_mark_matrix
mark <- function(idx, out) .Call(R_mark_matrix, idx, out)



#' @export
makepredictcall.hashtrick <- function(var, call){
    # Stolen from splines package
    if (as.character(call)[1L] != "hashtrick")
        return(call)
    args <- c("m", "algo", "key")


    at <- attributes(var)[args]
    xxx <- call
    xxx[args] <- NULL
    xxx[names(at)] <- at
    xxx
}

#' @export
predict.hashtrick <- function(object, newx, ...)
{
    if(missing(newx))
        return(object)
    a <- c(list(x = newx), attributes(object)[
                c("m", "algo", "key")])
    do.call("hashtrick", a)
}
