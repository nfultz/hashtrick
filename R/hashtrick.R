# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hash.trick <- function(..., m=256, key=NULL) {
  print("Hello, world!")
}

#' @export
hash_col <- function(x, m=256, algo='md5', key=NULL) {
  UseMethod(x)
}

hash_col.default <- function(x, m, algo, key) {
  x <- as.character(x);
  NextMethod();
}


#' @useDynLib hashtrick R_digest
hash_col.character <- function(x, m, algo, key) {
  .Call(R_digest, x, m, algo, key)
}

hash_col.factor <- function(x, m, algo, key) {
   map <- hash_col(levels(x), m, algo, key)
   factor(map[x], 0:(m-1))
}
