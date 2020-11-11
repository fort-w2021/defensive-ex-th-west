#' Function that replaces the first 'n' arguments of a Vector with 'NA'
#' @param x a Vector
#' @param n number of items in vector to be replaced with 'NA'
#' 
#' @return a vector with the first 'n' elements replaced by 'NA'
lag <- function(x, n = 1L) {
  checkmate::assert_number(n, lower = 0, finite = TRUE)
  # checkmate::assert_numeric(x)
  checkmate::assert_atomic_vector(x)
  xlen <- length(x)
  if (xlen >= n) {
    c(rep(NA, n), x[seq_len(xlen - n)])
  }
  stop("Length of 'n' must not be larger than length of 'x' (<=", xlen, ")")
}
