#' function that checks if the input is actually a count
#' @param supposedly_a_count an numeric value to be checked if it is a count
#'
#' @return an integer value (possibly rounded)
count_them <- function(supposedly_a_count) {
  checkmate::assert_number(supposedly_a_count, lower = 0, finite = TRUE)
  if (!checkmate::test_count(supposedly_a_count)) {
    warning(
      "rounding ", supposedly_a_count,
      " to the nearest integer."
    )
    supposedly_a_count <- round(supposedly_a_count)
  }
  supposedly_a_count <- as.integer(supposedly_a_count)
}
