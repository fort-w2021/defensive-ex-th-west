# compute means of all numeric columns in df
# output: a data.frame
col_means <- function(df, na.rm = FALSE) {
  if (!checkmate::test_data_frame(df)) {
    df <- as.data.frame(df)
  }
  for (i in 1:ncol(df)) {
    if (checkmate::test_factor(df[, i])) {
      warning("Factor!")
    }
  }

  numeric <- vapply(df[, ,drop = FALSE], is.numeric, logical(1))
  numeric_cols <- df[, numeric, drop = FALSE]
  
  result <- data.frame(lapply(numeric_cols, mean, na.rm = na.rm))
  result
}
