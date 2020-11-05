# compute means of all numeric columns in df
# output: a data.frame
col_means <- function(df, na.rm = FALSE) {
  
  # if-statement to change vectors, lists an matrices to a data frame
  if (!checkmate::test_data_frame(df)) {
    df <- as.data.frame(df)
  }
  
  # if-statement to check for "empty" data frames. and to treat data frames with
  # 0 rows equal to data frames with 0 columns
  if (checkmate::test_data_frame(df, min.cols = 0, max.cols = 0) |
      checkmate::test_data_frame(df, min.rows = 0, max.rows = 0)) {
    warning("Data frame contains 0 rows or 0 columns.")
    df <- NULL
  }
  
  # Checking if data frame contains a factor variable and outputting a warning
  for (i in seq_along(df)) {
    if (checkmate::test_factor(df[, i])) {
      warning(names(df[i]), " is a factor! Removing it from Calculation")
    }
  }

  numeric <- vapply(df[, ,drop = FALSE], is.numeric, logical(1))
  numeric_cols <- df[, numeric, drop = FALSE]
  
  result <- data.frame(lapply(numeric_cols, mean, na.rm = na.rm))
  result
}

# Not Every check vom test-defensive-colmeans.R works but i had a problem to get
# except_warning() in line with statements like except_equal()