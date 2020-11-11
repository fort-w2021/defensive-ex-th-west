library(testthat)

test_that("throws an error or warning for problematic lag inputs", {
  expect_error(lag(c(1, 2, 3), n = -2), regexp = ">= 0")
  expect_error(lag(c(1, 2, 3), n = Inf), regexp = "finite")
  expect_error(lag(c(1, 2, 3), n  = NA), regexp = "NA")
  expect_error(lag(c(1, 5, 6), n = NULL), regexp = "NULL")
  expect_error(lag(c(3, 5, 3), n = c(1, 2)), regexp = "length")
  expect_error(lag(c(2, 4), n = 4), regexp = "length")
  expect_error(lag(c(1, 5, 6), n = "vier"), regexp = "character")
  expect_error(lag(c(5, 2, 4), n = list(4, 5)), regexp = "list")
})

test_that("throws an error or warning for problematic inputs of x", {
  expect_error(lag(list(3, 4), n = 1L), regexp = "list")
  expect_error(lag(iris), regexp = "data.frame")
  expect_error(lag(diag(4), n = 1L), regexp = "matrix")
})
