library(testthat)

test_that("assocSelect returns CorrCombo with mixed data", {
  df <- data.frame(
    a = rnorm(10),
    b = factor(sample(letters[1:3], 10, TRUE)),
    c = ordered(sample(1:3, 10, TRUE))
  )
  res <- assocSelect(df, threshold = 0.8, method = "els")
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect works with numeric-only input", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  res <- assocSelect(df, threshold = 0.5, method = "els")
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect drops rows with NA", {
  df <- data.frame(a = c(1, NA, 2), b = factor(c("x", "y", "x")))
  expect_warning(res <- assocSelect(df, threshold = 0.7), "Removed")
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect fails with unsupported types", {
  df <- data.frame(a = rnorm(10), b = Sys.Date() + 1:10)
  expect_error(assocSelect(df), "Unsupported column types")
})


test_that("assocSelect respects threshold and excludes high associations", {
  df <- data.frame(
    a = 1:10,
    b = factor(rep(c("x", "y"), 5)),
    c = factor(rep(c("x", "y"), 5))
  )
  res <- assocSelect(df, threshold = 0.01, method = "els")
  expect_length(res@subset_list, 0)
})


test_that("assocSelect allows forcing variables by name", {
  df <- data.frame(
    x = rnorm(10),
    y = factor(sample(letters[1:3], 10, TRUE)),
    z = ordered(sample(1:3, 10, TRUE))
  )
  res <- assocSelect(df, threshold = 0.9, method = "els", force_in = c("x", "z"))
  expect_true(all(c("x", "z") %in% unlist(res@subset_list)))
})

test_that("assocSelect errors on bad method type", {
  df <- data.frame(a = rnorm(10), b = ordered(sample(1:3, 10, TRUE)))
  expect_error(assocSelect(df, method_num_ord = "not_a_method"),
               regexp = "should be one of")
})

test_that("assocSelect works with tibble input", {
  df <- tibble::tibble(
    a = rnorm(10),
    b = factor(sample(c("a", "b", "c"), 10, TRUE)),
    c = ordered(sample(1:3, 10, TRUE))
  )
  res <- assocSelect(df, threshold = 0.7)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles constant columns", {
  df <- data.frame(
    a = rep(1, 10),
    b = rnorm(10),
    c = factor(rep("x", 10))
  )
  res <- assocSelect(df, threshold = 0.8)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect can use kendall for num-num", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  res <- assocSelect(df, method_num_num = "kendall", threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect can use cramersv for factor-factor", {
  df <- data.frame(
    x = factor(sample(letters[1:3], 10, TRUE)),
    y = factor(sample(letters[1:3], 10, TRUE))
  )
  # cramersv is the default for factor-factor, no parameter needed
  res <- assocSelect(df, threshold = 1.0)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect can use eta for numeric-factor", {
  df <- data.frame(
    x = rnorm(10),
    y = factor(rep(c("a", "b"), 5))
  )
  # eta is the default for numeric-factor, no parameter needed
  res <- assocSelect(df, threshold = 1.0)
  expect_s4_class(res, "CorrCombo")
})
