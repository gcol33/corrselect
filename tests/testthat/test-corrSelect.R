library(testthat)

test_that("corrSelect returns CorrCombo with numeric data", {
  df <- data.frame(A = rnorm(10), B = rnorm(10), C = rnorm(10))
  res <- corrSelect(df, threshold = 0.5, method = "els")
  expect_s4_class(res, "CorrCombo")
})

test_that("fails when only one numeric column remains after skipping", {
  df <- data.frame(x = rnorm(5), y = letters[1:5], z = factor(1:5))
  expect_error(corrSelect(df, threshold = 0.5, method = "els"))
})

test_that("matrix input is converted to data.frame", {
  mat <- matrix(rnorm(100), ncol = 10)
  expect_s4_class(corrSelect(mat, threshold = 0.5, method = "els"), "CorrCombo")
})

test_that("fails with fewer than two columns", {
  df <- data.frame(x = rnorm(10))
  expect_error(corrSelect(df, threshold = 0.5, method = "els"), "at least two")
})

test_that("fails if fewer than two numeric columns remain", {
  df <- data.frame(a = factor(1:5), b = letters[1:5])
  expect_error(corrSelect(df, threshold = 0.5, method = "els"), "Less than two numeric")
})

test_that("force_in accepts column names", {
  df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))
  res <- corrSelect(df, threshold = 0.9, method = "els", force_in = c("a", "b"))
  expect_true(all(c("a", "b") %in% unlist(res@subset_list)))
})

test_that("invalid force_in names trigger error", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  expect_error(corrSelect(df, threshold = 0.9, method = "els", force_in = "z"), "do not exist")
})

test_that("works with NAs (rows are dropped)", {
  df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, 2, 3, 4), c = c(4, 3, 2, 1))
  expect_warning(res <- corrSelect(df, threshold = 0.9, method = "els"), "Removed")
  expect_s4_class(res, "CorrCombo")
})

test_that("bron-kerbosch with use_pivot = FALSE runs", {
  df <- data.frame(A = rnorm(10), B = rnorm(10), C = rnorm(10))
  res <- corrSelect(df, threshold = 0.7, method = "bron-kerbosch", use_pivot = FALSE)
  expect_s4_class(res, "CorrCombo")
  expect_equal(res@search_type, "bron-kerbosch")
})

test_that("correct slots are filled", {
  df <- data.frame(x = rnorm(10), y = rnorm(10))
  res <- corrSelect(df, threshold = 0.5, method = "els")
  expect_equal(
    slotNames(res),
    c("subset_list", "avg_corr", "min_corr", "max_corr",
      "names", "threshold", "forced_in", "search_type", "cor_method", "n_rows_used")
  )
})

test_that("min and max correlations are valid", {
  df <- data.frame(x = rnorm(10), y = rnorm(10), z = rnorm(10))
  res <- corrSelect(df, threshold = 0.5, method = "els")
  expect_length(res@min_corr, length(res@subset_list))
  expect_true(all(res@max_corr >= res@min_corr, na.rm = TRUE))
})

test_that("works with tibble-like input", {
  df <- as.data.frame(tibble::tibble(x = rnorm(5), y = rnorm(5)))
  res <- corrSelect(df, threshold = 0.9, method = "els")
  expect_s4_class(res, "CorrCombo")
})

test_that("force_in accepts numeric indices", {
  df <- data.frame(x = rnorm(10), y = rnorm(10), z = rnorm(10))
  res <- corrSelect(df, threshold = 0.8, method = "els", force_in = c(1, 3))
  expect_true(all(c("x", "z") %in% unlist(res@subset_list)))
})

test_that("returns empty when no pair meets threshold", {
  df <- data.frame(x = rnorm(10), y = -rnorm(10))
  res <- corrSelect(df, threshold = 0.01, method = "els")

  # Should get an empty CorrCombo (no subsets of size ≥ 2)
  expect_s4_class(res, "CorrCombo")
  expect_length(res@subset_list, 0)
  expect_equal(res@avg_corr, numeric(0))
  expect_equal(res@min_corr, numeric(0))
  expect_equal(res@max_corr, numeric(0))
})


test_that("additional args via ... are accepted", {
  df <- data.frame(x = rnorm(10), y = rnorm(10), z = rnorm(10))
  res <- corrSelect(df, threshold = 0.5, method = "bron-kerbosch", use_pivot = TRUE)
  expect_s4_class(res, "CorrCombo")
})

test_that("corrSelect works with cor_method = 'pearson'", {
  df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))
  res <- corrSelect(df, threshold = 0.8, cor_method = "pearson")
  expect_s4_class(res, "CorrCombo")
  expect_equal(res@cor_method, "pearson")
})

test_that("corrSelect works with cor_method = 'spearman'", {
  df <- data.frame(a = rnorm(10), b = runif(10), c = rnorm(10))
  res <- corrSelect(df, threshold = 0.8, cor_method = "spearman")
  expect_s4_class(res, "CorrCombo")
  expect_equal(res@cor_method, "spearman")
})

test_that("corrSelect works with cor_method = 'kendall'", {
  df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))
  res <- corrSelect(df, threshold = 0.8, cor_method = "kendall")
  expect_s4_class(res, "CorrCombo")
  expect_equal(res@cor_method, "kendall")
})

test_that("corrSelect works with cor_method = 'bicor' if WGCNA is available", {
  skip_if_not_installed("WGCNA")
  df <- data.frame(a = rnorm(20), b = rnorm(20), c = rnorm(20))
  res <- corrSelect(df, threshold = 0.8, cor_method = "bicor")
  expect_s4_class(res, "CorrCombo")
  expect_equal(res@cor_method, "bicor")
})

test_that("corrSelect works with cor_method = 'distance' if energy is available", {
  skip_if_not_installed("energy")
  df <- data.frame(a = rnorm(15), b = rnorm(15), c = rnorm(15))
  res <- corrSelect(df, threshold = 0.8, cor_method = "distance")
  expect_s4_class(res, "CorrCombo")
  expect_equal(res@cor_method, "distance")
})

test_that("corrSelect works with cor_method = 'maximal' if minerva is available", {
  skip_if_not_installed("minerva")
  df <- data.frame(a = rnorm(12), b = rnorm(12), c = rnorm(12))
  res <- corrSelect(df, threshold = 0.8, cor_method = "maximal")
  expect_s4_class(res, "CorrCombo")
  expect_equal(res@cor_method, "maximal")
})

test_that("force_in variables are allowed even if mutually too correlated", {
  df <- data.frame(
    A = rnorm(50),
    B = rnorm(50)
  )
  df$A_dup <- df$A + rnorm(50, sd = 0.001)  # Nearly identical to A

  expect_warning(
    res <- corrSelect(df, threshold = 0.7, force_in = c("A", "A_dup")),
    "Variables in `force_in` are mutually correlated"
  )
  expect_s4_class(res, "CorrCombo")
  all_sets <- unique(unlist(res@subset_list))
  expect_true(all(c("A", "A_dup") %in% all_sets))
})
