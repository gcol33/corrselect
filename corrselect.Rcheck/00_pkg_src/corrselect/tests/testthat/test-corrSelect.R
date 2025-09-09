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
test_that("can get more than one combination (named columns)", {
  # Build a 4×4 correlation matrix with names
  cor_mat <- matrix(0.9, nrow = 4, ncol = 4)
  diag(cor_mat) <- 1
  cor_mat[1,2] <- cor_mat[2,1] <- 0.1
  cor_mat[3,4] <- cor_mat[4,3] <- 0.1
  colnames(cor_mat) <- paste0("V", 1:4)
  rownames(cor_mat) <- colnames(cor_mat)

  # Run BK
  res <- MatSelect(cor_mat, threshold = 0.5, method = "bron-kerbosch", use_pivot = TRUE)

  # Expect exactly two subsets, named "V1","V2" and "V3","V4"
  expect_equal(length(res@subset_list), 2L)

  # Extract the subset names
  subsets <- res@subset_list
  # Sort each for comparison
  subsets <- lapply(subsets, sort)

  # The two expected sets
  expected <- list(c("V1","V2"), c("V3","V4"))

  # Check that each expected set appears in the output
  for (exp in expected) {
    found <- any(vapply(subsets, function(x) identical(x, exp), logical(1)))
    expect_true(found, info = paste("Missing subset", paste(exp, collapse=",")))
  }
})
test_that("ELS can get more than one combination (named columns)", {
  # Build a 4×4 correlation matrix with names
  cor_mat <- matrix(0.9, nrow = 4, ncol = 4)
  diag(cor_mat) <- 1
  cor_mat[1,2] <- cor_mat[2,1] <- 0.1
  cor_mat[3,4] <- cor_mat[4,3] <- 0.1
  colnames(cor_mat) <- paste0("V", 1:4)
  rownames(cor_mat) <- colnames(cor_mat)

  # Run ELS
  res <- MatSelect(cor_mat, threshold = 0.5, method = "els")

  res# Expect exactly two subsets, named "V1","V2" and "V3","V4"
  expect_equal(length(res@subset_list), 2L)

  # Extract the subset names
  subsets <- res@subset_list
  subsets <- lapply(subsets, sort)

  # The two expected sets
  expected <- list(c("V1","V2"), c("V3","V4"))

  # Check that each expected set appears in the output
  for (exp in expected) {
    found <- any(vapply(subsets, function(x) identical(x, exp), logical(1)))
    expect_true(found, info = paste("ELS missing subset", paste(exp, collapse = ",")))
  }
})
library(microbenchmark)

# Create synthetic correlation matrix
make_cor_matrix <- function(p, seed = 123) {
  set.seed(seed)
  X <- matrix(rnorm(200 * p), ncol = p)
  M <- cor(X)
  diag(M) <- 0  # optional, to avoid perfect self-correlation
  return(M)
}

# Wrapper functions
run_bk <- function(mat, threshold, forced = integer()) {
  runBronKerbosch(mat, threshold, forced, usePivot = TRUE)
}

run_els <- function(mat, threshold, forced = integer()) {
  runELS(mat, threshold, forced)
}

# Example: Benchmark on p = 30
cor_mat <- make_cor_matrix(30)
threshold <- 0.5
forcedVec <- c(1,5)  # or try c(1, 5)

microbenchmark(
  ELS = run_els(cor_mat, threshold, forcedVec),
  BK  = run_bk(cor_mat, threshold, forcedVec),
  times = 10
)
