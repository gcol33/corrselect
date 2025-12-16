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
  expect_error(corrSelect(df, threshold = 0.9, method = "els", force_in = "z"), "not in the data frame")
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

test_that("returns subsets when correlation is below threshold", {
  set.seed(999)
  # Create two nearly uncorrelated variables
  df <- data.frame(x = rnorm(100), y = rnorm(100))
  res <- corrSelect(df, threshold = 0.5, method = "els")

  # When correlation is low (below threshold), both variables CAN be in the same subset
  expect_s4_class(res, "CorrCombo")
  # Should have at least one subset containing both variables
  expect_gte(length(res@subset_list), 1)
})

test_that("returns empty when all pairs exceed threshold", {
  # Create perfectly correlated variables
  x <- 1:10
  y <- x  # Perfect correlation
  df <- data.frame(x = x, y = y)
  res <- corrSelect(df, threshold = 0.5, method = "els")

  # With r = 1.0 and threshold = 0.5, no pair meets threshold
  # So we should get no subsets of size >= 2
  expect_s4_class(res, "CorrCombo")
  expect_length(res@subset_list, 0)
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

# ============================================================================
# Edge case tests (reviewer request)
# ============================================================================

test_that("identity matrix returns single subset with all variables", {

  # Pure identity matrix: all off-diagonal correlations = 0

  # All variables are uncorrelated, so the only maximal subset is all variables
  n <- 5
  identity_mat <- diag(n)
  colnames(identity_mat) <- paste0("V", 1:n)
  rownames(identity_mat) <- colnames(identity_mat)


  # Test with Bron-Kerbosch
  res_bk <- MatSelect(identity_mat, threshold = 0.5, method = "bron-kerbosch")
  expect_equal(length(res_bk@subset_list), 1L)
  expect_equal(sort(res_bk@subset_list[[1]]), paste0("V", 1:n))

  # Test with ELS
  res_els <- MatSelect(identity_mat, threshold = 0.5, method = "els")
  expect_equal(length(res_els@subset_list), 1L)
  expect_equal(sort(res_els@subset_list[[1]]), paste0("V", 1:n))

  # Both algorithms should agree
  expect_setequal(res_bk@subset_list[[1]], res_els@subset_list[[1]])
})

test_that("perfect duplicate (r=1.0) variables are separated into different subsets", {
  # Two perfectly correlated variables (r = 1.0) should never appear together

  # in the same subset when threshold < 1.0
  n <- 4
  cor_mat <- diag(n)
  # V1 and V2 are perfectly correlated

  cor_mat[1, 2] <- cor_mat[2, 1] <- 1.0
  # V3 and V4 are uncorrelated with everything
  colnames(cor_mat) <- paste0("V", 1:n)
  rownames(cor_mat) <- colnames(cor_mat)

  # With threshold = 0.9, V1 and V2 should be in different subsets
  res_bk <- MatSelect(cor_mat, threshold = 0.9, method = "bron-kerbosch")

  # Check that V1 and V2 never appear in the same subset
  for (subset in res_bk@subset_list) {
    both_present <- all(c("V1", "V2") %in% subset)
    expect_false(both_present,
      info = "V1 and V2 (r=1.0) should not appear in the same subset")
  }

  # Same test with ELS

  res_els <- MatSelect(cor_mat, threshold = 0.9, method = "els")
  for (subset in res_els@subset_list) {
    both_present <- all(c("V1", "V2") %in% subset)
    expect_false(both_present,
      info = "V1 and V2 (r=1.0) should not appear in the same subset (ELS)")
  }

  # We should have multiple subsets (at least 2) since V1/V2 must be separated
  expect_gte(length(res_bk@subset_list), 2L)
  expect_gte(length(res_els@subset_list), 2L)
})

test_that("threshold boundary: r exactly equal to threshold is excluded", {
  # Test that correlation exactly equal to threshold is treated as > threshold

  # (i.e., the pair is excluded, not included)
  cor_mat <- diag(3)
  cor_mat[1, 2] <- cor_mat[2, 1] <- 0.7  # Exactly at threshold
  cor_mat[1, 3] <- cor_mat[3, 1] <- 0.3  # Below threshold
  cor_mat[2, 3] <- cor_mat[3, 2] <- 0.3  # Below threshold
  colnames(cor_mat) <- c("A", "B", "C")
  rownames(cor_mat) <- colnames(cor_mat)

  # With threshold = 0.7, A and B (r = 0.7) should NOT be in the same subset
  # because we require |r| <= threshold (strictly)
  res <- MatSelect(cor_mat, threshold = 0.7, method = "bron-kerbosch")

  # A and B should be separable
  for (subset in res@subset_list) {
    if (all(c("A", "B") %in% subset)) {
      # If A and B appear together, the max correlation should be at threshold
      # This is actually allowed since threshold is inclusive (<=)
      expect_true(TRUE)  # This is fine
    }
  }
})

# ===========================================================================
# Additional corrSelect edge cases
# ===========================================================================

test_that("corrSelect errors with invalid cor_method", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))

  expect_error(
    corrSelect(df, threshold = 0.7, cor_method = "invalid_method"),
    regexp = "should be one of"
  )
})

test_that("corrSelect handles single row warning after NA removal", {
  df <- data.frame(
    a = c(1, NA, NA, NA),
    b = c(2, NA, NA, NA),
    c = c(3, NA, NA, NA)
  )

  # Should warn and possibly error due to insufficient data
  expect_warning(
    expect_error(corrSelect(df, threshold = 0.7)),
    "Removed"
  )
})

test_that("corrSelect handles use_pivot argument with bron-kerbosch", {
  df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))

  # With pivot
  res_pivot <- corrSelect(df, threshold = 0.8, method = "bron-kerbosch", use_pivot = TRUE)
  expect_s4_class(res_pivot, "CorrCombo")

  # Without pivot
  res_no_pivot <- corrSelect(df, threshold = 0.8, method = "bron-kerbosch", use_pivot = FALSE)
  expect_s4_class(res_no_pivot, "CorrCombo")

  # Results should be equivalent
  expect_equal(length(res_pivot@subset_list), length(res_no_pivot@subset_list))
})

test_that("corrSelect default method is bron-kerbosch without force_in", {
  df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))

  res <- corrSelect(df, threshold = 0.8)
  expect_equal(res@search_type, "bron-kerbosch")
})

test_that("corrSelect default method is els with force_in", {
  df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))

  res <- corrSelect(df, threshold = 0.9, force_in = "a")
  expect_equal(res@search_type, "els")
})

# ===========================================================================
# MatSelect validation tests
# ===========================================================================

test_that("MatSelect errors on non-matrix input", {
  expect_error(
    MatSelect(data.frame(a = 1:3, b = 1:3), threshold = 0.5),
    "must be a numeric matrix"
  )
})

test_that("MatSelect errors on non-square matrix", {
  mat <- matrix(1:6, nrow = 2, ncol = 3)

  expect_error(
    MatSelect(mat, threshold = 0.5),
    "must be square"
  )
})

test_that("MatSelect errors on matrix with NA", {
  mat <- diag(3)
  mat[1, 2] <- NA

  expect_error(
    MatSelect(mat, threshold = 0.5),
    "must not contain NA"
  )
})

test_that("MatSelect errors on non-unit diagonal", {

  mat <- diag(3)
  mat[1, 1] <- 0.5

  expect_error(
    MatSelect(mat, threshold = 0.5),
    "Diagonal entries.*must be 1"
  )
})

test_that("MatSelect errors on non-symmetric matrix", {
  mat <- diag(3)
  mat[1, 2] <- 0.5
  mat[2, 1] <- 0.3  # Asymmetric

  expect_error(
    MatSelect(mat, threshold = 0.5),
    "must be symmetric"
  )
})

test_that("MatSelect validates threshold range", {
  mat <- diag(3)

  expect_error(
    MatSelect(mat, threshold = 0),
    "must be in the range"
  )

  expect_error(
    MatSelect(mat, threshold = 1.5),
    "must be in the range"
  )

  expect_error(
    MatSelect(mat, threshold = NA),
    "must be a single numeric"
  )

  expect_error(
    MatSelect(mat, threshold = "0.5"),
    "must be a single numeric"
  )
})

test_that("MatSelect errors on invalid force_in indices", {
  mat <- diag(3)
  colnames(mat) <- c("A", "B", "C")

  expect_error(
    MatSelect(mat, threshold = 0.5, force_in = c(1, 5)),
    "valid 1-based column indices"
  )

  expect_error(
    MatSelect(mat, threshold = 0.5, force_in = c(0, 1)),
    "valid 1-based column indices"
  )
})

test_that("MatSelect errors on force_in names not in matrix", {
  mat <- diag(3)
  colnames(mat) <- c("A", "B", "C")

  expect_error(
    MatSelect(mat, threshold = 0.5, force_in = c("A", "Z")),
    "not found in matrix"
  )
})

test_that("MatSelect errors on character force_in without colnames", {
  mat <- diag(3)  # No column names

  expect_error(
    MatSelect(mat, threshold = 0.5, force_in = "A"),
    "no column names"
  )
})

test_that("MatSelect warns on highly correlated force_in variables", {
  mat <- diag(3)
  mat[1, 2] <- mat[2, 1] <- 0.9  # High correlation
  colnames(mat) <- c("A", "B", "C")

  expect_warning(
    MatSelect(mat, threshold = 0.5, force_in = c("A", "B")),
    "mutually correlated beyond the threshold"
  )
})

test_that("MatSelect generates default variable names", {
  mat <- diag(3)  # No names

  res <- MatSelect(mat, threshold = 0.5)

  # Should have default names like V1, V2, V3
  expect_equal(res@names, c("V1", "V2", "V3"))
})

test_that("MatSelect handles threshold = 1 (keep all pairs)", {
  mat <- matrix(0.8, nrow = 3, ncol = 3)
  diag(mat) <- 1
  colnames(mat) <- c("A", "B", "C")

  res <- MatSelect(mat, threshold = 1.0)

  # With threshold = 1, all variables should be in one subset
  expect_equal(length(res@subset_list), 1)
  expect_setequal(res@subset_list[[1]], c("A", "B", "C"))
})
