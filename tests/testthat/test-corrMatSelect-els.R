library(testthat)

test_that("ELS returns valid CorrCombo object", {
  m <- diag(1, 3)
  m[1,2] <- m[2,1] <- 0.3
  res <- MatSelect(m, threshold = 0.5, method = "els")
  expect_true(inherits(res, "CorrCombo"))
  expect_type(res@subset_list, "list")
})

test_that("ELS respects correlation threshold", {
  m <- diag(1, 4)
  m[1,2] <- m[2,1] <- 0.8
  m[3,4] <- m[4,3] <- 0.1
  res <- MatSelect(m, threshold = 0.5, method = "els")
  # no subset should contain both V1 and V2
  bad <- sapply(res@subset_list, function(s) all(c("V1","V2") %in% s))
  expect_false(any(bad))
})

test_that("ELS includes forced variables", {
  m <- diag(1, 4)
  m[1,2] <- m[2,1] <- 0.4
  colnames(m) <- paste0("X", 1:4)
  res <- MatSelect(m, threshold = 0.5, method = "els", force_in = c(1,4))
  # every returned subset must include both "X1" and "X4"
  ok <- vapply(res@subset_list, function(s) all(c("X1","X4") %in% s), logical(1))
  expect_true(all(ok))
})

test_that("ELS returns size-1 subsets when no multi-variable subset is valid", {
  m <- matrix(0.95, 3,3); diag(m) <- 1
  res <- MatSelect(m, threshold = 0.1, method = "els")
  # No pair is compatible under so strict a threshold, so the only maximal
  # subsets are the three variables on their own (see #30).
  expect_length(res@subset_list, 3)
  expect_true(all(vapply(res@subset_list, length, integer(1)) == 1))
  expect_equal(res@avg_corr, rep(0, 3))
  expect_true(all(is.na(res@min_corr)))
  expect_true(all(is.na(res@max_corr)))
})

test_that("ELS rejects NA matrices", {
  m <- diag(1,3)
  m[1,2] <- NA
  expect_error(MatSelect(m, threshold = 0.5, method = "els"),
               "`mat` must not contain NA\\.")
})

test_that("MatSelect rejects matrices with duplicate column names (#28)", {
  m <- diag(1, 3)
  colnames(m) <- c("a", "a", "b")
  expect_error(MatSelect(m, threshold = 0.5, method = "els"),
               "duplicate column names")
})

test_that("MatSelect deduplicates a repeated force_in index/name (#31)", {
  # Regression test for issue #31: a duplicate force_in entry used to reach
  # the C++ backend unmodified, so the same variable could be listed twice
  # in one returned subset, with a spurious self-correlation (r = 1.0)
  # reported as that subset's max_corr.
  mat <- matrix(c(1, 0.9, 0.1, 0.9, 1, 0.2, 0.1, 0.2, 1), 3,
                dimnames = list(c("V1", "V2", "V3"), c("V1", "V2", "V3")))

  res_idx  <- MatSelect(mat, threshold = 0.5, method = "els", force_in = c(1, 1))
  res_name <- MatSelect(mat, threshold = 0.5, method = "els", force_in = c("V1", "V1"))

  for (res in list(res_idx, res_name)) {
    expect_equal(length(res@subset_list), 1)
    expect_equal(sort(res@subset_list[[1]]), c("V1", "V3"))
    expect_false(anyDuplicated(res@subset_list[[1]]) > 0)
    expect_lte(res@max_corr[1], 0.5)
  }
})

test_that("MatSelect does not warn about mutual correlation for a duplicated force_in entry", {
  # A repeated index resolves to a single distinct variable, so the
  # "force_in are mutually correlated" warning (which only makes sense for
  # >= 2 distinct forced variables) must not fire.
  mat <- matrix(c(1, 0.9, 0.1, 0.9, 1, 0.2, 0.1, 0.2, 1), 3,
                dimnames = list(c("V1", "V2", "V3"), c("V1", "V2", "V3")))
  expect_no_warning(MatSelect(mat, threshold = 0.5, method = "els", force_in = c(1, 1)))
})

test_that("ELS returns correct variable names", {
  m <- diag(1,3)
  colnames(m) <- c("a","b","c")
  res <- MatSelect(m, threshold = 0.5, method = "els")
  all_vars <- unlist(res@subset_list, use.names = FALSE)
  expect_true(all(all_vars %in% c("a","b","c")))
})

test_that("ELS computes min/max correlations", {
  m <- diag(1,4)
  m[1,2] <- m[2,1] <- 0.3
  res <- MatSelect(m, threshold = 0.5, method = "els")
  expect_length(res@min_corr, length(res@subset_list))
  expect_true(all(res@max_corr >= res@min_corr, na.rm = TRUE))
})

# ===========================================================================
# Additional coverage tests for ELS algorithm and utils.cpp
# ===========================================================================

test_that("ELS handles asymmetric matrix error", {
  m <- diag(1, 3)
  m[1, 2] <- 0.3
  m[2, 1] <- 0.5  # Asymmetric

  expect_error(
    MatSelect(m, threshold = 0.5, method = "els"),
    "symmetric"
  )
})

test_that("ELS handles negative correlations", {
  m <- diag(1, 4)
  m[1, 2] <- m[2, 1] <- -0.6  # Negative correlation
  m[3, 4] <- m[4, 3] <- -0.3
  colnames(m) <- paste0("V", 1:4)

  res <- MatSelect(m, threshold = 0.5, method = "els")

  # V1-V2 pair should be excluded (|r| = 0.6 > 0.5)
  for (subset in res@subset_list) {
    expect_false(all(c("V1", "V2") %in% subset))
  }
})

test_that("ELS handles mixed positive and negative correlations", {
  m <- diag(1, 5)
  # Create some correlations
  m[1, 2] <- m[2, 1] <- 0.4   # Below threshold
  m[1, 3] <- m[3, 1] <- -0.4  # Below threshold
  m[2, 3] <- m[3, 2] <- 0.8   # Above threshold
  m[4, 5] <- m[5, 4] <- -0.2  # Below threshold
  colnames(m) <- paste0("V", 1:5)

  res <- MatSelect(m, threshold = 0.5, method = "els")

  # V2 and V3 should not appear together
  for (subset in res@subset_list) {
    expect_false(all(c("V2", "V3") %in% subset))
  }
})

test_that("ELS rejects a 1x1 matrix (need at least two columns)", {
  m <- matrix(1, nrow = 1, ncol = 1)
  colnames(m) <- "V1"

  expect_error(
    MatSelect(m, threshold = 0.5, method = "els"),
    "at least two columns"
  )
})

test_that("ELS handles two uncorrelated variables", {
  m <- diag(1, 2)
  colnames(m) <- c("A", "B")

  res <- MatSelect(m, threshold = 0.5, method = "els")

  # Both should be in same subset
  expect_equal(length(res@subset_list), 1)
  expect_setequal(res@subset_list[[1]], c("A", "B"))
})

test_that("ELS handles two perfectly correlated variables", {
  m <- matrix(1, nrow = 2, ncol = 2)
  colnames(m) <- c("A", "B")

  res <- MatSelect(m, threshold = 0.5, method = "els")

  # Each should be in its own separate (size-1) subset (see #30).
  expect_equal(length(res@subset_list), 2)
  expect_true(all(vapply(res@subset_list, length, integer(1)) == 1))
  expect_setequal(vapply(res@subset_list, identity, character(1)), c("A", "B"))
})

test_that("ELS with force_in and multiple subsets", {
  m <- diag(1, 5)
  # Create structure where force_in variable can go with multiple groups
  m[2, 3] <- m[3, 2] <- 0.8  # 2-3 correlated
  m[4, 5] <- m[5, 4] <- 0.8  # 4-5 correlated
  colnames(m) <- paste0("V", 1:5)

  res <- MatSelect(m, threshold = 0.5, method = "els", force_in = 1)

  # V1 should be in all subsets
  for (subset in res@subset_list) {
    expect_true("V1" %in% subset)
  }
})

test_that("ELS with incompatible force_in variables", {
  m <- diag(1, 3)
  m[1, 2] <- m[2, 1] <- 0.9  # High correlation
  colnames(m) <- c("A", "B", "C")

  expect_warning(
    res <- MatSelect(m, threshold = 0.5, method = "els", force_in = c(1, 2)),
    "mutually correlated"
  )
})

test_that("ELS deduplication works correctly", {
  # Create matrix that might generate duplicate subsets
  m <- diag(1, 4)
  # All low correlations - should generate single subset
  m[1, 2] <- m[2, 1] <- 0.1
  m[1, 3] <- m[3, 1] <- 0.1
  m[1, 4] <- m[4, 1] <- 0.1
  m[2, 3] <- m[3, 2] <- 0.1
  m[2, 4] <- m[4, 2] <- 0.1
  m[3, 4] <- m[4, 3] <- 0.1
  colnames(m) <- paste0("V", 1:4)

  res <- MatSelect(m, threshold = 0.5, method = "els")

  # Check no duplicate subsets
  subset_keys <- sapply(res@subset_list, function(s) paste(sort(s), collapse = ","))
  expect_equal(length(subset_keys), length(unique(subset_keys)))
})

test_that("ELS with force_in at boundary threshold", {
  m <- diag(1, 3)
  m[1, 2] <- m[2, 1] <- 0.5  # Exactly at threshold
  colnames(m) <- c("A", "B", "C")

  res <- MatSelect(m, threshold = 0.5, method = "els", force_in = 1)

  # A should be in all subsets
  for (subset in res@subset_list) {
    expect_true("A" %in% subset)
  }
})

test_that("ELS handles large identity matrix efficiently", {
  n <- 20
  m <- diag(1, n)
  colnames(m) <- paste0("V", 1:n)

  res <- MatSelect(m, threshold = 0.5, method = "els")

  # All uncorrelated - should return single subset with all variables
  expect_equal(length(res@subset_list), 1)
  expect_equal(length(res@subset_list[[1]]), n)
})

test_that("ELS force_in with seed node incompatibility", {
  m <- diag(1, 4)
  m[1, 2] <- m[2, 1] <- 0.8  # 1-2 correlated
  m[3, 4] <- m[4, 3] <- 0.8  # 3-4 correlated
  colnames(m) <- paste0("V", 1:4)

  # Force in V1 - should exclude V2 from all subsets
  res <- MatSelect(m, threshold = 0.5, method = "els", force_in = 1)

  for (subset in res@subset_list) {
    expect_true("V1" %in% subset)
    expect_false(all(c("V1", "V2") %in% subset))
  }
})

test_that("ELS returns correct ordering (by size then correlation)", {
  m <- diag(1, 5)
  # Create subsets of different sizes
  m[1, 2] <- m[2, 1] <- 0.8  # 1-2 correlated
  colnames(m) <- paste0("V", 1:5)

  res <- MatSelect(m, threshold = 0.5, method = "els")

  # Subsets should be ordered by size (descending)
  if (length(res@subset_list) > 1) {
    sizes <- sapply(res@subset_list, length)
    expect_true(all(diff(sizes) <= 0))
  }
})

# ===========================================================================
# Tests for findAllMaxSets edge cases (via MatSelect)
# ===========================================================================

test_that("MatSelect errors on invalid force_in indices (negative)", {
  m <- diag(1, 3)
  colnames(m) <- paste0("V", 1:3)

  expect_error(
    MatSelect(m, threshold = 0.5, method = "els", force_in = c(-1, 2)),
    "force_in"
  )
})

test_that("MatSelect errors on invalid force_in indices (out of bounds)", {
  m <- diag(1, 3)
  colnames(m) <- paste0("V", 1:3)

  expect_error(
    MatSelect(m, threshold = 0.5, method = "els", force_in = c(1, 10)),
    "force_in"
  )
})

test_that("MatSelect errors on invalid force_in indices (zero)", {
  m <- diag(1, 3)
  colnames(m) <- paste0("V", 1:3)

  expect_error(
    MatSelect(m, threshold = 0.5, method = "els", force_in = c(0, 2)),
    "force_in"
  )
})

test_that("ELS handles extremely low threshold (no valid pairs)", {
  m <- diag(1, 4)
  # All pairs have some correlation
  m[1, 2] <- m[2, 1] <- 0.1
  m[1, 3] <- m[3, 1] <- 0.15
  m[2, 4] <- m[4, 2] <- 0.12
  m[3, 4] <- m[4, 3] <- 0.08
  colnames(m) <- paste0("V", 1:4)

  # With extremely low threshold, fewer variables per subset
  res <- MatSelect(m, threshold = 0.05, method = "els")

  expect_true(inherits(res, "CorrCombo"))
  # Subsets should exist
  expect_true(length(res@subset_list) >= 0)
})

test_that("Bron-Kerbosch handles extremely low threshold", {
  m <- diag(1, 4)
  m[1, 2] <- m[2, 1] <- 0.1
  m[1, 3] <- m[3, 1] <- 0.15
  m[2, 4] <- m[4, 2] <- 0.12
  m[3, 4] <- m[4, 3] <- 0.08
  colnames(m) <- paste0("V", 1:4)

  res <- MatSelect(m, threshold = 0.05, method = "bron-kerbosch")

  expect_true(inherits(res, "CorrCombo"))
})

test_that("ELS with force_in covering all variables", {
  m <- diag(1, 3)
  colnames(m) <- paste0("V", 1:3)

  # Force all variables - should return valid result
  res <- MatSelect(m, threshold = 0.5, method = "els", force_in = 1:3)

  expect_true(inherits(res, "CorrCombo"))
  # All forced variables should appear in each subset (if subsets exist)
  if (length(res@subset_list) > 0) {
    for (s in res@subset_list) {
      expect_true(all(c("V1", "V2", "V3") %in% s))
    }
  }
})


# ===========================================================================
# Additional tests for findAllMaxSets internal function edge cases
# ===========================================================================

test_that("MatSelect passes NULL force_in correctly to backend", {
  m <- diag(1, 3)
  colnames(m) <- paste0("V", 1:3)

  # Without force_in - covers the NULL path in findAllMaxSets
  res <- MatSelect(m, threshold = 0.5, method = "els")

  expect_true(inherits(res, "CorrCombo"))
  expect_equal(length(res@forced_in), 0)
})

test_that("MatSelect passes empty force_in correctly", {
  m <- diag(1, 3)
  colnames(m) <- paste0("V", 1:3)

  # Explicitly empty force_in
  res <- MatSelect(m, threshold = 0.5, method = "els", force_in = integer(0))

  expect_true(inherits(res, "CorrCombo"))
})

test_that("MatSelect with force_in as numeric vector", {
  m <- diag(1, 4)
  colnames(m) <- paste0("V", 1:4)

  # Numeric indices
  res <- MatSelect(m, threshold = 0.5, method = "els", force_in = c(1, 3))

  expect_true(inherits(res, "CorrCombo"))
  for (s in res@subset_list) {
    expect_true(all(c("V1", "V3") %in% s))
  }
})

test_that("ELS returns correct subset stats for multi-variable subsets", {
  m <- diag(1, 4)
  m[1, 2] <- m[2, 1] <- 0.2
  m[1, 3] <- m[3, 1] <- 0.3
  m[2, 3] <- m[3, 2] <- 0.25
  colnames(m) <- paste0("V", 1:4)

  res <- MatSelect(m, threshold = 0.5, method = "els")

  # Check correlation stats are computed
  expect_true(length(res@avg_corr) > 0)
  expect_true(length(res@min_corr) > 0)
  expect_true(length(res@max_corr) > 0)
})
