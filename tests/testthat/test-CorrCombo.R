# tests/testthat/test-CorrCombo.R

test_that("CorrCombo slots are correctly set", {
  combo <- new("CorrCombo",
               subset_list = list(c("A", "B"), c("C", "D")),
               avg_corr = c(0.1, 0.2),
               min_corr = c(0.05, 0.1),
               max_corr = c(0.2, 0.3),
               names = c("A", "B", "C", "D"),
               threshold = 0.5,
               forced_in = "A",
               search_type = "els",
               n_rows_used = 100L)
  expect_s4_class(combo, "CorrCombo")
  expect_equal(length(combo@subset_list), 2)
  expect_equal(combo@search_type, "els")
})

test_that("show() method prints a summary for non-empty CorrCombo", {
  combo <- new("CorrCombo",
               subset_list = list(c("X1", "X2", "X3")),
               avg_corr = 0.1,
               min_corr = 0.05,
               max_corr = 0.2,
               names = paste0("X", 1:5),
               threshold = 0.4,
               forced_in = "X1",
               search_type = "els",
               n_rows_used = 100L)
  expect_output(show(combo), "CorrCombo object")
  expect_output(show(combo), "X1, X2, X3")
})

test_that("show() method handles empty CorrCombo", {
  combo <- new("CorrCombo",
               subset_list = list(),
               avg_corr = numeric(),
               min_corr = numeric(),
               max_corr = numeric(),
               names = letters[1:5],
               threshold = 0.4,
               forced_in = character(),
               search_type = "els",
               n_rows_used = 5L)
  expect_output(show(combo), "No valid subsets found")
})

test_that("show() prints use_pivot when set for bron-kerbosch", {
  combo <- new("CorrCombo",
               subset_list = list(c("X1", "X2")),
               avg_corr = 0.2,
               min_corr = 0.2,
               max_corr = 0.2,
               names = c("X1", "X2", "X3", "X4"),
               threshold = 0.5,
               forced_in = "X1",
               search_type = "bron-kerbosch",
               n_rows_used = 4L)
  attr(combo, "use_pivot") <- TRUE
  expect_output(show(combo), "bron-kerbosch")
})

test_that("as.data.frame() returns a padded data frame", {
  combo <- new("CorrCombo",
               subset_list = list(c("A", "B"), c("C", "D", "E")),
               avg_corr = c(0.1, 0.2),
               min_corr = c(0.1, 0.2),
               max_corr = c(0.1, 0.2),
               names = LETTERS[1:5],
               threshold = 0.5,
               forced_in = character(),
               search_type = "els",
               n_rows_used = 5L)
  df <- as.data.frame(combo)
  expect_equal(nrow(df), 2)
  expect_equal(ncol(df), 3)
  expect_named(df, c("VarName01", "VarName02", "VarName03"))
})

test_that("as.data.frame() returns empty data frame for empty CorrCombo", {
  combo <- new("CorrCombo",
               subset_list = list(),
               avg_corr = numeric(),
               min_corr = numeric(),
               max_corr = numeric(),
               names = character(),
               threshold = 0.5,
               forced_in = character(),
               search_type = "els",
               n_rows_used = 0L)
  df <- as.data.frame(combo)
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 0)
})

test_that("CorrCombo allows empty forced_in", {
  combo <- new("CorrCombo",
               subset_list = list(c("X1", "X2")),
               avg_corr = 0.3,
               min_corr = 0.3,
               max_corr = 0.3,
               names = c("X1", "X2"),
               threshold = 0.5,
               forced_in = character(),
               search_type = "els",
               n_rows_used = 2L)
  expect_equal(combo@forced_in, character())
})

test_that("as.data.frame handles variable names with special characters", {
  combo <- new("CorrCombo",
               subset_list = list(c("x_1", "x-2")),
               avg_corr = 0.2,
               min_corr = 0.2,
               max_corr = 0.2,
               names = c("x_1", "x-2", "x.3"),
               threshold = 0.5,
               forced_in = character(),
               search_type = "bron-kerbosch",
               n_rows_used = 3L)
  df <- as.data.frame(combo)
  expect_equal(df$VarName01[1], "x_1")
  expect_equal(df$VarName02[1], "x-2")
})

test_that("show() truncates long variable strings gracefully", {
  vars <- paste0("V", 1:20)
  combo <- new("CorrCombo",
               subset_list = list(vars),
               avg_corr = 0.5,
               min_corr = 0.1,
               max_corr = 0.9,
               names = vars,
               threshold = 0.5,
               forced_in = character(),
               search_type = "els",
               n_rows_used = 20L)
  expect_output(show(combo), "...")
})

test_that("CorrCombo construction fails with mismatched slot lengths", {
  expect_error(
    new("CorrCombo",
        subset_list = list(c("A", "B"), c("C", "D")),
        avg_corr = c(0.3),  # too short
        min_corr = c(0.1, 0.2),
        max_corr = c(0.5, 0.6),
        names = LETTERS[1:4],
        threshold = 0.5,
        forced_in = character(),
        search_type = "els",
        n_rows_used = 4L
    ),
    "avg_corr.*subset_list length"
  )
})

test_that("CorrCombo constructs successfully when slot lengths match", {
  obj <- new("CorrCombo",
             subset_list = list(c("A", "B")),
             avg_corr = 0.3,
             min_corr = 0.1,
             max_corr = 0.5,
             names = c("A", "B"),
             threshold = 0.5,
             forced_in = character(),
             search_type = "bron-kerbosch",
             n_rows_used = 2L)
  expect_s4_class(obj, "CorrCombo")
})

test_that("conversion to data.frame retains row names", {
  combo <- new("CorrCombo",
               subset_list = list(c("X1", "X2"), c("X3")),
               avg_corr = c(0.2, 0.3),
               min_corr = c(0.1, 0.2),
               max_corr = c(0.5, 0.6),
               names = c("X1", "X2", "X3"),
               threshold = 0.4,
               forced_in = character(),
               search_type = "els",
               n_rows_used = 3L)
  df <- as.data.frame(combo, row.names = c("Subset_1", "Subset_2"))
  expect_equal(rownames(df), c("Subset_1", "Subset_2"))
})

# ===========================================================================
# Additional coverage tests for CorrCombo.R
# ===========================================================================

test_that("show() method displays assoc_methods_used when set", {
  combo <- new("CorrCombo",
               subset_list = list(c("X1", "X2")),
               avg_corr = 0.15,
               min_corr = 0.10,
               max_corr = 0.20,
               names = c("X1", "X2", "X3"),
               threshold = 0.5,
               forced_in = character(),
               search_type = "els",
               cor_method = "mixed",
               n_rows_used = 10L)

  # Add assoc_methods_used attribute
  attr(combo, "assoc_methods_used") <- list(
    numeric_numeric = "pearson",
    numeric_factor = "eta",
    factor_factor = "cramersv"
  )

  # Should display AssocMethod line
  expect_output(show(combo), "AssocMethod")
  expect_output(show(combo), "pearson")
})

test_that("show() method wraps long assoc_methods lines", {
  combo <- new("CorrCombo",
               subset_list = list(c("X1", "X2")),
               avg_corr = 0.15,
               min_corr = 0.10,
               max_corr = 0.20,
               names = c("X1", "X2", "X3"),
               threshold = 0.5,
               forced_in = character(),
               search_type = "els",
               cor_method = "mixed",
               n_rows_used = 10L)

  # Add many assoc_methods to force wrapping
  attr(combo, "assoc_methods_used") <- list(
    numeric_numeric = "pearson",
    numeric_ordered = "spearman",
    ordered_numeric = "spearman",
    numeric_factor = "eta",
    factor_numeric = "eta",
    ordered_ordered = "spearman",
    ordered_factor = "cramersv",
    factor_ordered = "cramersv",
    factor_factor = "cramersv"
  )

  # Should still display without error
  expect_output(show(combo), "AssocMethod")
})

test_that("show() displays more than 5 combinations message", {
  # Create combo with > 5 subsets
  subsets <- lapply(1:10, function(i) paste0("V", c(i, i + 10)))
  avg_corrs <- runif(10, 0.1, 0.3)

  combo <- new("CorrCombo",
               subset_list = subsets,
               avg_corr = avg_corrs,
               min_corr = avg_corrs - 0.05,
               max_corr = avg_corrs + 0.05,
               names = paste0("V", 1:20),
               threshold = 0.5,
               forced_in = character(),
               search_type = "bron-kerbosch",
               n_rows_used = 20L)

  # Should show "... (X more combinations)"
  expect_output(show(combo), "more combinations")
})

test_that("show() displays forced_in variables", {
  combo <- new("CorrCombo",
               subset_list = list(c("X1", "X2", "X3")),
               avg_corr = 0.2,
               min_corr = 0.1,
               max_corr = 0.3,
               names = c("X1", "X2", "X3"),
               threshold = 0.5,
               forced_in = c("X1", "X2"),
               search_type = "els",
               n_rows_used = 10L)

  expect_output(show(combo), "Forced-in")
  expect_output(show(combo), "X1")
})

test_that("show() displays use_pivot attribute for bron-kerbosch", {
  combo <- new("CorrCombo",
               subset_list = list(c("X1", "X2")),
               avg_corr = 0.2,
               min_corr = 0.2,
               max_corr = 0.2,
               names = c("X1", "X2"),
               threshold = 0.5,
               forced_in = character(),
               search_type = "bron-kerbosch",
               n_rows_used = 5L)

  attr(combo, "use_pivot") <- FALSE

  expect_output(show(combo), "Pivot")
  expect_output(show(combo), "FALSE")
})

test_that("CorrCombo validity check for min_corr length mismatch", {
  expect_error(
    new("CorrCombo",
        subset_list = list(c("A", "B"), c("C", "D")),
        avg_corr = c(0.3, 0.4),
        min_corr = c(0.1),  # Length mismatch
        max_corr = c(0.5, 0.6),
        names = LETTERS[1:4],
        threshold = 0.5,
        forced_in = character(),
        search_type = "els",
        n_rows_used = 4L),
    "min_corr.*subset_list length"
  )
})

test_that("CorrCombo validity check for max_corr length mismatch", {
  expect_error(
    new("CorrCombo",
        subset_list = list(c("A", "B"), c("C", "D")),
        avg_corr = c(0.3, 0.4),
        min_corr = c(0.1, 0.2),
        max_corr = c(0.5),  # Length mismatch
        names = LETTERS[1:4],
        threshold = 0.5,
        forced_in = character(),
        search_type = "els",
        n_rows_used = 4L),
    "max_corr.*subset_list length"
  )
})

test_that("CorrCombo allows empty numeric vectors for stats when subset_list empty", {
  # Valid case: empty subset_list with empty stats vectors
  combo <- new("CorrCombo",
               subset_list = list(),
               avg_corr = numeric(0),
               min_corr = numeric(0),
               max_corr = numeric(0),
               names = c("A", "B"),
               threshold = 0.5,
               forced_in = character(),
               search_type = "els",
               n_rows_used = 2L)

  expect_s4_class(combo, "CorrCombo")
  expect_equal(length(combo@subset_list), 0)
})

test_that("as.data.frame handles two-element subsets", {
  # Test with subsets that have 2 elements each (avoids edge case with single-element)
  combo <- new("CorrCombo",
               subset_list = list(c("X1", "X2"), c("X3", "X4")),
               avg_corr = c(0.1, 0.2),
               min_corr = c(0.1, 0.2),
               max_corr = c(0.1, 0.2),
               names = c("X1", "X2", "X3", "X4"),
               threshold = 0.5,
               forced_in = character(),
               search_type = "els",
               n_rows_used = 4L)

  df <- as.data.frame(combo)
  expect_equal(nrow(df), 2)
  expect_equal(ncol(df), 2)
})

test_that("as.data.frame handles subsets with varying lengths", {
  combo <- new("CorrCombo",
               subset_list = list(c("A"), c("B", "C"), c("D", "E", "F")),
               avg_corr = c(0, 0.1, 0.2),
               min_corr = c(0, 0.05, 0.1),
               max_corr = c(0, 0.15, 0.3),
               names = LETTERS[1:6],
               threshold = 0.5,
               forced_in = character(),
               search_type = "els",
               n_rows_used = 6L)

  df <- as.data.frame(combo)

  # Should have 3 rows, 3 columns (max length of subsets)
  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 3)

  # First row should have NA padding
  expect_equal(df$VarName01[1], "A")
  expect_true(is.na(df$VarName02[1]))
  expect_true(is.na(df$VarName03[1]))
})

test_that("show() displays variable names with truncation correctly", {
  # Create subset with exactly at the truncation boundary
  vars <- paste0("V", 1:7)  # More than max_vars_display (6)
  combo <- new("CorrCombo",
               subset_list = list(vars),
               avg_corr = 0.3,
               min_corr = 0.1,
               max_corr = 0.5,
               names = vars,
               threshold = 0.5,
               forced_in = character(),
               search_type = "els",
               n_rows_used = 10L)

  expect_output(show(combo), "\\.\\.\\.")
})

test_that("show() handles cor_method display", {
  combo <- new("CorrCombo",
               subset_list = list(c("A", "B")),
               avg_corr = 0.2,
               min_corr = 0.1,
               max_corr = 0.3,
               names = c("A", "B"),
               threshold = 0.5,
               forced_in = character(),
               search_type = "els",
               cor_method = "spearman",
               n_rows_used = 10L)

  expect_output(show(combo), "Correlation")
  expect_output(show(combo), "spearman")
})
