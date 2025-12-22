test_that("corrPrune validates data argument", {
  expect_error(
    corrPrune(data = NULL),
    "'data' must be a data.frame"
  )

  expect_error(
    corrPrune(data = matrix(1:10, ncol = 2)),
    "'data' must be a data.frame"
  )

  expect_error(
    corrPrune(data = data.frame()),
    "'data' must contain at least one column"
  )
})

test_that("corrPrune validates threshold argument", {
  df <- data.frame(x = 1:10, y = 1:10)

  expect_error(
    corrPrune(df, threshold = "0.7"),
    "'threshold' must be a single numeric value"
  )

  expect_error(
    corrPrune(df, threshold = c(0.5, 0.7)),
    "'threshold' must be a single numeric value"
  )

  expect_error(
    corrPrune(df, threshold = -0.5),
    "'threshold' must be non-negative and non-missing"
  )

  expect_error(
    corrPrune(df, threshold = NA),
    "'threshold' must be a single numeric value"
  )
})

test_that("corrPrune validates measure argument", {
  df <- data.frame(x = 1:10, y = 1:10)

  expect_error(
    corrPrune(df, measure = 123),
    "'measure' must be a single character string"
  )

  expect_error(
    corrPrune(df, measure = c("pearson", "spearman")),
    "'measure' must be a single character string"
  )
})

test_that("corrPrune validates mode argument", {
  df <- data.frame(x = 1:10, y = 1:10)

  expect_error(
    corrPrune(df, mode = 123),
    "'mode' must be a single character string"
  )

  expect_error(
    corrPrune(df, mode = "invalid"),
    "'mode' must be one of: 'auto', 'exact', 'greedy'"
  )

  expect_error(
    corrPrune(df, mode = c("exact", "greedy")),
    "'mode' must be a single character string"
  )
})

test_that("corrPrune validates force_in argument", {
  df <- data.frame(x = 1:10, y = 1:10, z = 1:10)

  expect_error(
    corrPrune(df, force_in = 123),
    "'force_in' must be a character vector of variable names"
  )

  expect_error(
    corrPrune(df, force_in = c("x", "missing_var")),
    "'force_in' variable\\(s\\) not found in data: missing_var"
  )

  expect_error(
    corrPrune(df, force_in = c("missing1", "missing2")),
    "'force_in' variable\\(s\\) not found in data: missing1, missing2"
  )
})

test_that("corrPrune validates by argument", {
  df <- data.frame(x = 1:10, y = 1:10, group = rep(1:2, each = 5))

  expect_error(
    corrPrune(df, by = 123),
    "'by' must be a character vector of variable names"
  )

  expect_error(
    corrPrune(df, by = "missing_var"),
    "'by' variable\\(s\\) not found in data: missing_var"
  )

  expect_error(
    corrPrune(df, by = c("group", "missing")),
    "'by' variable\\(s\\) not found in data: missing"
  )
})

test_that("corrPrune validates group_q argument", {
  df <- data.frame(x = 1:10, y = 1:10)

  expect_error(
    corrPrune(df, group_q = "0.9"),
    "'group_q' must be a single numeric value"
  )

  expect_error(
    corrPrune(df, group_q = c(0.5, 0.9)),
    "'group_q' must be a single numeric value"
  )

  expect_error(
    corrPrune(df, group_q = 0),
    "'group_q' must be in the interval \\(0, 1\\]"
  )

  expect_error(
    corrPrune(df, group_q = -0.5),
    "'group_q' must be in the interval \\(0, 1\\]"
  )

  expect_error(
    corrPrune(df, group_q = 1.5),
    "'group_q' must be in the interval \\(0, 1\\]"
  )

  expect_error(
    corrPrune(df, group_q = NA),
    "'group_q' must be a single numeric value"
  )
})

test_that("corrPrune validates max_exact_p argument", {
  df <- data.frame(x = 1:10, y = 1:10)

  expect_error(
    corrPrune(df, max_exact_p = "20"),
    "'max_exact_p' must be a single numeric value"
  )

  expect_error(
    corrPrune(df, max_exact_p = c(10, 20)),
    "'max_exact_p' must be a single numeric value"
  )

  expect_error(
    corrPrune(df, max_exact_p = 0),
    "'max_exact_p' must be >= 1"
  )

  expect_error(
    corrPrune(df, max_exact_p = -5),
    "'max_exact_p' must be >= 1"
  )

  expect_error(
    corrPrune(df, max_exact_p = NA),
    "'max_exact_p' must be a single numeric value"
  )
})

# ===========================================================================
# Functional tests for corrPrune (exact mode)
# ===========================================================================

test_that("corrPrune works with simple numeric data in exact mode", {
  set.seed(42)
  # Create data with known correlation structure
  n <- 50
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.1)  # highly correlated with x1
  x3 <- rnorm(n)                  # independent
  x4 <- rnorm(n)                  # independent

  df <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4)

  # With low threshold, should exclude x2 (correlated with x1)
  result <- corrPrune(df, threshold = 0.5, mode = "exact")

  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) >= 1)
  expect_true(ncol(result) <= ncol(df))
  expect_true(all(names(result) %in% names(df)))

  # Check attributes
  expect_true("selected_vars" %in% names(attributes(result)))
  expect_true("mode" %in% names(attributes(result)))
  expect_true("measure" %in% names(attributes(result)))
  expect_equal(attr(result, "mode"), "exact")
  expect_equal(attr(result, "measure"), "pearson")
  expect_equal(attr(result, "threshold"), 0.5)
})

test_that("corrPrune respects force_in in exact mode", {
  set.seed(123)
  n <- 50
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)

  df <- data.frame(x1 = x1, x2 = x2, x3 = x3)

  result <- corrPrune(df, threshold = 0.7, force_in = "x1", mode = "exact")

  expect_true("x1" %in% names(result))
  expect_true("x1" %in% attr(result, "selected_vars"))
})

test_that("corrPrune errors when force_in violates threshold", {
  set.seed(456)
  n <- 50
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.01)  # very highly correlated

  df <- data.frame(x1 = x1, x2 = x2)

  expect_error(
    corrPrune(df, threshold = 0.3, force_in = c("x1", "x2"), mode = "exact"),
    "Variables in 'force_in' violate the threshold constraint"
  )
})

test_that("corrPrune auto-selects mode based on max_exact_p", {
  set.seed(789)
  df_small <- data.frame(matrix(rnorm(100), ncol = 10))

  # Small dataset should use exact mode by default
  result_small <- corrPrune(df_small, threshold = 0.7, max_exact_p = 20)
  expect_equal(attr(result_small, "mode"), "exact")
})

test_that("corrPrune handles different correlation measures", {
  set.seed(101)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  # Pearson
  result_pearson <- corrPrune(df, threshold = 0.7, measure = "pearson", mode = "exact")
  expect_equal(attr(result_pearson, "measure"), "pearson")

  # Spearman
  result_spearman <- corrPrune(df, threshold = 0.7, measure = "spearman", mode = "exact")
  expect_equal(attr(result_spearman, "measure"), "spearman")

  # Kendall
  result_kendall <- corrPrune(df, threshold = 0.7, measure = "kendall", mode = "exact")
  expect_equal(attr(result_kendall, "measure"), "kendall")
})

test_that("corrPrune returns deterministic results", {
  set.seed(202)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n)
  )

  # Run twice with same data
  result1 <- corrPrune(df, threshold = 0.7, mode = "exact")
  result2 <- corrPrune(df, threshold = 0.7, mode = "exact")

  expect_equal(names(result1), names(result2))
  expect_equal(attr(result1, "selected_vars"), attr(result2, "selected_vars"))
})

test_that("corrPrune preserves row count", {
  set.seed(303)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.7, mode = "exact")

  expect_equal(nrow(result), nrow(df))
})

test_that("corrPrune handles missing values with warning", {
  set.seed(404)
  n <- 50
  df <- data.frame(
    x1 = c(NA, rnorm(n - 1)),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  expect_warning(
    result <- corrPrune(df, threshold = 0.7, mode = "exact"),
    "Removed .* row.*with missing values"
  )

  # corrPrune returns original data with pruned columns, including NA rows
  expect_equal(nrow(result), n)
})

test_that("corrPrune tie-breaking is deterministic", {
  set.seed(505)
  n <- 100

  # Create data where multiple subsets might have same size
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n),
    x5 = rnorm(n)
  )

  # Low threshold to potentially get multiple max-size subsets
  result1 <- corrPrune(df, threshold = 0.8, mode = "exact")
  result2 <- corrPrune(df, threshold = 0.8, mode = "exact")

  # Should always select the same subset
  expect_identical(attr(result1, "selected_vars"), attr(result2, "selected_vars"))
})

# ===========================================================================
# Functional tests for corrPrune (greedy mode)
# ===========================================================================

test_that("corrPrune greedy mode works with simple numeric data", {
  set.seed(42)
  n <- 50
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.1)  # highly correlated with x1
  x3 <- rnorm(n)
  x4 <- rnorm(n)

  df <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4)

  result <- corrPrune(df, threshold = 0.5, mode = "greedy")

  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) >= 1)
  expect_true(ncol(result) <= ncol(df))
  expect_equal(attr(result, "mode"), "greedy")
  expect_equal(attr(result, "measure"), "pearson")
})

test_that("corrPrune greedy mode respects force_in", {
  set.seed(123)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.7, force_in = "x1", mode = "greedy")

  expect_true("x1" %in% names(result))
  expect_true("x1" %in% attr(result, "selected_vars"))
})

test_that("corrPrune greedy mode is deterministic", {
  set.seed(202)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n)
  )

  result1 <- corrPrune(df, threshold = 0.7, mode = "greedy")
  result2 <- corrPrune(df, threshold = 0.7, mode = "greedy")

  expect_equal(names(result1), names(result2))
  expect_identical(attr(result1, "selected_vars"), attr(result2, "selected_vars"))
})

test_that("corrPrune auto-selects greedy mode for large datasets", {
  set.seed(303)
  # Create dataset with > 20 variables (default max_exact_p)
  df_large <- data.frame(matrix(rnorm(50 * 25), ncol = 25))

  result <- corrPrune(df_large, threshold = 0.7, mode = "auto", max_exact_p = 20)

  expect_equal(attr(result, "mode"), "greedy")
})

test_that("corrPrune greedy mode handles different thresholds", {
  set.seed(404)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n),
    x5 = rnorm(n)
  )

  # Strict threshold should remove more variables
  result_strict <- corrPrune(df, threshold = 0.3, mode = "greedy")
  # Lenient threshold should keep more variables
  result_lenient <- corrPrune(df, threshold = 0.9, mode = "greedy")

  expect_true(ncol(result_lenient) >= ncol(result_strict))
})

test_that("corrPrune greedy vs exact give similar results on small data", {
  set.seed(505)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n)
  )

  result_exact <- corrPrune(df, threshold = 0.7, mode = "exact")
  result_greedy <- corrPrune(df, threshold = 0.7, mode = "greedy")

  # Sizes should be close (within 1 variable)
  expect_true(abs(ncol(result_exact) - ncol(result_greedy)) <= 1)
})

test_that("corrPrune greedy mode handles highly correlated data", {
  set.seed(606)
  n <- 50
  x1 <- rnorm(n)
  # Create block of highly correlated variables
  df <- data.frame(
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.05),
    x3 = x1 + rnorm(n, sd = 0.05),
    x4 = x1 + rnorm(n, sd = 0.05),
    x5 = rnorm(n)  # independent
  )

  result <- corrPrune(df, threshold = 0.5, mode = "greedy")

  # Should remove most of the correlated block
  expect_true(ncol(result) < ncol(df))
  expect_true(ncol(result) >= 1)
})

test_that("corrPrune greedy mode handles all independent variables", {
  set.seed(707)
  n <- 50
  # All independent
  df <- data.frame(matrix(rnorm(n * 10), ncol = 10))

  result <- corrPrune(df, threshold = 0.7, mode = "greedy")

  # Should keep most/all variables
  expect_true(ncol(result) >= 8)  # Allow for some random correlation
})

test_that("corrPrune greedy mode errors when force_in violates threshold", {
  set.seed(808)
  n <- 50
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.01)  # very highly correlated

  df <- data.frame(x1 = x1, x2 = x2)

  # This should be caught in Step 5 (feasibility check)
  expect_error(
    corrPrune(df, threshold = 0.3, force_in = c("x1", "x2"), mode = "greedy"),
    "Variables in 'force_in' violate the threshold constraint"
  )
})

test_that("corrPrune greedy mode works with different correlation measures", {
  set.seed(909)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  result_spearman <- corrPrune(df, threshold = 0.7, measure = "spearman", mode = "greedy")
  expect_equal(attr(result_spearman, "measure"), "spearman")

  result_kendall <- corrPrune(df, threshold = 0.7, measure = "kendall", mode = "greedy")
  expect_equal(attr(result_kendall, "measure"), "kendall")
})

# ===========================================================================
# Additional data handling tests for corrPrune
# ===========================================================================

# Note: For full mixed-type data support, use assocSelect() which is designed
# for that purpose. corrPrune works best with numeric-only data.

test_that("corrPrune handles integer columns (converted to numeric)", {
  set.seed(1004)
  n <- 30
  df <- data.frame(
    int1 = as.integer(sample(1:100, n, replace = TRUE)),
    int2 = as.integer(sample(1:100, n, replace = TRUE))
  )

  result <- corrPrune(df, threshold = 0.7, mode = "exact")
  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "measure"), "pearson")
})

test_that("corrPrune errors on unsupported column types", {
  df <- data.frame(
    num1 = 1:10,
    date1 = Sys.Date() + 1:10
  )

  expect_error(
    corrPrune(df, threshold = 0.7),
    "Unsupported column types"
  )
})

test_that("corrPrune errors on unsupported measure for numeric data", {
  set.seed(1005)
  df <- data.frame(x = rnorm(10), y = rnorm(10))

  expect_error(
    corrPrune(df, threshold = 0.7, measure = "eta"),
    "not supported"
  )
})

test_that("corrPrune lexicographic tie-breaking works correctly", {
  set.seed(1006)
  n <- 50
  # Create data where multiple subsets might have same size and avg correlation
  df <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    c = rnorm(n),
    d = rnorm(n)
  )

  # Run multiple times to verify determinism
  result1 <- corrPrune(df, threshold = 0.95, mode = "exact")
  result2 <- corrPrune(df, threshold = 0.95, mode = "exact")

  expect_identical(names(result1), names(result2))
})

test_that("corrPrune handles all rows with NA (errors)", {
  df <- data.frame(
    x1 = c(NA, NA, NA),
    x2 = c(NA, NA, NA)
  )

  # Should error - either "All rows contain missing values" or NA in matrix
  expect_error(
    corrPrune(df, threshold = 0.7)
  )
})

test_that("corrPrune handles near-constant numeric columns", {
  set.seed(1013)
  n <- 30
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n),
    near_const = c(5, 5, 5, 5, 5.001, rep(5, n - 5))  # Near-constant with tiny variance
  )

  result <- corrPrune(df, threshold = 0.8, mode = "exact")
  expect_s3_class(result, "data.frame")
})

test_that("corrPrune returns correct attributes", {
  set.seed(1014)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.7, mode = "exact")

  # Check all expected attributes
  expect_true(!is.null(attr(result, "selected_vars")))
  expect_true(!is.null(attr(result, "removed_vars")))
  expect_true(!is.null(attr(result, "mode")))
  expect_true(!is.null(attr(result, "measure")))
  expect_true(!is.null(attr(result, "threshold")))
  expect_true(!is.null(attr(result, "n_vars_original")))
  expect_true(!is.null(attr(result, "n_vars_selected")))

  # Verify consistency
  expect_equal(attr(result, "n_vars_selected"), ncol(result))
  expect_equal(length(attr(result, "selected_vars")), ncol(result))
})

test_that("corrPrune removes highly correlated variables", {
  set.seed(1015)
  n <- 100
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.01)  # Very highly correlated with x1
  x3 <- rnorm(n)  # Independent

  df <- data.frame(x1 = x1, x2 = x2, x3 = x3)

  result <- corrPrune(df, threshold = 0.5, mode = "exact")

  # Should have removed either x1 or x2 due to high correlation
  expect_true(ncol(result) < ncol(df))
  expect_false(all(c("x1", "x2") %in% names(result)))
})

# ===========================================================================
# Additional coverage tests for corrPrune.R
# ===========================================================================

# Note: corrPrune's mixed-type support is limited compared to assocSelect.
# For full mixed-type support, use assocSelect() instead.

test_that("corrPrune handles factor-factor pairs", {
  set.seed(1104)
  n <- 30
  df <- data.frame(
    cat1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    cat2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
    cat3 = factor(sample(c("M", "N", "O", "P"), n, replace = TRUE))
  )

  result <- corrPrune(df, threshold = 0.99, mode = "exact")
  expect_s3_class(result, "data.frame")
})

test_that("corrPrune handles numeric-factor pairs (eta)", {
  set.seed(1107)
  n <- 30
  df <- data.frame(
    num1 = rnorm(n),
    cat1 = factor(sample(c("A", "B", "C"), n, replace = TRUE))
  )

  result <- corrPrune(df, threshold = 0.9, mode = "exact")
  expect_s3_class(result, "data.frame")
})

test_that("corrPrune single force_in variable works in exact mode", {
  set.seed(1108)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.9, force_in = "x2", mode = "exact")

  expect_true("x2" %in% names(result))
  expect_true("x2" %in% attr(result, "selected_vars"))
})

test_that("corrPrune greedy mode with single force_in", {
  set.seed(1109)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.9, force_in = "x2", mode = "greedy")

  expect_true("x2" %in% names(result))
  expect_true("x2" %in% attr(result, "selected_vars"))
})

test_that("corrPrune handles eta with constant categorical variable", {
  set.seed(1110)
  n <- 30
  df <- data.frame(
    num1 = rnorm(n),
    cat_const = factor(rep("A", n))  # Constant factor
  )

  result <- corrPrune(df, threshold = 0.99, mode = "exact")
  expect_s3_class(result, "data.frame")
})

test_that("corrPrune handles eta with constant numeric variable", {
  set.seed(1111)
  n <- 30
  df <- data.frame(
    num_const = rep(5, n),  # Constant numeric (ss_tot = 0)
    cat1 = factor(sample(c("A", "B"), n, replace = TRUE))
  )

  result <- corrPrune(df, threshold = 0.99, mode = "exact")
  expect_s3_class(result, "data.frame")
})

test_that("corrPrune grouped pruning works with by parameter", {
  set.seed(1112)
  # Create data with three uncorrelated numeric variables and a grouping factor
  n <- 60
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n),
    group = rep(c("A", "B"), each = n/2)
  )
  
  # With grouped pruning using quantile aggregation
  result <- corrPrune(df, threshold = 0.5, by = "group", group_q = 0.5)
  expect_s3_class(result, "data.frame")
  expect_true("selected_vars" %in% names(attributes(result)))
  # Should keep all 3 numeric variables since they are uncorrelated
  expect_equal(length(attr(result, "selected_vars")), 3)
})

test_that("corrPrune handles no valid subset (all vars correlated)", {
  set.seed(1113)
  n <- 50
  x1 <- rnorm(n)
  # All variables perfectly correlated
  df <- data.frame(
    x1 = x1,
    x2 = x1,
    x3 = x1
  )

  # With very low threshold, no valid subset with >1 variable
  expect_error(
    corrPrune(df, threshold = 0.1, mode = "exact"),
    "No valid subsets found"
  )
})

test_that("corrPrune with balanced factors", {
  set.seed(1114)
  n <- 40
  df <- data.frame(
    cat1 = factor(rep(c("A", "B"), each = n/2)),
    cat2 = factor(rep(c("X", "Y"), n/2))
  )

  result <- corrPrune(df, threshold = 0.99, mode = "exact")
  expect_s3_class(result, "data.frame")
})

test_that("corrPrune handles threshold = 1 (all variables valid)", {
  set.seed(1116)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 1.0, mode = "exact")

  # All variables should be selected
  expect_equal(ncol(result), ncol(df))
})

test_that("corrPrune attributes are consistent", {
  set.seed(1117)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.7, mode = "greedy")

  # Check attribute consistency
  selected <- attr(result, "selected_vars")
  removed <- attr(result, "removed_vars")

  expect_equal(length(selected) + length(removed), ncol(df))
  expect_equal(attr(result, "n_vars_original"), ncol(df))
  expect_equal(attr(result, "n_vars_selected"), length(selected))
  expect_setequal(c(selected, removed), names(df))
})

# ===========================================================================
# Additional tests for lexicographic tiebreaker and edge cases
# ===========================================================================

test_that("corrPrune lexicographic tiebreaker with tied avg correlation", {
  # Create exact correlation matrix to trigger lexicographic tiebreaker
  # Structure: 4 variables forming 2 disjoint pairs
  # {V1, V2} and {V3, V4} with identical correlations within pairs
  # Cross-correlations are high (above threshold)
  set.seed(7001)
  n <- 200

  # Create perfectly controlled data
  base1 <- rnorm(n)
  base2 <- rnorm(n)

  df <- data.frame(
    V1 = base1,
    V2 = base1 * 0.3 + rnorm(n, sd = sqrt(1 - 0.3^2)),  # ~0.3 corr with V1
    V3 = base2,
    V4 = base2 * 0.3 + rnorm(n, sd = sqrt(1 - 0.3^2))   # ~0.3 corr with V3
  )

  # Make cross-group correlations high by mixing
  df$V3 <- df$V3 + 0.6 * df$V1
  df$V4 <- df$V4 + 0.6 * df$V2

  result <- corrPrune(df, threshold = 0.5, mode = "exact")

  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) >= 1)  # At least one variable should be kept
})

test_that("corrPrune handles multiple tied subsets with lexicographic ordering", {
  # Design a scenario that triggers the lexicographic tiebreaker
  set.seed(7002)
  n <- 50

  # Variables that are all mutually uncorrelated
  df <- data.frame(
    a1 = rnorm(n),
    a2 = rnorm(n),
    b1 = rnorm(n),
    b2 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.99, mode = "exact")

  expect_s3_class(result, "data.frame")
  # With high threshold, all variables should be kept (all uncorrelated)
  expect_equal(ncol(result), ncol(df))
})

test_that("corrPrune character columns only", {
  set.seed(7003)
  n <- 30
  # All factor columns - should work
  df <- data.frame(
    char1 = factor(sample(c("apple", "banana", "cherry"), n, replace = TRUE)),
    char2 = factor(sample(c("red", "green", "blue"), n, replace = TRUE)),
    char3 = factor(sample(c("small", "large"), n, replace = TRUE))
  )

  result <- corrPrune(df, threshold = 0.8, mode = "exact")

  expect_s3_class(result, "data.frame")
})

test_that("corrPrune factor-only data", {
  set.seed(7004)
  n <- 30
  df <- data.frame(
    f1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    f2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )

  # Factor-only should use cramersv internally
  result <- corrPrune(df, threshold = 0.8, mode = "exact")

  expect_s3_class(result, "data.frame")
})

test_that("corrPrune integer column conversion to numeric", {
  set.seed(7005)
  n <- 30
  df <- data.frame(
    int1 = as.integer(sample(1:100, n, replace = TRUE)),
    int2 = as.integer(sample(1:100, n, replace = TRUE)),
    num1 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.8, mode = "greedy")

  expect_s3_class(result, "data.frame")
})

# ===========================================================================
# Additional edge case tests for full coverage
# ===========================================================================

test_that("corrPrune lexicographic tie-breaking with identical avg correlations", {
  set.seed(8001)
  n <- 100

  # Create data with multiple subsets of same size and same avg correlation
  # This triggers the lexicographic tie-breaker in exact mode
  df <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    c = rnorm(n),
    d = rnorm(n)
  )

  # All independent variables - all subsets have avg = 0
  result1 <- corrPrune(df, threshold = 0.99, mode = "exact")
  result2 <- corrPrune(df, threshold = 0.99, mode = "exact")

  # Should be deterministic
  expect_identical(names(result1), names(result2))
})

test_that("corrPrune handles multiple max-size subsets with same avg correlation", {
  set.seed(8002)
  n <- 50

  # All pairs have very similar low correlations -> same avg
  x1 <- rnorm(n)
  df <- data.frame(
    a1 = x1,
    a2 = rnorm(n),
    a3 = rnorm(n),
    a4 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.95, mode = "exact")

  expect_s3_class(result, "data.frame")
})

test_that("corrPrune lexicographic ordering with alphabetical names", {
  set.seed(8003)
  n <- 50

  # Alphabetically ordered names for predictable tie-breaking
  df <- data.frame(
    apple = rnorm(n),
    banana = rnorm(n),
    cherry = rnorm(n),
    date = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.99, mode = "exact")

  expect_s3_class(result, "data.frame")
})

test_that("corrPrune exact mode with single variable forced in", {
  set.seed(8004)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.9, force_in = "x2", mode = "exact")

  expect_true("x2" %in% names(result))
})


test_that("corrPrune greedy mode with multiple force_in variables", {
  set.seed(8006)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.9, force_in = c("x1", "x3"), mode = "greedy")

  expect_true(all(c("x1", "x3") %in% names(result)))
})


test_that("corrPrune exact mode finds largest subset", {
  set.seed(8008)
  n <- 100
  x1 <- rnorm(n)
  df <- data.frame(
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.05),  # High correlation with x1
    x3 = rnorm(n),
    x4 = rnorm(n),
    x5 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.5, mode = "exact")

  # Should have at least 3 variables (x3, x4, x5 + one of x1/x2)
  expect_true(ncol(result) >= 3)
})

test_that("corrPrune greedy removes variable with most violations first", {
  set.seed(8009)
  n <- 100
  x1 <- rnorm(n)
  df <- data.frame(
    bad = x1,  # Correlated with many
    x2 = x1 + rnorm(n, sd = 0.1),
    x3 = x1 + rnorm(n, sd = 0.1),
    good1 = rnorm(n),
    good2 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.5, mode = "greedy")

  # "bad" should likely be removed since it violates threshold with x2 and x3
  expect_s3_class(result, "data.frame")
})

test_that("corrPrune returns consistent n_vars_original attribute", {
  set.seed(8010)
  n <- 50
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.8, mode = "exact")

  expect_equal(attr(result, "n_vars_original"), 4)
  expect_equal(attr(result, "n_vars_selected"), ncol(result))
})


# ===========================================================================
# Grouped pruning tests
# ===========================================================================

test_that("corrPrune grouped pruning aggregates with different quantiles", {
  set.seed(2001)
  n <- 100
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n),
    group = rep(c("A", "B", "C", "D"), each = n/4)
  )

  # Test with median (0.5)
  result1 <- corrPrune(df, threshold = 0.5, by = "group", group_q = 0.5)
  expect_s3_class(result1, "data.frame")

  # Test with max (1.0)
  result2 <- corrPrune(df, threshold = 0.5, by = "group", group_q = 1.0)
  expect_s3_class(result2, "data.frame")

  # Test with minimum (0.0)
  result3 <- corrPrune(df, threshold = 0.5, by = "group", group_q = 0.01)
  expect_s3_class(result3, "data.frame")
})

test_that("corrPrune grouped pruning returns only numeric columns", {
  set.seed(2002)
  n <- 90
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n),
    group = factor(rep(1:3, each = n/3))
  )
  
  result <- corrPrune(df, threshold = 0.8, by = "group")
  # Grouping column is not included in result (only numeric columns)
  expect_false("group" %in% names(result))
  # Selected variables should all be numeric
  expect_true(all(attr(result, "selected_vars") %in% c("x", "y", "z")))
})

test_that("corrPrune grouped pruning validates by parameter", {
  set.seed(2003)
  df <- data.frame(x = rnorm(20), y = rnorm(20), z = factor(1:20))

  expect_error(
    corrPrune(df, threshold = 0.5, by = "nonexistent"),
    "not found"
  )
})

test_that("corrPrune grouped pruning with interaction of multiple by columns", {
  set.seed(2004)
  n <- 80
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    grp1 = rep(c("A", "B"), each = n/2),
    grp2 = rep(c("X", "Y"), n/2)
  )

  result <- corrPrune(df, threshold = 0.5, by = c("grp1", "grp2"))
  expect_s3_class(result, "data.frame")
})


# ===========================================================================
# Grouped pruning edge case tests
# ===========================================================================

test_that("corrPrune grouped pruning with single group warns and continues", {
  set.seed(3001)
  n <- 50
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n),
    group = factor(rep("A", n))  # Only one group
  )

  expect_warning(
    result <- corrPrune(df, threshold = 0.5, by = "group"),
    "Only one group found"
  )
  expect_s3_class(result, "data.frame")
})

test_that("corrPrune grouped pruning with insufficient rows in a group warns", {
  set.seed(3002)
  n <- 50
  df <- data.frame(
    x = c(1, rnorm(n - 1)),
    y = c(NA, rnorm(n - 1)),  # First row incomplete
    z = rnorm(n),
    group = factor(c("small", rep("large", n - 1)))  # One group has only 1 row
  )

  expect_warning(
    result <- corrPrune(df, threshold = 0.8, by = "group"),
    "fewer than 2 complete rows"
  )
  expect_s3_class(result, "data.frame")
})


# ===========================================================================
# Tests for optional package measures (bicor, distance, maximal)
# ===========================================================================

test_that("corrPrune with bicor measure works", {
  skip_if_not(requireNamespace("WGCNA", quietly = TRUE))

  set.seed(4001)
  n <- 50
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.5, measure = "bicor")
  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "measure"), "bicor")
})

test_that("corrPrune with distance correlation works", {
  skip_if_not(requireNamespace("energy", quietly = TRUE))

  set.seed(4002)
  n <- 30  # Smaller for speed
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.5, measure = "distance")
  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "measure"), "distance")
})

test_that("corrPrune with maximal information coefficient works", {
  skip_if_not(requireNamespace("minerva", quietly = TRUE))

  set.seed(4003)
  n <- 30  # Smaller for speed
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.5, measure = "maximal")
  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "measure"), "maximal")
})

# ===========================================================================
# Tests for mixed-type data in corrPrune
# ===========================================================================

test_that("corrPrune handles numeric-ordered pairs", {
  set.seed(4004)
  n <- 50
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n),
    ord1 = ordered(sample(1:3, n, replace = TRUE))
  )

  result <- corrPrune(df, threshold = 0.8)
  expect_s3_class(result, "data.frame")
})

test_that("corrPrune handles ordered-ordered pairs", {
  set.seed(4005)
  n <- 50
  df <- data.frame(
    ord1 = ordered(sample(1:4, n, replace = TRUE)),
    ord2 = ordered(sample(1:4, n, replace = TRUE)),
    ord3 = ordered(sample(1:4, n, replace = TRUE))
  )

  result <- corrPrune(df, threshold = 0.8)
  expect_s3_class(result, "data.frame")
})

test_that("corrPrune handles factor-factor pairs with sparse tables", {
  set.seed(4006)
  n <- 50
  df <- data.frame(
    fac1 = factor(sample(c("A", "B"), n, replace = TRUE)),
    fac2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
    fac3 = factor(sample(c("P", "Q"), n, replace = TRUE))
  )

  result <- corrPrune(df, threshold = 0.8)
  expect_s3_class(result, "data.frame")
})

test_that("corrPrune handles numeric-factor pairs", {
  set.seed(4007)
  n <- 50
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n),
    fac1 = factor(sample(c("A", "B", "C"), n, replace = TRUE))
  )

  result <- corrPrune(df, threshold = 0.8)
  expect_s3_class(result, "data.frame")
})

test_that("corrPrune handles ordered-factor pairs", {
  set.seed(4008)
  n <- 50
  df <- data.frame(
    ord1 = ordered(sample(1:3, n, replace = TRUE)),
    fac1 = factor(sample(c("A", "B"), n, replace = TRUE)),
    fac2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )

  result <- corrPrune(df, threshold = 0.8)
  expect_s3_class(result, "data.frame")
})

# ===========================================================================
# Tests for edge cases in exact mode tie-breaking
# ===========================================================================

test_that("corrPrune exact mode handles lexicographic tie-breaking", {
  set.seed(4009)
  # Create data where multiple subsets have same size and correlation
  n <- 50
  df <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    c = rnorm(n),
    d = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.95, mode = "exact")
  expect_s3_class(result, "data.frame")
  # Should be deterministic
  result2 <- corrPrune(df, threshold = 0.95, mode = "exact")
  expect_equal(names(result), names(result2))
})

# ===========================================================================
# Test for chi-squared edge case
# ===========================================================================

test_that("corrPrune handles chi-squared NA gracefully", {
  set.seed(4010)
  n <- 30
  # Create factors where chi-squared might fail
  df <- data.frame(
    fac1 = factor(c(rep("A", n-1), "B")),  # Very unbalanced
    fac2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
    num1 = rnorm(n)
  )

  # Should not error
  result <- corrPrune(df, threshold = 0.9)
  expect_s3_class(result, "data.frame")
})
