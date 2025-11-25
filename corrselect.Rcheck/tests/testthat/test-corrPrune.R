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
