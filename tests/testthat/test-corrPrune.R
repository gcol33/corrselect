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

test_that("corrPrune handles single-column input under default mode = 'auto' (#34)", {
  # Regression test for issue #34: mode = "auto" resolved to "exact" for any
  # p <= max_exact_p, including p == 1, but exact mode routes through
  # MatSelect(), which refuses ncol < 2 -- so single-predictor input, which
  # corrPrune()'s own Step 7A logic treats as a trivially valid answer,
  # crashed with MatSelect()'s internal "mat must have at least two columns"
  # message. mode = "auto" now degrades to greedy for p < 2.
  set.seed(9340)
  df <- data.frame(x = rnorm(10))

  result <- corrPrune(df, threshold = 0.7)
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), "x")
  expect_equal(attr(result, "mode"), "greedy")
})

test_that("corrPrune requires mode = 'exact' to have >= 2 columns, with a corrPrune-specific message (#34)", {
  df <- data.frame(x = rnorm(10))

  expect_error(
    corrPrune(df, threshold = 0.7, mode = "exact"),
    "mode = 'exact' requires at least two variables"
  )

  # mode = "greedy" already handles single-column input.
  expect_s3_class(corrPrune(df, threshold = 0.7, mode = "greedy"), "data.frame")
})

test_that("corrPrune handles threshold = 0 under default mode = 'auto' (#34)", {
  # Regression test for issue #34: threshold = 0 is documented as valid
  # ("must be non-negative") and mode = "greedy" already handled it, but
  # mode = "auto" resolved to "exact", which routes through MatSelect()'s
  # stricter threshold in (0, 1] contract and crashed with MatSelect()'s
  # internal range-check message. mode = "auto" now degrades to greedy for
  # threshold == 0.
  set.seed(9341)
  df <- data.frame(x = rnorm(10), y = rnorm(10))

  result <- corrPrune(df, threshold = 0)
  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "mode"), "greedy")
})

test_that("corrPrune requires mode = 'exact' to have threshold > 0, with a corrPrune-specific message (#34)", {
  df <- data.frame(x = rnorm(10), y = rnorm(10))

  expect_error(
    corrPrune(df, threshold = 0, mode = "exact"),
    "mode = 'exact' requires 'threshold' > 0"
  )

  # mode = "greedy" already handles threshold = 0.
  expect_s3_class(corrPrune(df, threshold = 0, mode = "greedy"), "data.frame")
})

test_that("corrPrune rejects data with duplicate column names (#28)", {
  df <- data.frame(x = 1:10, y = 1:10, z = 1:10)
  names(df) <- c("a", "a", "b")

  expect_error(
    corrPrune(df),
    "duplicate column names"
  )
})

test_that("corrPrune deduplicates a repeated force_in name (#31)", {
  set.seed(9310)
  n <- 30
  df <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))

  result <- corrPrune(df, threshold = 0.9, force_in = c("a", "a"))

  expect_true("a" %in% names(result))
  expect_false(anyDuplicated(names(result)) > 0)
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
    "'threshold' must be in the range \\[0, 1\\]"
  )

  expect_error(
    corrPrune(df, threshold = NA),
    "'threshold' must be a single numeric value"
  )
})

test_that("corrPrune rejects threshold > 1 consistently across modes (#90)", {
  # Regression test for #90: mode = "auto" resolved to "exact" and routed
  # through MatSelect()'s own (0, 1] check for threshold > 1, but
  # mode = "greedy" had no such check and silently accepted it. corrPrune()
  # now validates the upper bound itself, before mode dispatch, so both
  # modes fail the same way on the same out-of-range input.
  set.seed(9342)
  df <- data.frame(x = rnorm(10), y = rnorm(10))

  expect_error(
    corrPrune(df, threshold = 1.5, mode = "auto"),
    "'threshold' must be in the range \\[0, 1\\]"
  )
  expect_error(
    corrPrune(df, threshold = 1.5, mode = "greedy"),
    "'threshold' must be in the range \\[0, 1\\]"
  )
  expect_error(
    corrPrune(df, threshold = 1.5, mode = "exact"),
    "'threshold' must be in the range \\[0, 1\\]"
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

test_that("corrPrune handles integer columns (converted to numeric internally, kept integer on output) (#88)", {
  set.seed(1004)
  n <- 30
  df <- data.frame(
    int1 = as.integer(sample(1:100, n, replace = TRUE)),
    int2 = as.integer(sample(1:100, n, replace = TRUE))
  )

  result <- corrPrune(df, threshold = 0.7, mode = "exact")
  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "measure"), "pearson")
  # Regression test for #88: the internal integer -> numeric conversion used
  # for association computation must not leak into the returned columns.
  expect_true(is.integer(result$int1))
  expect_true(is.integer(result$int2))
  expect_identical(result$int1, df$int1)
  expect_identical(result$int2, df$int2)
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
    "must be one of"
  )
})

test_that("corrPrune handles all rows with NA (errors)", {
  df <- data.frame(
    x1 = c(NA, NA, NA),
    x2 = c(NA, NA, NA)
  )

  # Should error - either "no complete element pairs|All rows contain missing values" or NA in matrix
  expect_error(
    corrPrune(df, threshold = 0.7)
  )
})

test_that("corrPrune handles constant-factor pairs, matching assocSelect (#33)", {
  # Regression test for issue #33: corrPrune()'s mixed-type dispatch had no
  # constant-column gate for the Cramer's V (factor-factor) branch, so a
  # single-level factor produced an undefined (NA) association via
  # table()/chisq.test() and tripped the "surface NA explicitly" stop, even
  # though assocSelect() -- which corrPrune()'s own docs claim to mirror --
  # already treats a constant categorical variable as association = 0.
  set.seed(9330)
  df <- data.frame(
    const_fac = factor(rep("A", 20)),
    other_fac = factor(sample(c("X", "Y", "Z"), 20, TRUE))
  )

  expect_no_error(res <- corrPrune(df, threshold = 0.9))
  expect_s3_class(res, "data.frame")
  expect_true(all(c("const_fac", "other_fac") %in% names(res)))

  # assocSelect() already handled this case; corrPrune() should now agree.
  expect_no_error(assocSelect(df, threshold = 0.9))
})

test_that("corrPrune handles a constant numeric column in a mixed-type data frame (#33)", {
  set.seed(9331)
  n <- 20
  df <- data.frame(
    const_num = rep(5, n),
    other_num = rnorm(n),
    fac = factor(sample(c("X", "Y", "Z"), n, replace = TRUE))
  )

  expect_no_error(res <- corrPrune(df, threshold = 0.9))
  expect_s3_class(res, "data.frame")
})

test_that("corrPrune still errors on genuinely all-missing columns after the #33 fix", {
  # The constant-column gate added for #33 is restricted to fully-observed
  # columns (no NA) so it doesn't mask genuinely undefined all-missing data
  # as a false "association = 0".
  df <- data.frame(x1 = c(NA, NA, NA), x2 = c(NA, NA, NA))
  expect_error(corrPrune(df, threshold = 0.7))

  df2 <- data.frame(
    x = c(NA, NA, NA, NA),
    y = c(NA, 1, NA, NA),
    z = c(NA, NA, 2, NA)
  )
  expect_error(
    corrPrune(df2, threshold = 0.7),
    "no complete element pairs|All rows contain missing values|undefined \\(NA\\) values"
  )
})

test_that("corrPrune mixed-type branch warns and reports n_rows_used on missing data, matching assocSelect (#35)", {
  # Regression test for issue #35: the mixed-type association path had no
  # complete-cases step (each pair type applied its own ad hoc NA policy)
  # and never warned about dropped rows, unlike the all-numeric path and
  # assocSelect(). n_rows_used was also computed but never attached to the
  # returned object's attributes.
  set.seed(9350)
  n <- 60
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n),
    grp = factor(sample(c("A", "B", "C"), n, replace = TRUE))
  )
  df$num1[sample(n, 10)] <- NA

  expect_warning(
    res <- corrPrune(df, threshold = 0.9),
    "Removed 10 rows with missing values"
  )
  expect_equal(attr(res, "n_rows_used"), 50L)

  # corrPrune() only prunes columns -- the returned data keeps every row,
  # including the ones with missing values that were excluded only for the
  # association computation.
  expect_equal(nrow(res), n)

  # assocSelect() should agree on how many rows were usable.
  suppressWarnings(res_assoc <- assocSelect(df, threshold = 0.9))
  expect_equal(res_assoc@n_rows_used, attr(res, "n_rows_used"))
})

test_that("corrPrune attaches n_rows_used with no missing data too", {
  set.seed(9351)
  n <- 30
  df <- data.frame(num1 = rnorm(n), grp = factor(sample(c("A", "B"), n, replace = TRUE)))

  res <- corrPrune(df, threshold = 0.9)
  expect_equal(attr(res, "n_rows_used"), n)
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

test_that("corrPrune grouped mode genuinely dispatches to exact search, not silently greedy (#82)", {
  set.seed(1112)
  n <- 60
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n),
    group = rep(c("A", "B"), each = n / 2)
  )

  result <- corrPrune(df, threshold = 0.5, by = "group", group_q = 0.5, mode = "exact")
  expect_equal(attr(result, "mode"), "exact")

  result_greedy <- corrPrune(df, threshold = 0.5, by = "group", group_q = 0.5, mode = "greedy")
  expect_equal(attr(result_greedy, "mode"), "greedy")
})

test_that("corrPrune falls back to a single variable when all vars are correlated", {
  set.seed(1113)
  n <- 50
  x1 <- rnorm(n)
  # All variables perfectly correlated
  df <- data.frame(
    x1 = x1,
    x2 = x1,
    x3 = x1
  )

  # With very low threshold, no pair can coexist -- the pairwise constraint
  # still holds vacuously for a single variable, so corrPrune() keeps exactly
  # one (lexicographically first among the tied singleton candidates).
  result <- corrPrune(df, threshold = 0.1, mode = "exact")
  expect_equal(attr(result, "selected_vars"), "x1")
  expect_equal(sort(attr(result, "removed_vars")), c("x2", "x3"))
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

test_that("corrPrune exact mode selects the lowest-avg-correlation size-2 subset (#84)", {
  # Despite the two-block correlation structure here, no two candidate
  # size-2 subsets share the same avg correlation (0.218-0.606, all
  # distinct) -- this is plain sort-by-avg-correlation selection, not a
  # tie-break; {V2, V3} is the unique lowest-avg-correlation candidate.
  set.seed(7001)
  n <- 200

  base1 <- rnorm(n)
  base2 <- rnorm(n)

  df <- data.frame(
    V1 = base1,
    V2 = base1 * 0.3 + rnorm(n, sd = sqrt(1 - 0.3^2)),
    V3 = base2,
    V4 = base2 * 0.3 + rnorm(n, sd = sqrt(1 - 0.3^2))
  )

  df$V3 <- df$V3 + 0.6 * df$V1
  df$V4 <- df$V4 + 0.6 * df$V2

  result <- corrPrune(df, threshold = 0.5, mode = "exact")

  expect_s3_class(result, "data.frame")
  expect_equal(sort(names(result)), c("V2", "V3"))
})

test_that("corrPrune exact mode keeps every variable when all pairs are mutually compatible (#84)", {
  # With threshold well above every pairwise correlation, the only maximal
  # subset is the full variable set -- there is nothing to choose between
  # (no tie, no lexicographic ordering involved), so the checkable claim is
  # simply that nothing gets dropped.
  set.seed(7002)
  n <- 50

  df <- data.frame(
    a1 = rnorm(n),
    a2 = rnorm(n),
    b1 = rnorm(n),
    b2 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.99, mode = "exact")

  expect_s3_class(result, "data.frame")
  expect_equal(sort(names(result)), sort(names(df)))
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

test_that("corrPrune integer columns stay integer in greedy mode output (#88)", {
  set.seed(7005)
  n <- 30
  df <- data.frame(
    int1 = as.integer(sample(1:100, n, replace = TRUE)),
    int2 = as.integer(sample(1:100, n, replace = TRUE)),
    num1 = rnorm(n)
  )

  result <- corrPrune(df, threshold = 0.8, mode = "greedy")

  expect_s3_class(result, "data.frame")
  for (nm in names(result)) {
    expect_identical(result[[nm]], df[[nm]])
  }
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

test_that("corrPrune combines an optional-package measure with by grouping (#82)", {
  skip_if_not(requireNamespace("WGCNA", quietly = TRUE))

  set.seed(4015)
  n <- 60
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n),
    group = rep(c("A", "B"), each = n / 2)
  )

  result <- corrPrune(df, threshold = 0.5, measure = "bicor", by = "group", group_q = 0.5)
  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "measure"), "bicor")
})

test_that("corrPrune combines the maximal-information-coefficient measure with by grouping (#82)", {
  skip_if_not(requireNamespace("minerva", quietly = TRUE))

  set.seed(4016)
  n <- 40  # smaller for speed
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n),
    group = rep(c("A", "B"), each = n / 2)
  )

  result <- corrPrune(df, threshold = 0.5, measure = "maximal", by = "group", group_q = 0.5)
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

test_that("corrPrune reports assoc_methods_used for mixed-type data (#82)", {
  set.seed(4020)
  n <- 50
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n),
    fac1 = factor(sample(c("A", "B"), n, replace = TRUE))
  )

  result <- corrPrune(df, threshold = 0.8)
  methods_used <- attr(result, "assoc_methods_used")

  expect_type(methods_used, "list")
  expect_true("numeric_numeric" %in% names(methods_used))
  expect_true("numeric_factor" %in% names(methods_used))
  expect_equal(methods_used[["numeric_numeric"]], "pearson")
  expect_equal(methods_used[["numeric_factor"]], "eta")
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


# ===========================================================================
# Edge case: All rows have NA in grouped aggregation
# ===========================================================================

test_that("corrPrune grouped pruning handles groups with all NA", {
  set.seed(5002)
  n <- 40

  # Create data where one group has only NA values
  df <- data.frame(
    x = c(rnorm(30), rep(NA, 10)),
    y = c(rnorm(30), rep(NA, 10)),
    z = rnorm(n),
    group = rep(c("good", "bad"), c(30, 10))
  )

  # Should handle the NA group gracefully
  expect_warning(
    result <- corrPrune(df, threshold = 0.8, by = "group"),
    "fewer than 2 complete rows"
  )
  expect_s3_class(result, "data.frame")
})


# ===========================================================================
# Lexicographic tie-breaking: lines 502-508 in corrPrune
# Need: multiple subsets with SAME size AND SAME avg correlation
# ===========================================================================

test_that("corrPrune exact mode selects the lowest-avg-correlation candidate among four same-size subsets (#84)", {
  # a,b are highly correlated (0.9), c,d are highly correlated (0.9), and
  # all cross pairs (a-c, a-d, b-c, b-d) are only approximately uncorrelated
  # (finite-sample noise, not exactly 0) -- giving four size-2 maximal
  # subsets {a,c}, {a,d}, {b,c}, {b,d} whose avg correlations are close but
  # NOT exactly tied. This originally claimed to test the lexicographic
  # tie-break with an inline comment asserting "a,c" wins -- neither claim
  # holds: the actual winner (verified below) is determined by sort-by-
  # avg-correlation, and it is "b,d", not "a,c". See
  # "corrPrune exact mode resolves an exact avg-correlation tie
  # lexicographically" elsewhere in this file for a genuine tie,
  # constructed so the four candidates are exactly equal.

  n <- 200
  set.seed(9001)

  base1 <- rnorm(n)
  base2 <- rnorm(n)

  a <- base1
  b <- base1 + rnorm(n, sd = 0.1)
  c <- base2
  d <- base2 + rnorm(n, sd = 0.1)

  df <- data.frame(a = a, b = b, c = c, d = d)

  cor_mat <- cor(df)
  expect_true(abs(cor_mat["a", "b"]) > 0.9)
  expect_true(abs(cor_mat["c", "d"]) > 0.9)
  expect_true(abs(cor_mat["a", "c"]) < 0.3)

  result <- corrPrune(df, threshold = 0.5, mode = "exact")
  expect_s3_class(result, "data.frame")
  expect_equal(sort(names(result)), c("b", "d"))

  result2 <- corrPrune(df, threshold = 0.5, mode = "exact")
  expect_identical(names(result), names(result2))
})

# ===========================================================================
# Chi-squared returning NA: line 350 in corrPrune
# Need: contingency table where chisq.test returns NA
# ===========================================================================

test_that("corrPrune handles chi-squared NA from degenerate table", {
  set.seed(9002)
  n <- 50

  # Create factors where chi-squared might produce NA
  # This happens with very sparse tables
  df <- data.frame(
    fac1 = factor(c(rep("A", 48), "B", "C")),  # Very unbalanced
    fac2 = factor(c(rep("X", 48), "Y", "Z")),  # Very unbalanced
    num1 = rnorm(n)
  )

  # Should handle gracefully
  result <- corrPrune(df, threshold = 0.95)
  expect_s3_class(result, "data.frame")
})

# ===========================================================================
# All NA in grouped aggregation: line 410 in corrPrune
# Need: all groups produce NA for a variable pair
# ===========================================================================


# ===========================================================================
# assocSelect: single-level factor (line 197) and constant numeric (line 199)
# ===========================================================================

test_that("assocSelect handles eta with single-level factor", {
  set.seed(9004)
  n <- 30

  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n),
    fac_single = factor(rep("only_level", n))
  )

  # eta computation should return 0 for single-level factor
  result <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(result, "CorrCombo"))
})

test_that("assocSelect handles eta with constant numeric", {
  set.seed(9005)
  n <- 30

  df <- data.frame(
    const_num = rep(42, n),  # Constant - ss_tot = 0
    fac1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    num2 = rnorm(n)
  )

  # eta computation should handle ss_tot = 0
  result <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(result, "CorrCombo"))
})

# ===========================================================================
# corrSelect: bicor, distance, maximal with installed packages
# ===========================================================================

test_that("corrSelect with bicor computes correctly", {
  skip_if_not(requireNamespace("WGCNA", quietly = TRUE))

  set.seed(9006)
  n <- 50
  x <- rnorm(n)
  df <- data.frame(
    a = x,
    b = x + rnorm(n, sd = 0.5),
    c = rnorm(n)
  )

  result <- corrSelect(df, threshold = 0.7, cor_method = "bicor")
  expect_true(inherits(result, "CorrCombo"))
  expect_true(length(result@subset_list) >= 1)
})

test_that("corrSelect with distance computes correctly", {
  skip_if_not(requireNamespace("energy", quietly = TRUE))

  set.seed(9007)
  n <- 30
  df <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))

  result <- corrSelect(df, threshold = 0.5, cor_method = "distance")
  expect_true(inherits(result, "CorrCombo"))
})

test_that("corrSelect with maximal computes correctly", {
  skip_if_not(requireNamespace("minerva", quietly = TRUE))

  set.seed(9008)
  n <- 30
  df <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))

  result <- corrSelect(df, threshold = 0.5, cor_method = "maximal")
  expect_true(inherits(result, "CorrCombo"))
})

# ===========================================================================
# VIF: predictor column matching edge case
# ===========================================================================


# ===========================================================================
# corrPrune: Deep edge case tests
# ===========================================================================

test_that("corrPrune errors when all rows have missing values", {
  df <- data.frame(
    x = c(NA, NA, NA, NA),
    y = c(NA, 1, NA, NA),
    z = c(NA, NA, 2, NA)
  )

  expect_error(
    corrPrune(df, threshold = 0.7),
    "no complete element pairs|All rows contain missing values"
  )
})

test_that("corrPrune errors clearly on a single-row data frame with no missing values (#64)", {
  # Before this fix, a single complete row fell through to
  # .numeric_assoc_matrix()'s constant-column check (sd() of one value is
  # NA, not 0/FALSE) and surfaced as an opaque "missing value where
  # TRUE/FALSE needed" rather than a corrPrune-specific message.
  df <- data.frame(x = 1, y = 2, z = 3)
  expect_error(
    corrPrune(df, threshold = 0.7),
    "Fewer than two complete-case rows"
  )
})

test_that("corrPrune auto mode switches from exact to greedy exactly at the default max_exact_p boundary (#64)", {
  # Exercises the actual documented default (100), not an overridden small
  # value -- distinct from the auto-mode tests elsewhere in this file that
  # explicitly pass a small max_exact_p to force a fast switch. n = 200 (not
  # a smaller n) keeps sampling noise low enough that the p = 95 exact-mode
  # compatibility graph stays dense (few chance correlations above 0.3), so
  # exact search stays fast rather than risking combinatorial blowup.
  set.seed(4001)
  n <- 200
  df_under <- as.data.frame(matrix(rnorm(n * 95), n, 95))   # p = 95 <= 100
  df_over  <- as.data.frame(matrix(rnorm(n * 105), n, 105)) # p = 105 > 100

  res_under <- corrPrune(df_under, threshold = 0.3, mode = "auto")
  res_over  <- corrPrune(df_over, threshold = 0.3, mode = "auto")

  expect_equal(attr(res_under, "mode"), "exact")
  expect_equal(attr(res_over, "mode"), "greedy")
})

test_that("corrPrune converts character columns to factor internally but returns the original character column (#88)", {
  set.seed(12001)
  n <- 20
  df <- data.frame(
    char_col = sample(c("apple", "banana", "cherry"), n, replace = TRUE),
    num_col = rnorm(n),
    stringsAsFactors = FALSE
  )

  res <- corrPrune(df, threshold = 0.9)
  expect_s3_class(res, "data.frame")
  expect_true(is.character(res$char_col))
  expect_identical(res$char_col, df$char_col)
})

test_that("corrPrune converts logical columns to factor internally but returns the original logical column (#88)", {
  set.seed(12002)
  n <- 20
  df <- data.frame(
    bool_col = sample(c(TRUE, FALSE), n, replace = TRUE),
    num_col = rnorm(n)
  )

  res <- corrPrune(df, threshold = 0.9)
  expect_s3_class(res, "data.frame")
  expect_true(is.logical(res$bool_col))
  expect_identical(res$bool_col, df$bool_col)
})

test_that("corrPrune converts integer columns to numeric internally but returns the original integer column (#88)", {
  set.seed(12003)
  n <- 20
  df <- data.frame(
    int_col = as.integer(sample(1:100, n, replace = TRUE)),
    num_col = rnorm(n)
  )

  res <- corrPrune(df, threshold = 0.9)
  expect_s3_class(res, "data.frame")
  expect_true(is.integer(res$int_col))
  expect_identical(res$int_col, df$int_col)
})

# ===========================================================================
# corrPrune: Mixed-type chi2 edge cases
# ===========================================================================

test_that("corrPrune handles factor-factor with sparse contingency table", {
  set.seed(16001)
  n <- 15
  df <- data.frame(
    f1 = factor(c(rep("A", 12), rep("B", 3))),
    f2 = factor(c(rep("X", 13), rep("Y", 2))),
    num = rnorm(n)
  )

  res <- corrPrune(df, threshold = 0.9)
  expect_s3_class(res, "data.frame")
})

test_that("corrPrune handles chi2 NA in mixed-type matrix computation", {
  set.seed(16002)
  # Create very degenerate factor-factor pair
  n <- 10
  df <- data.frame(
    f1 = factor(c(rep("A", 9), "B")),
    f2 = factor(c(rep("X", 9), "Y")),
    num = rnorm(n)
  )

  res <- corrPrune(df, threshold = 0.95)
  expect_s3_class(res, "data.frame")
})

test_that("corrPrune all-numeric with all rows NA errors correctly", {
  # All-numeric data where complete.cases returns nothing
  df <- data.frame(
    x = c(1, NA, NA),
    y = c(NA, 2, NA),
    z = c(NA, NA, 3)
  )

  expect_error(
    corrPrune(df, threshold = 0.7, measure = "pearson"),
    "no complete element pairs|All rows"
  )
})

# ===========================================================================
# Recovery-style and reference-verified tests (closes coverage gaps flagged
# in issue #27: prior tests mostly checked "does not error", not that the
# documented guarantees -- grouped aggregation math, exact tie-break order,
# greedy/exact agreement -- actually hold against an independently computed
# expectation).
# ===========================================================================

test_that("corrPrune grouped group_q aggregation matches hand-computed quantiles", {
  # Group A: x,y perfectly correlated. Group B: x,y correlated at exactly 0.3
  # (verified below via cor() directly, independent of corrPrune's internals).
  dfA <- data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 2, 3, 4, 5), group = "A")
  dfB <- data.frame(x = c(1, 2, 3, 4, 5), y = c(3, 1, 5, 2, 4), group = "B")
  cor_a <- abs(cor(dfA$x, dfA$y))
  cor_b <- abs(cor(dfB$x, dfB$y))
  expect_equal(cor_a, 1)
  expect_equal(round(cor_b, 4), 0.3)

  df <- rbind(dfA, dfB)
  threshold <- (cor_a + cor_b) / 2  # strictly between the two group correlations

  # group_q = 1 aggregates by the max across groups (cor_a = 1), which
  # exceeds `threshold` -- x and y cannot coexist.
  res_max <- corrPrune(df, threshold = threshold, by = "group", group_q = 1)
  expect_equal(attr(res_max, "selected_vars"), "x")

  # group_q near 0 aggregates toward the min across groups (cor_b = 0.3),
  # which is below `threshold` -- x and y can coexist.
  res_min <- corrPrune(df, threshold = threshold, by = "group", group_q = 0.01)
  expect_setequal(attr(res_min, "selected_vars"), c("x", "y"))
})

test_that("corrPrune grouped mode errors when a factor level is unused within one group (#55)", {
  # region has an "East" level, but group A only ever has North/South -- the
  # eta/Cramer's V computation for group A therefore returns NA for any pair
  # involving region, even though group A had plenty of complete rows. This
  # must not be silently dropped from the group_q aggregation (which would
  # let group_q = 1's "holds in all groups" guarantee pass unverified).
  set.seed(1)
  n <- 30
  site <- rep(c("A", "B"), each = 15)
  region <- factor(rep(NA_character_, n), levels = c("North", "South", "East"))
  region[site == "A"] <- sample(c("North", "South"), 15, replace = TRUE)
  region[site == "B"] <- sample(c("North", "South", "East"), 15, replace = TRUE)
  df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), region = region, site = site)

  expect_error(
    corrPrune(df, threshold = 0.9, by = "site", group_q = 1),
    "undefined"
  )
})

test_that("corrPrune grouped mode warns about NA rows confined to a single group (#55)", {
  df <- data.frame(x1 = rnorm(20), x2 = rnorm(20), grp = rep(c("A", "B"), each = 10))
  df$x1[c(1, 2, 3)] <- NA  # all within group A

  expect_warning(
    corrPrune(df, threshold = 0.9, by = "grp"),
    "Removed 3 row"
  )
})

test_that("corrPrune grouped mode's n_rows_used excludes skipped groups (#55)", {
  df <- data.frame(x1 = rnorm(10), x2 = rnorm(10),
                    grp = c(rep("A", 8), "B", "C"))  # B, C have 1 row each: skipped

  res <- suppressWarnings(corrPrune(df, threshold = 0.9, by = "grp"))
  expect_equal(attr(res, "n_rows_used"), 8)
})

test_that("corrPrune grouped mode warns when the by column itself has NA (#55)", {
  df <- data.frame(x1 = rnorm(20), x2 = rnorm(20), grp = rep(c("A", "B"), each = 10))
  df$grp[c(1, 2)] <- NA

  expect_warning(
    corrPrune(df, threshold = 0.9, by = "grp"),
    "missing values in the grouping variable"
  )
})

test_that("corrPrune exact mode resolves an exact avg-correlation tie lexicographically", {
  # Construct a,b,c,d so that a-b and c-d are perfectly anti-correlated
  # (excluded by the threshold) while all four cross pairs (a-c, a-d, b-c,
  # b-d) share the exact same absolute correlation, by making a=-b and c=-d
  # from two exactly orthogonal base vectors. This forces a genuine tie in
  # both size and avg correlation across all four candidate pairs, isolating
  # the documented lexicographic tie-break (not just "gives a reproducible
  # answer", but specifically the alphabetically-first one).
  set.seed(42)
  n <- 20
  Q <- qr.Q(qr(matrix(rnorm(n * 2), n, 2)))
  z1 <- Q[, 1]; z2 <- Q[, 2]
  df <- data.frame(a = z1, b = -z1, c = z2, d = -z2)

  m <- abs(cor(df))
  expect_equal(m["a", "c"], m["a", "d"], tolerance = 1e-10)
  expect_equal(m["a", "c"], m["b", "c"], tolerance = 1e-10)
  expect_equal(m["a", "c"], m["b", "d"], tolerance = 1e-10)

  result <- corrPrune(df, threshold = 0.5, mode = "exact")
  expect_equal(sort(attr(result, "selected_vars")), c("a", "c"))
})

test_that("corrPrune greedy mode removes the variable with the most violations first", {
  # a is built to correlate with both b (via base1) and c (via base2), while
  # b and c share nothing and are uncorrelated with each other. So a has 2
  # threshold violations (with b and with c) while b and c each have only 1
  # (with a) -- isolating the primary greedy tie-break criterion cleanly.
  set.seed(11)
  n <- 40
  base1 <- rnorm(n)
  base2 <- rnorm(n)
  df <- data.frame(
    a = base1 + base2,
    b = base1 + rnorm(n, sd = 0.05),
    c = base2 + rnorm(n, sd = 0.05),
    d = rnorm(n)
  )
  m <- abs(cor(df))
  expect_true(m["a", "b"] > 0.5 && m["a", "c"] > 0.5 && m["b", "c"] < 0.5)

  result <- corrPrune(df, threshold = 0.5, mode = "greedy")
  expect_equal(sort(attr(result, "selected_vars")), c("b", "c", "d"))
  expect_equal(attr(result, "removed_vars"), "a")
})

test_that("corrPrune greedy mode's 2nd tie-break (highest max association) determines the final subset (#63)", {
  # A and C tie on violation count (2 each: A via B,C; C via A,D) but A's max
  # association (0.95, with B) clearly exceeds C's (0.6) -- no near-tie
  # epsilon involved, a plain "which max is bigger" decision. Removing A
  # first (correct) vs. C first (what a level-2 comparison bug would do)
  # leads to two entirely different survivor sets, so this is observable
  # through the public API, not just internal removal order.
  m <- matrix(c(
    1,    0.95, 0.6,  0.1,
    0.95, 1,    0.05, 0.2,
    0.6,  0.05, 1,    0.6,
    0.1,  0.2,  0.6,  1
  ), 4, 4, byrow = TRUE)
  colnames(m) <- rownames(m) <- c("A", "B", "C", "D")

  keep <- corrselect:::greedyPruneBackend(m, 0.5, NULL)
  expect_setequal(colnames(m)[keep], c("B", "C"))
})

test_that("corrPrune greedy mode's 3rd tie-break (highest average association) determines the final subset (#63)", {
  # A and B mutually violate (0.9), so their max association always ties at
  # that shared edge -- isolating the 3rd tie-break (average association)
  # rather than the 2nd. A's average across all its edges (0.9, 0.4, 0.3) is
  # higher than B's (0.9, 0.2, 0.1), so A is judged worse and removed;
  # removing B instead (what a level-3 comparison bug would do) leaves a
  # different survivor set.
  m <- matrix(c(
    1,   0.9, 0.4, 0.3,
    0.9, 1,   0.2, 0.1,
    0.4, 0.2, 1,   0.1,
    0.3, 0.1, 0.1, 1
  ), 4, 4, byrow = TRUE)
  colnames(m) <- rownames(m) <- c("A", "B", "C", "D")

  keep <- corrselect:::greedyPruneBackend(m, 0.5, NULL)
  expect_setequal(colnames(m)[keep], c("B", "C", "D"))
})

test_that("corrPrune greedy mode's 4th tie-break (lowest column index removed) determines the final subset (#63)", {
  # A, B, C, E all tie on violation count, max association, and average
  # association by construction (each violates exactly one other variable
  # at 0.9 and has three legal 0.1 edges) -- isolating the final tie-break,
  # column index. The lowest-index variable among the tied set is removed
  # each round (A, then B), matching the documented "smallest index wins"
  # rule; removing the largest-index variable instead (a plausible inverted
  # comparison bug) leaves a different survivor set.
  m <- matrix(c(
    1,   0.1, 0.9, 0.1, 0.1,
    0.1, 1,   0.1, 0.1, 0.9,
    0.9, 0.1, 1,   0.1, 0.1,
    0.1, 0.1, 0.1, 1,   0.1,
    0.1, 0.9, 0.1, 0.1, 1
  ), 5, 5, byrow = TRUE)
  colnames(m) <- rownames(m) <- c("A", "B", "C", "D", "E")

  keep <- corrselect:::greedyPruneBackend(m, 0.5, NULL)
  expect_setequal(colnames(m)[keep], c("C", "D", "E"))
})

test_that("greedyPruneBackend() errors when two force_in variables mutually violate the threshold (#93)", {
  # Regression test for #93: when every variable in a violating pair is
  # protected by force_in, greedyPruneBackend() cannot remove either one to
  # satisfy the threshold and must stop() with a specific message, rather
  # than looping forever or silently returning both. This C++-level check is
  # unreachable through the public corrPrune() API (its R layer pre-checks
  # force_in-vs-force_in violations first), but is directly testable here,
  # matching the tie-break tests above that already call the backend
  # directly.
  m <- matrix(c(
    1,   0.95,
    0.95, 1
  ), 2, 2, byrow = TRUE)
  colnames(m) <- rownames(m) <- c("A", "B")

  # force_in indices are 0-based at this direct C++ boundary.
  expect_error(
    corrselect:::greedyPruneBackend(m, 0.5, c(0L, 1L)),
    "Cannot satisfy threshold: force_in variables violate the constraint"
  )
})

test_that("corrPrune greedy and exact modes agree on an unambiguous case", {
  # a, b, c are mutually near-independent; d is a near-exact blend of all
  # three, so it conflicts with each of them individually. {a,b,c} is the
  # unique largest valid subset -- both modes must return the same subset,
  # not merely a same-sized one.
  set.seed(21)
  n <- 60
  a <- rnorm(n); b <- rnorm(n); c <- rnorm(n)
  d <- (a + b + c) + rnorm(n, sd = 0.01)
  df <- data.frame(a = a, b = b, c = c, d = d)

  res_exact  <- corrPrune(df, threshold = 0.5, mode = "exact")
  res_greedy <- corrPrune(df, threshold = 0.5, mode = "greedy")
  expect_equal(sort(attr(res_exact, "selected_vars")), c("a", "b", "c"))
  expect_equal(
    sort(attr(res_exact, "selected_vars")),
    sort(attr(res_greedy, "selected_vars"))
  )
})

test_that("corrPrune force_in infeasibility is detected under grouped aggregation", {
  dfA <- data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 2, 3, 4, 5), group = "A")
  dfB <- data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 2, 3, 4, 5), group = "B")
  df <- rbind(dfA, dfB)

  expect_error(
    corrPrune(df, threshold = 0.5, force_in = c("x", "y"), by = "group"),
    "violate the threshold constraint"
  )
})

test_that("corrPrune errors when force_in overlaps with by (#82)", {
  df <- data.frame(x = c(1, 2, 3, 4), y = c(4, 3, 2, 1), group = c("A", "A", "B", "B"))

  expect_error(
    corrPrune(df, threshold = 0.5, force_in = c("x", "group"), by = "group"),
    "'force_in' cannot include grouping variable\\(s\\) named in 'by'"
  )
})

test_that("corrPrune recovers at-most-one-per-block structure across seeds", {
  # Two independent correlated blocks of two variables each, plus one
  # independent noise variable. A correct pruning should never retain both
  # members of the same block (they violate the threshold with each other)
  # and should always retain the independent noise variable.
  n_trials <- 20
  recovered <- 0
  for (seed in seq_len(n_trials)) {
    set.seed(1000 + seed)
    n <- 50
    block1_base <- rnorm(n)
    block2_base <- rnorm(n)
    df <- data.frame(
      b1_a = block1_base + rnorm(n, sd = 0.05),
      b1_b = block1_base + rnorm(n, sd = 0.05),
      b2_a = block2_base + rnorm(n, sd = 0.05),
      b2_b = block2_base + rnorm(n, sd = 0.05),
      noise = rnorm(n)
    )
    result <- corrPrune(df, threshold = 0.5, mode = "exact")
    sel <- attr(result, "selected_vars")
    ok <- sum(c("b1_a", "b1_b") %in% sel) <= 1 &&
      sum(c("b2_a", "b2_b") %in% sel) <= 1 &&
      "noise" %in% sel
    if (ok) recovered <- recovered + 1
  }
  expect_equal(recovered, n_trials)
})
