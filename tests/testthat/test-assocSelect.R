library(testthat)

test_that("assocSelect returns CorrCombo with mixed data", {
  df <- data.frame(
    a = rnorm(10),
    b = factor(sample(letters[1:3], 10, TRUE)),
    c = ordered(sample(1:3, 10, TRUE))
  )
  res <- assocSelect(df, threshold = 0.8, method = "els")
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect works with numeric-only input", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  res <- assocSelect(df, threshold = 0.5, method = "els")
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect drops rows with NA", {
  df <- data.frame(a = c(1, NA, 2), b = factor(c("x", "y", "x")))
  expect_warning(res <- assocSelect(df, threshold = 0.7), "Removed")
  expect_true(inherits(res, "CorrCombo"))
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
  # No pair of variables is compatible under such a strict threshold, so the
  # only maximal subsets are the three variables on their own (see #30).
  expect_length(res@subset_list, 3)
  expect_true(all(vapply(res@subset_list, length, integer(1)) == 1))
})


test_that("assocSelect allows forcing variables by name", {
  set.seed(42)
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
  skip_if_not_installed("tibble")
  df <- tibble::tibble(
    a = rnorm(10),
    b = factor(sample(c("a", "b", "c"), 10, TRUE)),
    c = ordered(sample(1:3, 10, TRUE))
  )
  res <- assocSelect(df, threshold = 0.7)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect handles constant columns", {
  df <- data.frame(
    a = rep(1, 10),
    b = rnorm(10),
    c = factor(rep("x", 10))
  )
  res <- assocSelect(df, threshold = 0.8)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect can use kendall for num-num", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))
  res <- assocSelect(df, method_num_num = "kendall", threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect can use cramersv for factor-factor", {
  df <- data.frame(
    x = factor(sample(letters[1:3], 10, TRUE)),
    y = factor(sample(letters[1:3], 10, TRUE))
  )
  # cramersv is the default for factor-factor, no parameter needed
  res <- assocSelect(df, threshold = 1.0)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect can use eta for numeric-factor", {
  df <- data.frame(
    x = rnorm(10),
    y = factor(rep(c("a", "b"), 5))
  )
  # eta is the default for numeric-factor, no parameter needed
  res <- assocSelect(df, threshold = 1.0)
  expect_true(inherits(res, "CorrCombo"))
})

# ===========================================================================
# Additional edge case tests for assocSelect
# ===========================================================================

test_that("assocSelect validates threshold argument", {
  df <- data.frame(a = rnorm(10), b = rnorm(10))

  expect_error(
    assocSelect(df, threshold = "0.5"),
    "threshold.*must be a single numeric"
  )

  expect_error(
    assocSelect(df, threshold = c(0.5, 0.7)),
    "threshold.*must be a single numeric"
  )

  expect_error(
    assocSelect(df, threshold = NA),
    "threshold.*must be a single numeric"
  )

  expect_error(
    assocSelect(df, threshold = 0),
    "threshold.*must be in the range"
  )

  expect_error(
    assocSelect(df, threshold = 1.5),
    "threshold.*must be in the range"
  )
})

test_that("assocSelect validates minimum columns", {
  df <- data.frame(a = rnorm(10))

  expect_error(
    assocSelect(df, threshold = 0.7),
    "at least two columns"
  )
})

test_that("assocSelect handles character columns (converted to factors)", {
  set.seed(2001)
  df <- data.frame(
    a = rnorm(15),
    b = sample(c("x", "y", "z"), 15, replace = TRUE),
    stringsAsFactors = FALSE
  )

  res <- assocSelect(df, threshold = 0.8)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect handles logical columns (converted to factors)", {
  set.seed(2002)
  df <- data.frame(
    a = rnorm(15),
    b = sample(c(TRUE, FALSE), 15, replace = TRUE)
  )

  res <- assocSelect(df, threshold = 0.8)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect handles integer columns (converted to numeric)", {
  set.seed(2003)
  df <- data.frame(
    a = as.integer(sample(1:100, 15, replace = TRUE)),
    b = as.integer(sample(1:100, 15, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.8)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect handles ordered-ordered pairs", {
  set.seed(2004)
  df <- data.frame(
    a = ordered(sample(c("low", "med", "high"), 15, replace = TRUE)),
    b = ordered(sample(c("small", "large"), 15, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect handles ordered-factor pairs", {
  set.seed(2005)
  df <- data.frame(
    a = ordered(sample(1:3, 15, replace = TRUE)),
    b = factor(sample(c("A", "B", "C"), 15, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect handles numeric-ordered pairs", {
  set.seed(2006)
  df <- data.frame(
    a = rnorm(15),
    b = ordered(sample(c("low", "med", "high"), 15, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect with kendall for ord_ord", {
  set.seed(2007)
  df <- data.frame(
    a = ordered(sample(1:3, 15, replace = TRUE)),
    b = ordered(sample(1:4, 15, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9, method_ord_ord = "kendall")
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect with spearman for num_num", {
  set.seed(2008)
  df <- data.frame(
    a = rnorm(15),
    b = rnorm(15),
    c = rnorm(15)
  )

  res <- assocSelect(df, threshold = 0.8, method_num_num = "spearman")
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect errors on force_in names not in data", {
  df <- data.frame(
    a = rnorm(10),
    b = factor(sample(c("x", "y"), 10, replace = TRUE))
  )

  expect_error(
    assocSelect(df, threshold = 0.9, force_in = c("a", "z")),
    "do not match column names"
  )
})

test_that("assocSelect with force_in by index", {
  set.seed(2009)
  df <- data.frame(
    a = rnorm(15),
    b = factor(sample(c("x", "y"), 15, replace = TRUE)),
    c = ordered(sample(1:3, 15, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9, force_in = c(1, 3))
  expect_true(inherits(res, "CorrCombo"))
  expect_true(all(c("a", "c") %in% unlist(res@subset_list)))
})

test_that("assocSelect handles factor with sparse table", {
  # Test case where chi-squared test might fail due to sparse data
  set.seed(2010)
  df <- data.frame(
    a = factor(c(rep("A", 8), rep("B", 2))),
    b = factor(c(rep("X", 9), "Y"))
  )

  # Should handle sparse table gracefully
  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect stores methods used in attributes", {
  set.seed(2011)
  df <- data.frame(
    a = rnorm(15),
    b = factor(sample(c("x", "y"), 15, replace = TRUE)),
    c = ordered(sample(1:3, 15, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)

  # Check that assoc_methods_used attribute exists
  expect_true(!is.null(attr(res, "assoc_methods_used")))
})

test_that("assocSelect handles all type combinations in one df", {
  set.seed(2012)
  n <- 20
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n),
    ord1 = ordered(sample(1:3, n, replace = TRUE)),
    ord2 = ordered(sample(c("low", "high"), n, replace = TRUE)),
    cat1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    cat2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
  expect_equal(res@cor_method, "mixed")
})

test_that("assocSelect handles data.table input (#82)", {
  skip_if_not_installed("data.table")

  set.seed(2012)
  n <- 20
  dt <- data.table::data.table(
    num1 = rnorm(n),
    num2 = rnorm(n),
    cat1 = factor(sample(c("A", "B", "C"), n, replace = TRUE))
  )

  res <- assocSelect(dt, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
  expect_equal(res@cor_method, "mixed")
})

# ===========================================================================
# Additional coverage tests for assocSelect.R
# ===========================================================================

test_that("assocSelect stores assoc_methods_used attribute", {
  set.seed(2013)
  n <- 20
  df <- data.frame(
    num = rnorm(n),
    cat = factor(sample(c("A", "B"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)

  # Check that assoc_methods_used attribute exists
  methods_used <- attr(res, "assoc_methods_used")
  expect_true(!is.null(methods_used))
  expect_true(is.list(methods_used))
})

test_that("assocSelect uses method = NULL to auto-select", {
  set.seed(2015)
  n <- 20
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n)
  )

  # Without force_in, should use bron-kerbosch
  res1 <- assocSelect(df, threshold = 0.9, method = NULL)
  expect_equal(res1@search_type, "bron-kerbosch")

  # With force_in, should use els
  res2 <- assocSelect(df, threshold = 0.9, method = NULL, force_in = "num1")
  expect_equal(res2@search_type, "els")
})

test_that("assocSelect with bicor method", {
  skip_if_not_installed("WGCNA")

  set.seed(2016)
  n <- 30
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n)
  )

  res <- assocSelect(df, threshold = 0.9, method_num_num = "bicor")
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect with distance method", {
  skip_if_not_installed("energy")

  set.seed(2017)
  n <- 30
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n)
  )

  res <- assocSelect(df, threshold = 0.9, method_num_num = "distance")
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect with maximal method", {
  skip_if_not_installed("minerva")

  set.seed(2018)
  n <- 30
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n)
  )

  res <- assocSelect(df, threshold = 0.9, method_num_num = "maximal")
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect errors for bicor when WGCNA not installed", {
  skip_if(requireNamespace("WGCNA", quietly = TRUE))

  df <- data.frame(num1 = rnorm(10), num2 = rnorm(10))

  expect_error(
    assocSelect(df, threshold = 0.9, method_num_num = "bicor"),
    "Install the 'WGCNA' package"
  )
})

test_that("assocSelect errors for distance when energy not installed", {
  skip_if(requireNamespace("energy", quietly = TRUE))

  df <- data.frame(num1 = rnorm(10), num2 = rnorm(10))

  expect_error(
    assocSelect(df, threshold = 0.9, method_num_num = "distance"),
    "Install the 'energy' package"
  )
})

test_that("assocSelect errors for maximal when minerva not installed", {
  skip_if(requireNamespace("minerva", quietly = TRUE))

  df <- data.frame(num1 = rnorm(10), num2 = rnorm(10))

  expect_error(
    assocSelect(df, threshold = 0.9, method_num_num = "maximal"),
    "Install the 'minerva' package"
  )
})

test_that("assocSelect handles factor with only one unique value", {
  set.seed(2019)
  n <- 20
  df <- data.frame(
    num1 = rnorm(n),
    single_level = factor(rep("A", n))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect handles numeric with zero variance (ss_tot = 0)", {
  set.seed(2020)
  n <- 20
  df <- data.frame(
    const_num = rep(3.14, n),
    cat1 = factor(sample(c("A", "B"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

# ===========================================================================
# Additional edge case tests for association methods
# ===========================================================================

test_that("assocSelect's cramersv matches a chisq.test()-derived reference once unused levels are dropped (#84)", {
  # Despite the original title, this is not actually a degenerate
  # "row/col all zeros" table for cramersv: droplevels() removes the
  # unused "C"/"Z" levels first, leaving an ordinary 2x2 table with every
  # row/column sum > 0, so it's a genuine, checkable finite value.
  set.seed(2021)
  factor1 <- factor(c(rep("A", 8), rep("B", 2)), levels = c("A", "B", "C"))
  factor2 <- factor(c(rep("X", 9), "Y"), levels = c("X", "Y", "Z"))
  df <- data.frame(factor1 = factor1, factor2 = factor2)

  tbl <- table(droplevels(factor1), droplevels(factor2))
  chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE))$statistic
  v_ref <- sqrt(as.numeric(chi2) / (sum(tbl) * (min(dim(tbl)) - 1)))

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], v_ref, tolerance = 1e-8)
})

test_that("assocSelect's cramersv matches a chisq.test()-derived reference on a minimal 2x2 table (#84)", {
  set.seed(2022)
  f1 <- factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
  f2 <- factor(c("X", "X", "Y", "Y", "X", "Y", "X", "Y"))
  df <- data.frame(f1 = f1, f2 = f2)

  tbl <- table(f1, f2)
  chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE))$statistic
  v_ref <- sqrt(as.numeric(chi2) / (sum(tbl) * (min(dim(tbl)) - 1)))

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], v_ref, tolerance = 1e-8)
})

test_that("assocSelect handles eta with numeric-factor pairs", {
  set.seed(2023)
  n <- 25
  df <- data.frame(
    numeric_var = rnorm(n),
    factor_var = factor(sample(c("Low", "Medium", "High"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))

  # Check that eta was specifically the method used (not merely non-NULL).
  methods_used <- attr(res, "assoc_methods_used")
  expect_equal(methods_used[["numeric_factor"]], "eta")
})

test_that("assocSelect handles eta with factor-numeric order", {
  set.seed(2024)
  n <- 25
  # Reverse order: factor first, numeric second
  df <- data.frame(
    factor_var = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    numeric_var = rnorm(n)
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))

  methods_used <- attr(res, "assoc_methods_used")
  expect_equal(methods_used[["factor_numeric"]], "eta")
})

test_that("assocSelect with spearman for numeric-ordered pairs", {
  set.seed(2026)
  n <- 25
  df <- data.frame(
    numeric_var = rnorm(n),
    ordered_var = ordered(sample(c("Low", "Med", "High"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9, method_num_ord = "spearman")
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect with kendall for numeric-ordered pairs", {
  set.seed(2027)
  n <- 25
  df <- data.frame(
    numeric_var = rnorm(n),
    ordered_var = ordered(sample(1:4, n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9, method_num_ord = "kendall")
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect stores all method types in attributes", {
  set.seed(2028)
  n <- 30
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n),
    ord1 = ordered(sample(1:3, n, replace = TRUE)),
    cat1 = factor(sample(c("A", "B"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)

  # Check assoc_methods_used
  methods_used <- attr(res, "assoc_methods_used")
  expect_true(!is.null(methods_used))
  expect_true(is.list(methods_used))

  # Check assoc_methods_all
  methods_all <- attr(res, "assoc_methods_all")
  expect_true(!is.null(methods_all))
})

test_that("assocSelect handles all unique values in factor (no repeated levels)", {
  set.seed(2029)
  n <- 10
  df <- data.frame(
    numeric_var = rnorm(n),
    unique_factor = factor(paste0("level_", 1:n))  # Each observation has unique level
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect's cramersv matches a chisq.test()-derived reference, not an NA/0 fallback (#84)", {
  # Despite the original title, no NA or fallback is involved: droplevels()
  # removes the unused "C"/"Z" levels first, leaving a normal table with
  # every row/column sum > 0 and a genuine, checkable finite cramersv.
  set.seed(2030)
  f1 <- factor(c(rep("A", 15), rep("B", 5)), levels = c("A", "B", "C"))
  f2 <- factor(c(rep("X", 18), rep("Y", 2)), levels = c("X", "Y", "Z"))
  df <- data.frame(f1 = f1, f2 = f2)

  tbl <- table(droplevels(f1), droplevels(f2))
  chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE))$statistic
  v_ref <- sqrt(as.numeric(chi2) / (sum(tbl) * (min(dim(tbl)) - 1)))

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], v_ref, tolerance = 1e-8)
})

test_that("assocSelect with all numeric uses pearson by default", {
  set.seed(2031)
  n <- 30
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n),
    num3 = rnorm(n)
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))

  # numeric_numeric is always populated for an all-numeric input with >= 2
  # columns -- the method actually used is the checkable claim here.
  methods_used <- attr(res, "assoc_methods_used")
  expect_equal(methods_used[["numeric_numeric"]], "pearson")
})

test_that("assocSelect handles ordered-factor pairs", {
  set.seed(2032)
  n <- 30
  df <- data.frame(
    ord_var = ordered(sample(c("Low", "Med", "High"), n, replace = TRUE)),
    factor_var = factor(sample(c("A", "B", "C"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect handles factor-ordered pairs (reverse)", {
  set.seed(2033)
  n <- 30
  df <- data.frame(
    factor_var = factor(sample(c("X", "Y", "Z"), n, replace = TRUE)),
    ord_var = ordered(sample(1:4, n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect handles ordered-numeric pairs", {
  set.seed(2034)
  n <- 30
  df <- data.frame(
    ord_var = ordered(sample(c("Low", "Med", "High"), n, replace = TRUE)),
    num_var = rnorm(n)
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

# ===========================================================================
# Edge case tests for association computation branches
# ===========================================================================

test_that("assocSelect gives a fully-observed single-level factor exactly zero Cramer's V (#84)", {
  # f1 has only one level -- this hits .pairwise_assoc_value()'s
  # constant-column short-circuit (association is well-defined as exactly
  # 0), not cramersv's own min(dim(tbl)) < 2 NA path, despite the original
  # title's "sparse 1-dim table" framing.
  set.seed(2035)
  df <- data.frame(
    f1 = factor(c(rep("A", 20))),
    f2 = factor(c(rep("X", 10), rep("Y", 10)))
  )

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], 0)
})

test_that("assocSelect's cramersv matches a chisq.test()-derived reference on an imbalanced table (#84)", {
  # Despite the original title, this table is imbalanced but not actually
  # sparse/degenerate (every row and column sum is > 0), so cramersv never
  # hits the NA path at all -- it's a genuine, checkable finite value.
  set.seed(2038)
  f1 <- factor(c(rep("A", 18), "B", "B"))
  f2 <- factor(c(rep("X", 17), rep("Y", 3)))
  df <- data.frame(f1 = f1, f2 = f2)

  tbl <- table(f1, f2)
  chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE))$statistic
  v_ref <- sqrt(as.numeric(chi2) / (sum(tbl) * (min(dim(tbl)) - 1)))

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], v_ref, tolerance = 1e-8)
})

test_that("assocSelect drops a pair into separate singleton subsets when cramersv reaches 1 exactly (#84)", {
  # Despite the original title claiming an "NA association fallback", no NA
  # is involved: f1 and f2 are perfectly aligned (every A row-position is X,
  # every B row-position is Y) after their unused "C"/"Z" levels are
  # dropped, giving cramersv = 1 exactly -- which exceeds threshold = 0.9,
  # so the pair can never coexist and each variable is its own maximal
  # (singleton) subset.
  set.seed(2039)
  f1 <- factor(c(rep("A", 15), rep("B", 5)), levels = c("A", "B", "C"))
  f2 <- factor(c(rep("X", 15), rep("Y", 5)), levels = c("X", "Y", "Z"))
  df <- data.frame(f1 = f1, f2 = f2)

  res <- assocSelect(df, threshold = 0.9)
  expect_equal(length(res@subset_list), 2)
  expect_setequal(vapply(res@subset_list, `[[`, character(1), 1), c("f1", "f2"))

  res_at_one <- assocSelect(df, threshold = 1)
  expect_equal(length(res_at_one@subset_list), 1)
  expect_equal(res_at_one@avg_corr[1], 1)
})

# ===========================================================================
# Additional edge case tests for full coverage
# ===========================================================================

test_that("assocSelect gives a single-level factor (sampled partner) exactly zero Cramer's V (#84)", {
  # Same constant-column short-circuit as the "single-level factor" test
  # above, with a randomly-sampled (rather than fixed) partner column.
  set.seed(3001)
  df <- data.frame(
    f1 = factor(rep("A", 20)),
    f2 = factor(sample(c("X", "Y"), 20, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], 0)
})

test_that("assocSelect's cramersv matches a chisq.test()-derived reference after unused levels are dropped (#84)", {
  # f1/f2 each carry an unused level ("C"/"Z") that droplevels() removes
  # before the contingency table is built -- exercising the same
  # droplevels() preprocessing as other tests, but here checked against an
  # independently-computed reference value rather than just class/shape.
  set.seed(3002)
  f1 <- factor(c(rep("A", 10), rep("B", 10)), levels = c("A", "B", "C"))
  f2 <- factor(c(rep("X", 15), rep("Y", 5)), levels = c("X", "Y", "Z"))
  df <- data.frame(f1 = f1, f2 = f2)

  tbl <- table(droplevels(f1), droplevels(f2))
  chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE))$statistic
  v_ref <- sqrt(as.numeric(chi2) / (sum(tbl) * (min(dim(tbl)) - 1)))

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], v_ref, tolerance = 1e-8)
})

test_that("assocSelect gives a constant numeric column exactly zero association with a factor (#84)", {
  set.seed(3005)
  n <- 25
  df <- data.frame(
    zero_var_num = rep(5.5, n),
    cat = factor(sample(c("A", "B"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], 0)
})

test_that("assocSelect errors when every row is dropped for missing values (#32, #45)", {
  # Regression test for issue #32: get_assoc()'s early gate treated
  # `unique(numeric(0))` (no data at all) the same as a genuinely constant
  # column, silently returning association = 0 and reporting a valid-looking
  # result with "Data Rows: 0 used" instead of surfacing an error. Issue #45
  # replaced the generic "NA values" diagnosis with an explicit too-few-rows
  # message for this case, mirroring corrSelect()'s check.
  df <- data.frame(x = rnorm(10), y = rep(NA_real_, 10), z = rnorm(10))

  expect_warning(
    expect_error(assocSelect(df, threshold = 0.7), "complete-case rows"),
    "Removed"
  )
})

test_that("assocSelect errors when only one complete-case row remains (#32, #45)", {
  df <- data.frame(
    x = c(1, NA, NA, NA),
    y = c(2, NA, NA, NA),
    z = c(3, NA, NA, NA)
  )

  expect_warning(
    expect_error(assocSelect(df, threshold = 0.7), "complete-case rows"),
    "Removed"
  )
})

test_that("assocSelect errors clearly on a single-row data frame with no missing values (#64)", {
  # Distinct from the two tests above: no NA-driven row drop at all, just a
  # data frame that starts with a single row.
  df <- data.frame(x = 1, y = 2, z = 3)
  expect_error(assocSelect(df, threshold = 0.7), "complete-case rows")
})

test_that("assocSelect NA in association matrix triggers error", {
  # Regression test for issue #37: this test used to run assocSelect() on
  # two clean numeric columns and only assert the call didn't error --
  # never constructing NA-producing input, despite its name and its own
  # comment claiming the NA-triggers-an-error path was "difficult to
  # trigger directly". It would have kept passing even if the
  # anyNA(mat) -> stop() branch it's named after were deleted entirely.
  #
  # Real repro: assocSelect() calls droplevels() on factor columns before
  # removing rows with missing values, so a factor level that only occurs
  # in a row later dropped for missing data survives as a declared-but-
  # unobserved level. Cramer's V's contingency table then has an all-zero
  # row/column for that level, which is undefined, not 0.
  set.seed(3006)
  df <- data.frame(
    cat1 = factor(c("A", "A", "B", "B", "B", "C", rep("A", 14)), levels = c("A", "B", "C")),
    cat2 = factor(c("X", "Y", "X", "Y", "X", NA, sample(c("X", "Y"), 14, TRUE)))
  )

  expect_warning(
    expect_error(assocSelect(df, threshold = 0.9), "NA values"),
    "Removed"
  )
})

test_that("assocSelect succeeds on clean numeric data (smoke test)", {
  set.seed(3006)
  df <- data.frame(
    num1 = rnorm(20),
    num2 = rnorm(20)
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect handles force_in as numeric indices", {
  set.seed(3007)
  n <- 25
  df <- data.frame(
    num1 = rnorm(n),
    cat1 = factor(sample(c("A", "B"), n, replace = TRUE)),
    ord1 = ordered(sample(1:3, n, replace = TRUE))
  )

  # Force in by numeric index
  res <- assocSelect(df, threshold = 0.9, force_in = c(1, 3))

  expect_true(inherits(res, "CorrCombo"))
  expect_true(all(c("num1", "ord1") %in% unlist(res@subset_list)))
})

test_that("assocSelect spearman for numeric-numeric pairs", {
  set.seed(3008)
  df <- data.frame(
    num1 = rnorm(30),
    num2 = rnorm(30),
    num3 = rnorm(30)
  )

  res <- assocSelect(df, threshold = 0.8, method_num_num = "spearman")
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect kendall for ordered-ordered pairs", {
  set.seed(3009)
  n <- 30
  df <- data.frame(
    ord1 = ordered(sample(1:4, n, replace = TRUE)),
    ord2 = ordered(sample(c("low", "med", "high"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9, method_ord_ord = "kendall")
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect handles all methods in single mixed dataset", {
  set.seed(3010)
  n <- 40
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n),
    ord1 = ordered(sample(1:3, n, replace = TRUE)),
    ord2 = ordered(sample(c("L", "M", "H"), n, replace = TRUE)),
    cat1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    cat2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)

  # Check all method types were recorded
  methods_used <- attr(res, "assoc_methods_used")
  expect_true(!is.null(methods_used))
})

test_that("assocSelect's cramersv matches a chisq.test()-derived reference on a heavily imbalanced table (#84)", {
  set.seed(3011)
  f1 <- factor(c(rep("A", 48), rep("B", 2)))
  f2 <- factor(c(rep("X", 49), "Y"))
  df <- data.frame(f1 = f1, f2 = f2)

  tbl <- table(f1, f2)
  chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE))$statistic
  v_ref <- sqrt(as.numeric(chi2) / (sum(tbl) * (min(dim(tbl)) - 1)))

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], v_ref, tolerance = 1e-8)
})

test_that("assocSelect ordered-numeric pairs work correctly", {
  set.seed(3012)
  n <- 30
  df <- data.frame(
    ord1 = ordered(sample(1:4, n, replace = TRUE)),
    num1 = rnorm(n)
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect with kendall for numeric-ordered pairs", {
  set.seed(3013)
  n <- 30
  df <- data.frame(
    num1 = rnorm(n),
    ord1 = ordered(sample(1:3, n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9, method_num_ord = "kendall")
  expect_true(inherits(res, "CorrCombo"))
})

# ===========================================================================
# Edge case tests to increase coverage
# ===========================================================================

test_that("assocSelect gives a fully-constant factor column exactly zero Cramer's V (#84)", {
  # f2 has only one observed level -- the constant-column short-circuit in
  # .pairwise_assoc_value() fires (association well-defined as exactly 0),
  # not cramersv's own NA path, despite the original "degenerate table"
  # framing.
  set.seed(3101)
  df <- data.frame(
    f1 = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B")),
    f2 = factor(c("X", "X", "X", "X", "X", "X", "X", "X", "X", "X"))
  )

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], 0)
})

test_that("assocSelect's eta matches a hand-computed sum-of-squares reference when one category is constant (#84)", {
  set.seed(3102)
  n <- 20
  num <- c(rep(0, 10), rnorm(10))
  cat <- factor(c(rep("A", 10), rep("B", 10)))
  df <- data.frame(num = num, cat = cat)

  ss_tot <- sum((num - mean(num))^2)
  ss_bet <- sum(tapply(num, cat, function(z) length(z) * (mean(z) - mean(num))^2))
  eta_ref <- ss_bet / ss_tot

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], eta_ref, tolerance = 1e-8)
})

test_that("assocSelect handles all-zero variance after split by factor", {
  set.seed(3103)
  n <- 20
  # All values constant within each factor level - ss_within = 0
  df <- data.frame(
    num = c(rep(5, 10), rep(10, 10)),  # Different constants per level
    cat = factor(c(rep("A", 10), rep("B", 10)))
  )

  res <- assocSelect(df, threshold = 0.99)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect falls back gracefully with problematic pairs", {
  set.seed(3104)
  n <- 30
  # Create an ordered with only 1 level (edge case)
  df <- data.frame(
    num1 = rnorm(n),
    num2 = rnorm(n),
    ord1 = ordered(rep(1, n))  # Single level ordered
  )

  res <- assocSelect(df, threshold = 0.99)
  expect_true(inherits(res, "CorrCombo"))
})

test_that("assocSelect errors on unsupported method", {
  set.seed(3108)
  df <- data.frame(num1 = rnorm(20), num2 = rnorm(20))

  expect_error(
    assocSelect(df, threshold = 0.9, method_num_num = "unsupported_method"),
    "should be one of"
  )
})


# ===========================================================================
# Tests for assocSelect with optional measures
# ===========================================================================

test_that("assocSelect with bicor for numeric pairs works", {
  skip_if_not(requireNamespace("WGCNA", quietly = TRUE))

  set.seed(6001)
  n <- 50
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n)
  )

  result <- assocSelect(df, threshold = 0.5, method_num_num = "bicor")
  expect_true(inherits(result, "CorrCombo"))
})

test_that("assocSelect with distance correlation works", {
  skip_if_not(requireNamespace("energy", quietly = TRUE))

  set.seed(6002)
  n <- 30
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n)
  )

  result <- assocSelect(df, threshold = 0.5, method_num_num = "distance")
  expect_true(inherits(result, "CorrCombo"))
})

test_that("assocSelect with maximal information coefficient works", {
  skip_if_not(requireNamespace("minerva", quietly = TRUE))

  set.seed(6003)
  n <- 30
  df <- data.frame(
    x = rnorm(n),
    y = rnorm(n),
    z = rnorm(n)
  )

  result <- assocSelect(df, threshold = 0.5, method_num_num = "maximal")
  expect_true(inherits(result, "CorrCombo"))
})


# ===========================================================================
# Edge case tests for assocSelect
# ===========================================================================

test_that("assocSelect handles ordered-numeric pairs", {
  set.seed(7001)
  n <- 50
  df <- data.frame(
    num1 = rnorm(n),
    ord1 = ordered(sample(1:4, n, replace = TRUE)),
    num2 = rnorm(n)
  )

  result <- assocSelect(df, threshold = 0.8)
  expect_true(inherits(result, "CorrCombo"))
})

test_that("assocSelect handles ordered-ordered pairs", {
  set.seed(7002)
  n <- 50
  df <- data.frame(
    ord1 = ordered(sample(1:3, n, replace = TRUE)),
    ord2 = ordered(sample(1:3, n, replace = TRUE)),
    ord3 = ordered(sample(1:3, n, replace = TRUE))
  )

  result <- assocSelect(df, threshold = 0.8)
  expect_true(inherits(result, "CorrCombo"))
})

test_that("assocSelect handles factor-ordered pairs", {
  set.seed(7003)
  n <- 50
  df <- data.frame(
    fac1 = factor(sample(c("A", "B"), n, replace = TRUE)),
    ord1 = ordered(sample(1:3, n, replace = TRUE)),
    num1 = rnorm(n)
  )

  result <- assocSelect(df, threshold = 0.9)
  expect_true(inherits(result, "CorrCombo"))
})


# ===========================================================================
# assocSelect: cramersv with sparse table (line 187)
# ===========================================================================

test_that("assocSelect's cramersv matches a chisq.test()-derived reference on a near-degenerate table (#84)", {
  set.seed(10003)
  n <- 30
  fac1 <- factor(c(rep("A", n - 1), "B"))
  fac2 <- factor(c("X", rep("Y", n - 1)))
  df <- data.frame(fac1 = fac1, fac2 = fac2)

  tbl <- table(fac1, fac2)
  chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE))$statistic
  v_ref <- sqrt(as.numeric(chi2) / (sum(tbl) * (min(dim(tbl)) - 1)))

  result <- assocSelect(df, threshold = 1)
  expect_equal(length(result@subset_list), 1)
  expect_equal(result@avg_corr[1], v_ref, tolerance = 1e-8)
})

# ===========================================================================
# assocSelect: numeric-ordered with spearman (line 219, 226)
# ===========================================================================

test_that("assocSelect computes spearman for numeric-ordered pairs", {
  set.seed(10004)
  n <- 50

  df <- data.frame(
    num1 = rnorm(n),
    ord1 = ordered(sample(1:5, n, replace = TRUE)),
    num2 = rnorm(n)
  )

  result <- assocSelect(df, threshold = 0.8, method_num_ord = "spearman")
  expect_true(inherits(result, "CorrCombo"))
})

# ===========================================================================
# assocSelect: ordered-ordered pairs (line 235)
# ===========================================================================

test_that("assocSelect computes association for ordered-ordered pairs", {
  set.seed(10005)
  n <- 50

  df <- data.frame(
    ord1 = ordered(sample(1:4, n, replace = TRUE)),
    ord2 = ordered(sample(1:4, n, replace = TRUE)),
    num1 = rnorm(n)
  )

  result <- assocSelect(df, threshold = 0.8, method_ord_ord = "spearman")
  expect_true(inherits(result, "CorrCombo"))
})

# ===========================================================================
# Deep edge case tests for get_assoc internal function
# ===========================================================================

test_that("assocSelect gives a single-level factor exactly zero Cramer's V against a multi-level factor (#84)", {
  set.seed(11001)
  n <- 20
  single_level <- factor(rep("A", n))
  multi_level <- factor(sample(c("X", "Y", "Z"), n, replace = TRUE))
  df <- data.frame(single_level = single_level, multi_level = multi_level)

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], 0)
})

test_that("assocSelect's cramersv matches a chisq.test()-derived reference once an unused level is dropped, not NA (#84)", {
  # Despite the original title claiming a "zero row", droplevels() removes
  # f1's unused "C" level before the table is built, and the remaining
  # table is a perfect A<->X / B<->Y bijection -- cramersv is exactly 1,
  # not NA.
  set.seed(11002)
  f1 <- factor(c("A", "A", "A", "A", "B", "B"), levels = c("A", "B", "C"))
  f2 <- factor(c("X", "X", "X", "X", "Y", "Y"), levels = c("X", "Y"))
  df <- data.frame(f1 = f1, f2 = f2)

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], 1)
})

test_that("assocSelect gives a constant numeric column exactly zero eta association with a factor (#84)", {
  set.seed(11003)
  n <- 20
  df <- data.frame(
    const_num = rep(5.0, n),
    factor_var = factor(sample(c("A", "B"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], 0)
})

test_that("assocSelect gives a single-level factor exactly zero eta association with a numeric variable (#84)", {
  set.seed(11004)
  n <- 15
  df <- data.frame(
    numeric_var = rnorm(n),
    single_cat = factor(rep("only", n))
  )

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], 0)
})

# ===========================================================================
# Deep synthetic edge cases for get_assoc internal function
# ===========================================================================

test_that("assocSelect gives a perfectly-aligned imbalanced 2-level pair Cramer's V of exactly 1, not NA (#84)", {
  # Every A row-position is X and every B row-position is Y -- a perfect
  # bijection, so cramersv is exactly 1 despite the imbalance, contrary to
  # the "chi2 might be NA" framing these tests (three near-duplicates,
  # consolidated here) originally claimed.
  set.seed(15005)
  n <- 10
  f1 <- factor(c(rep("A", n - 1), "B"))
  f2 <- factor(c(rep("X", n - 1), "Y"))
  df <- data.frame(f1 = f1, f2 = f2)

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], 1)
})
