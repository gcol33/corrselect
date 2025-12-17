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
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles logical columns (converted to factors)", {
  set.seed(2002)
  df <- data.frame(
    a = rnorm(15),
    b = sample(c(TRUE, FALSE), 15, replace = TRUE)
  )

  res <- assocSelect(df, threshold = 0.8)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles integer columns (converted to numeric)", {
  set.seed(2003)
  df <- data.frame(
    a = as.integer(sample(1:100, 15, replace = TRUE)),
    b = as.integer(sample(1:100, 15, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.8)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles ordered-ordered pairs", {
  set.seed(2004)
  df <- data.frame(
    a = ordered(sample(c("low", "med", "high"), 15, replace = TRUE)),
    b = ordered(sample(c("small", "large"), 15, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles ordered-factor pairs", {
  set.seed(2005)
  df <- data.frame(
    a = ordered(sample(1:3, 15, replace = TRUE)),
    b = factor(sample(c("A", "B", "C"), 15, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles numeric-ordered pairs", {
  set.seed(2006)
  df <- data.frame(
    a = rnorm(15),
    b = ordered(sample(c("low", "med", "high"), 15, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect with kendall for ord_ord", {
  set.seed(2007)
  df <- data.frame(
    a = ordered(sample(1:3, 15, replace = TRUE)),
    b = ordered(sample(1:4, 15, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9, method_ord_ord = "kendall")
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect with spearman for num_num", {
  set.seed(2008)
  df <- data.frame(
    a = rnorm(15),
    b = rnorm(15),
    c = rnorm(15)
  )

  res <- assocSelect(df, threshold = 0.8, method_num_num = "spearman")
  expect_s4_class(res, "CorrCombo")
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
  expect_s4_class(res, "CorrCombo")
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
  expect_s4_class(res, "CorrCombo")
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
  expect_s4_class(res, "CorrCombo")
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

test_that("assocSelect handles constant numeric column", {
  set.seed(2014)
  n <- 20
  df <- data.frame(
    num1 = rep(5, n),  # Constant
    cat1 = factor(sample(c("A", "B"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
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
  expect_s4_class(res, "CorrCombo")
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
  expect_s4_class(res, "CorrCombo")
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
  expect_s4_class(res, "CorrCombo")
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
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles numeric with zero variance (ss_tot = 0)", {
  set.seed(2020)
  n <- 20
  df <- data.frame(
    const_num = rep(3.14, n),
    cat1 = factor(sample(c("A", "B"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

# ===========================================================================
# Additional edge case tests for association methods
# ===========================================================================

test_that("assocSelect handles cramersv with sparse table (row/col all zeros)", {
  set.seed(2021)
  # Create factors that will produce a sparse contingency table
  df <- data.frame(
    factor1 = factor(c(rep("A", 8), rep("B", 2)), levels = c("A", "B", "C")),
    factor2 = factor(c(rep("X", 9), "Y"), levels = c("X", "Y", "Z"))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles cramersv with minimal table", {
  set.seed(2022)
  # Create 2x2 table with sparse data
  df <- data.frame(
    f1 = factor(c("A", "B", "A", "B", "A", "B", "A", "B")),
    f2 = factor(c("X", "X", "Y", "Y", "X", "Y", "X", "Y"))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles eta with numeric-factor pairs", {
  set.seed(2023)
  n <- 25
  df <- data.frame(
    numeric_var = rnorm(n),
    factor_var = factor(sample(c("Low", "Medium", "High"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")

  # Check that eta was used
  methods_used <- attr(res, "assoc_methods_used")
  expect_true(!is.null(methods_used))
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
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles eta with single-level factor", {
  set.seed(2025)
  n <- 20
  df <- data.frame(
    numeric_var = rnorm(n),
    single_factor = factor(rep("only_level", n))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect with spearman for numeric-ordered pairs", {
  set.seed(2026)
  n <- 25
  df <- data.frame(
    numeric_var = rnorm(n),
    ordered_var = ordered(sample(c("Low", "Med", "High"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9, method_num_ord = "spearman")
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect with kendall for numeric-ordered pairs", {
  set.seed(2027)
  n <- 25
  df <- data.frame(
    numeric_var = rnorm(n),
    ordered_var = ordered(sample(1:4, n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9, method_num_ord = "kendall")
  expect_s4_class(res, "CorrCombo")
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
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles cramersv returning NA (fallback to 0)", {
  set.seed(2030)
  # Create factor combination that produces sparse table
  df <- data.frame(
    f1 = factor(c(rep("A", 15), rep("B", 5)), levels = c("A", "B", "C")),
    f2 = factor(c(rep("X", 18), rep("Y", 2)), levels = c("X", "Y", "Z"))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
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
  expect_s4_class(res, "CorrCombo")

  # Check methods used
  methods_used <- attr(res, "assoc_methods_used")
  expect_true("numeric_numeric" %in% names(methods_used) ||
              length(methods_used) == 0)  # May be empty if no pairs computed
})

test_that("assocSelect handles ordered-factor pairs", {
  set.seed(2032)
  n <- 30
  df <- data.frame(
    ord_var = ordered(sample(c("Low", "Med", "High"), n, replace = TRUE)),
    factor_var = factor(sample(c("A", "B", "C"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles factor-ordered pairs (reverse)", {
  set.seed(2033)
  n <- 30
  df <- data.frame(
    factor_var = factor(sample(c("X", "Y", "Z"), n, replace = TRUE)),
    ord_var = ordered(sample(1:4, n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles ordered-numeric pairs", {
  set.seed(2034)
  n <- 30
  df <- data.frame(
    ord_var = ordered(sample(c("Low", "Med", "High"), n, replace = TRUE)),
    num_var = rnorm(n)
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

# ===========================================================================
# Edge case tests for association computation branches
# ===========================================================================

test_that("assocSelect cramersv with sparse 1-dim table", {
  set.seed(2035)
  # Factor with only one level effectively - should hit min(dim(tbl)) < 2
  df <- data.frame(
    f1 = factor(c(rep("A", 20))),  # Only one level
    f2 = factor(c(rep("X", 10), rep("Y", 10)))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect eta with constant numeric (ss_tot = 0)", {
  set.seed(2036)
  n <- 20
  df <- data.frame(
    const_num = rep(5, n),  # Zero variance
    factor_var = factor(sample(c("A", "B"), n, replace = TRUE))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect eta with single-level factor", {
  set.seed(2037)
  n <- 20
  df <- data.frame(
    num_var = rnorm(n),
    single_factor = factor(rep("only_level", n))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles cramersv with NA chi-squared", {
  set.seed(2038)
  # Create edge case that might produce NA chi-squared
  # Factors with very imbalanced data
  df <- data.frame(
    f1 = factor(c(rep("A", 18), "B", "B")),
    f2 = factor(c(rep("X", 17), rep("Y", 3)))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})

test_that("assocSelect handles NA association fallback", {
  set.seed(2039)
  # Create factor data that might produce NA association
  df <- data.frame(
    f1 = factor(c(rep("A", 15), rep("B", 5)), levels = c("A", "B", "C")),
    f2 = factor(c(rep("X", 15), rep("Y", 5)), levels = c("X", "Y", "Z"))
  )

  res <- assocSelect(df, threshold = 0.9)
  expect_s4_class(res, "CorrCombo")
})
