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
