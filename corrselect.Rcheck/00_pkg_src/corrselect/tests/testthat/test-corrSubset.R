library(testthat)

test_that("errors if res is not a CorrCombo", {
  df <- data.frame(A = 1:5)
  fake <- list(subset_list = list(c("A")), names = "A")
  expect_error(
    corrSubset(fake, df),
    "`res` must be a CorrCombo object\\."
  )
})

test_that("errors if df is not a data.frame or matrix", {
  res <- new("CorrCombo",
             subset_list = list(c("A")),
             avg_corr    = 0.0,
             min_corr    = 0.0,
             max_corr    = 0.0,
             names       = "A",
             threshold   = 0.5,
             forced_in   = character(),
             search_type = "els",
             n_rows_used = 1L)
  expect_error(
    corrSubset(res, list(A = 1:5)),
    "`df` must be a data frame or matrix\\."
  )
})

test_that("errors if required columns are missing in df", {
  df <- data.frame(A = 1:5)
  res <- new("CorrCombo",
             subset_list = list(c("A", "B")),
             avg_corr    = 0.1,
             min_corr    = 0.05,
             max_corr    = 0.2,
             names       = c("A", "B"),
             threshold   = 0.5,
             forced_in   = character(),
             search_type = "els",
             n_rows_used = 5L)
  expect_error(
    corrSubset(res, df),
    "The following variables are missing in `df`: B"
  )
})

test_that("default (which = 'best') returns first subset as data.frame", {
  df <- data.frame(A = 1:5, B = 6:10, C = 11:15)
  res <- new("CorrCombo",
             subset_list = list(c("A", "C"), c("B", "C")),
             avg_corr    = c(0.1, 0.2),
             min_corr    = c(0.05, 0.1),
             max_corr    = c(0.2, 0.3),
             names       = c("A", "B", "C"),
             threshold   = 0.5,
             forced_in   = character(),
             search_type = "els",
             n_rows_used = 5L)
  out <- corrSubset(res, df)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("A", "C"))
})

test_that("which = integer returns that subset as data.frame", {
  df <- data.frame(A = 1:5, B = 6:10, C = 11:15)
  res <- new("CorrCombo",
             subset_list = list(c("A", "B"), c("B", "C"), c("A", "C")),
             avg_corr    = c(0.1, 0.2, 0.15),
             min_corr    = c(0.05, 0.1, 0.07),
             max_corr    = c(0.2, 0.3, 0.25),
             names       = c("A", "B", "C"),
             threshold   = 0.5,
             forced_in   = character(),
             search_type = "els",
             n_rows_used = 5L)
  out2 <- corrSubset(res, df, which = 3)
  expect_s3_class(out2, "data.frame")
  expect_named(out2, c("A", "C"))
})

test_that("which = vector returns named list of data.frames", {
  df <- data.frame(A = 1:5, B = 6:10, C = 11:15)
  res <- new("CorrCombo",
             subset_list = list(c("A", "B"), c("B", "C"), c("A", "C")),
             avg_corr    = c(0.1, 0.2, 0.15),
             min_corr    = c(0.05, 0.1, 0.07),
             max_corr    = c(0.2, 0.3, 0.25),
             names       = c("A", "B", "C"),
             threshold   = 0.5,
             forced_in   = character(),
             search_type = "els",
             n_rows_used = 5L)
  out_list <- corrSubset(res, df, which = c(1, 3))
  expect_type(out_list, "list")
  expect_named(out_list, c("Subset1", "Subset3"))
  expect_named(out_list$Subset1, c("A", "B"))
  expect_named(out_list$Subset3, c("A", "C"))
})

test_that("which = 'all' returns all subsets named", {
  df <- data.frame(A = 1:5, B = 6:10, C = 11:15)
  res <- new("CorrCombo",
             subset_list = list(c("A", "B"), c("B", "C")),
             avg_corr    = c(0.1, 0.2),
             min_corr    = c(0.05, 0.1),
             max_corr    = c(0.2, 0.3),
             names       = c("A", "B", "C"),
             threshold   = 0.5,
             forced_in   = character(),
             search_type = "els",
             n_rows_used = 5L)
  out_all <- corrSubset(res, df, which = "all")
  expect_type(out_all, "list")
  expect_length(out_all, 2)
  expect_named(out_all, c("Subset1", "Subset2"))
})

test_that("keepExtra = TRUE preserves non-selected columns", {
  df <- data.frame(
    A      = 1:5,
    B      = 6:10,
    C      = 11:15,         # included because res@names has C
    extra1 = letters[1:5],
    extra2 = LETTERS[1:5]
  )
  res <- new("CorrCombo",
             subset_list = list(c("A", "B")),
             avg_corr    = 0.1,
             min_corr    = 0.05,
             max_corr    = 0.2,
             names       = c("A", "B", "C"),
             threshold   = 0.5,
             forced_in   = character(),
             search_type = "els",
             n_rows_used = 5L)
  out_kept <- corrSubset(res, df, keepExtra = TRUE)
  # Only A, B and the extra columns (not C) should be present
  expect_named(out_kept, c("A", "B", "extra1", "extra2"))
  expect_false("C" %in% names(out_kept))
})

test_that("warns on rows with missing values in selected vars", {
  df <- data.frame(A = c(1, NA, 3, 4), B = 5:8)
  res <- new("CorrCombo",
             subset_list = list(c("A", "B")),
             avg_corr    = 0.1,
             min_corr    = 0.05,
             max_corr    = 0.2,
             names       = c("A", "B"),
             threshold   = 0.5,
             forced_in   = character(),
             search_type = "els",
             n_rows_used = 4L)
  expect_warning(
    corrSubset(res, df),
    "Some subsets contain rows with missing values"
  )
})

test_that("errors on invalid which argument", {
  df <- data.frame(A = 1:5, B = 6:10)
  res <- new("CorrCombo",
             subset_list = list(c("A", "B")),
             avg_corr    = 0.1,
             min_corr    = 0.05,
             max_corr    = 0.2,
             names       = c("A", "B"),
             threshold   = 0.5,
             forced_in   = character(),
             search_type = "els",
             n_rows_used = 5L)
  expect_error(
    corrSubset(res, df, which = "foo"),
    "`which` must be"
  )
  expect_error(
    corrSubset(res, df, which = 5),
    "out of bounds"
  )
})
