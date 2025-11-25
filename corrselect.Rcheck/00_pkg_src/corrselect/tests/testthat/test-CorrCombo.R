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
