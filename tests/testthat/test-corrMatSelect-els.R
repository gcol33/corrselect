library(testthat)

test_that("ELS returns valid CorrCombo object", {
  m <- diag(1, 3)
  m[1,2] <- m[2,1] <- 0.3
  res <- MatSelect(m, threshold = 0.5, method = "els")
  expect_s4_class(res, "CorrCombo")
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

test_that("ELS returns empty when no subset is valid", {
  m <- matrix(0.95, 3,3); diag(m) <- 1
  res <- MatSelect(m, threshold = 0.1, method = "els")
  expect_length(res@subset_list, 0)
  expect_equal(res@avg_corr, numeric(0))
  expect_equal(res@min_corr, numeric(0))
  expect_equal(res@max_corr, numeric(0))
})

test_that("ELS rejects NA matrices", {
  m <- diag(1,3)
  m[1,2] <- NA
  expect_error(MatSelect(m, threshold = 0.5, method = "els"),
               "`mat` must not contain NA\\.")
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





