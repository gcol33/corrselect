library(testthat)

test_that("BK returns valid CorrCombo object", {
  m <- diag(1, 3)
  m[1,2] <- m[2,1] <- 0.3
  res <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch")
  expect_s4_class(res, "CorrCombo")
  expect_type(res@subset_list, "list")
})

test_that("BK respects correlation threshold", {
  m <- diag(1, 4)
  m[1,2] <- m[2,1] <- 0.8
  m[3,4] <- m[4,3] <- 0.1
  res <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch")
  bad <- sapply(res@subset_list, function(s) all(c("V1","V2") %in% s))
  expect_false(any(bad))
})

test_that("BK includes forced variables", {
  m <- diag(1, 4)
  m[1,2] <- m[2,1] <- 0.4
  colnames(m) <- paste0("X", 1:4)
  res <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch", force_in = c(1,4))
  ok <- vapply(res@subset_list, function(s) all(c("X1","X4") %in% s), logical(1))
  expect_true(all(ok))
})

test_that("BK returns empty when no subset is valid", {
  m <- matrix(0.95, 3,3); diag(m) <- 1
  res <- MatSelect(m, threshold = 0.1, method = "bron-kerbosch")
  expect_length(res@subset_list, 0)
})

test_that("BK rejects NA matrices", {
  m <- diag(1,3)
  m[1,2] <- NA
  expect_error(MatSelect(m, threshold = 0.5, method = "bron-kerbosch"),
               "`mat` must not contain NA\\.")
})

test_that("BK returns correct variable names", {
  m <- diag(1,3)
  colnames(m) <- c("a","b","c")
  res <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch")
  all_vars <- unlist(res@subset_list, use.names = FALSE)
  expect_true(all(all_vars %in% c("a","b","c")))
})

test_that("BK computes min/max correlations", {
  m <- diag(1,4)
  m[1,2] <- m[2,1] <- 0.3
  res <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch")
  expect_length(res@min_corr, length(res@subset_list))
  expect_true(all(res@max_corr >= res@min_corr, na.rm = TRUE))
})

test_that("BK pivot behavior can be toggled", {
  m <- matrix(c(1,0.2,0.8,
                0.2,1,0.3,
                0.8,0.3,1), 3,3)
  # default uses pivot
  r1 <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch")
  expect_true(attr(r1, "use_pivot"))
  # explicit use_pivot = FALSE
  r2 <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch", use_pivot = FALSE)
  expect_false(attr(r2, "use_pivot"))
})

test_that("BK and ELS agree on small example", {
  m <- matrix(c(1,0.2,0.8,
                0.2,1,0.3,
                0.8,0.3,1), 3,3)
  r_els <- MatSelect(m, threshold = 0.5, method = "els")
  r_bk  <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch", use_pivot = FALSE)
  # compare sets (unordered)
  s_els <- lapply(r_els@subset_list, sort)
  s_bk  <- lapply(r_bk@subset_list,  sort)
  expect_setequal(s_els, s_bk)
})

test_that("MatSelect allows too-correlated force_in with warning handled in R", {
  mat <- diag(1, 3)
  mat[1, 2] <- mat[2, 1] <- 0.99  # A and A_dup are too correlated
  colnames(mat) <- c("A", "A_dup", "B")

  expect_silent({
    res <- MatSelect(mat, threshold = 0.7, force_in = 1:2)
  })

  expect_s4_class(res, "CorrCombo")
  all_sets <- unique(unlist(res@subset_list))
  expect_true(all(c("A", "A_dup") %in% all_sets))
})

