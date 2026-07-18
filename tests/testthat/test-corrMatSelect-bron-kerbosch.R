library(testthat)

test_that("BK returns valid CorrCombo object", {
  m <- diag(1, 3)
  m[1,2] <- m[2,1] <- 0.3
  res <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch")
  expect_true(inherits(res, "CorrCombo"))
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

test_that("BK returns size-1 subsets when no multi-variable subset is valid", {
  m <- matrix(0.95, 3,3); diag(m) <- 1
  res <- MatSelect(m, threshold = 0.1, method = "bron-kerbosch")
  # No pair is compatible under so strict a threshold, so the only maximal
  # subsets are the three variables on their own (see #30).
  expect_length(res@subset_list, 3)
  expect_true(all(vapply(res@subset_list, length, integer(1)) == 1))
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

test_that("MatSelect warns about combinatorial blowup for a large, dense compatibility graph (#63)", {
  n <- 101
  m <- matrix(0.1, n, n)
  diag(m) <- 1
  colnames(m) <- rownames(m) <- paste0("V", seq_len(n))

  # All pairs are compatible (0.1 <= 0.5), so compat_density = 1 > 0.3 and
  # n > 100: the blowup guard must fire. The graph is a single clique here,
  # so this stays fast despite the large n.
  expect_warning(
    MatSelect(m, threshold = 0.5, method = "bron-kerbosch"),
    "exhaustive maximal-subset enumeration can be exponential"
  )
})

test_that("MatSelect does not warn about blowup for a large but sparse compatibility graph (#63)", {
  n <- 101
  m <- matrix(0.9, n, n)
  diag(m) <- 1
  colnames(m) <- rownames(m) <- paste0("V", seq_len(n))

  # No pair is compatible (0.9 > 0.3), so compat_density = 0 <= 0.3: the
  # blowup guard must not fire even though n > 100. The graph has no edges,
  # so this also stays fast.
  expect_no_warning(
    MatSelect(m, threshold = 0.3, method = "bron-kerbosch")
  )
})

test_that("BK's avg_corr/min_corr/max_corr match hand-computed values for a size-4 subset (#62)", {
  # A 4-variable subset with 6 distinct known pairwise values (C(4,2) = 6),
  # all below threshold, so a bug in the pairwise-index walk (wrong
  # triangle, off-by-one, transposed pair) would show up as a wrong
  # avg/min/max rather than being masked by repeated or symmetric-looking
  # values.
  m <- matrix(c(
    1,    0.10, 0.20, 0.30,
    0.10, 1,    0.15, 0.25,
    0.20, 0.15, 1,    0.05,
    0.30, 0.25, 0.05, 1
  ), nrow = 4, byrow = TRUE)
  colnames(m) <- rownames(m) <- c("A", "B", "C", "D")

  res <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch")

  expect_length(res@subset_list, 1)
  expect_setequal(res@subset_list[[1]], c("A", "B", "C", "D"))
  expect_equal(res@avg_corr[1], mean(c(0.10, 0.20, 0.30, 0.15, 0.25, 0.05)))
  expect_equal(res@min_corr[1], 0.05)
  expect_equal(res@max_corr[1], 0.30)
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

  # Should warn but still proceed with user's explicit force_in request
  expect_warning({
    res <- MatSelect(mat, threshold = 0.7, force_in = 1:2)
  }, "mutually correlated beyond the threshold")

  expect_true(inherits(res, "CorrCombo"))
  all_sets <- unique(unlist(res@subset_list))
  expect_true(all(c("A", "A_dup") %in% all_sets))
})

test_that("MatSelect's force_in mutual-violation warning names the offending pair and value (#98)", {
  mat <- diag(1, 3)
  mat[1, 2] <- mat[2, 1] <- 0.99
  colnames(mat) <- c("A", "A_dup", "B")

  expect_warning(
    MatSelect(mat, threshold = 0.7, force_in = 1:2),
    "'A' and 'A_dup' have association 0.990 > 0.700"
  )
})

test_that("MatSelect(): use_pivot errors on non-coercible input instead of silently falling back (#99)", {
  m <- diag(1, 3)
  m[1, 2] <- m[2, 1] <- 0.3

  expect_error(
    MatSelect(m, threshold = 0.5, method = "bron-kerbosch", use_pivot = "typo_value"),
    "use_pivot.*must be a single TRUE/FALSE"
  )
  expect_error(
    MatSelect(m, threshold = 0.5, method = "bron-kerbosch", use_pivot = c(TRUE, FALSE)),
    "use_pivot.*must be a single TRUE/FALSE"
  )
})

test_that("MatSelect(): use_pivot warns that it has no effect when method = \"els\" (#100)", {
  m <- diag(1, 3)
  m[1, 2] <- m[2, 1] <- 0.3

  expect_warning(
    MatSelect(m, threshold = 0.5, method = "els", use_pivot = FALSE),
    "use_pivot.*no effect when method"
  )
  expect_no_warning(
    MatSelect(m, threshold = 0.5, method = "bron-kerbosch", use_pivot = FALSE)
  )
})


# ===========================================================================
# Additional coverage tests for Bron-Kerbosch algorithm
# ===========================================================================

test_that("BK handles matrix without column names", {
  m <- diag(1, 3)
  m[1, 2] <- m[2, 1] <- 0.3

  res <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch")

  expect_true(inherits(res, "CorrCombo"))
  # Should use default names V1, V2, V3
  expect_true(all(grepl("^V", unlist(res@subset_list))))
})

test_that("BK handles force_in as character names", {
  m <- diag(1, 4)
  colnames(m) <- c("apple", "banana", "cherry", "date")

  res <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch",
                   force_in = c("apple", "cherry"))

  expect_true(inherits(res, "CorrCombo"))
  for (s in res@subset_list) {
    expect_true(all(c("apple", "cherry") %in% s))
  }
})

test_that("BK errors on character force_in with unnamed matrix", {
  m <- diag(1, 3)
  # No column names

  expect_error(
    MatSelect(m, threshold = 0.5, method = "bron-kerbosch",
              force_in = c("A", "B")),
    "no column names"
  )
})

test_that("BK errors on force_in names not in matrix", {
  m <- diag(1, 3)
  colnames(m) <- c("A", "B", "C")

  expect_error(
    MatSelect(m, threshold = 0.5, method = "bron-kerbosch",
              force_in = c("A", "X")),
    "not found in matrix"
  )
})

test_that("BK handles non-square matrix error", {
  m <- matrix(1, nrow = 2, ncol = 3)

  expect_error(
    MatSelect(m, threshold = 0.5, method = "bron-kerbosch"),
    "square"
  )
})

test_that("BK handles non-unit diagonal error", {
  m <- diag(0.9, 3)

  expect_error(
    MatSelect(m, threshold = 0.5, method = "bron-kerbosch"),
    "Diagonal entries.*must be 1"
  )
})

test_that("BK handles invalid threshold (0)", {
  m <- diag(1, 3)

  expect_error(
    MatSelect(m, threshold = 0, method = "bron-kerbosch"),
    "threshold.*must be in the range"
  )
})

test_that("BK handles threshold = 1 (all valid)", {
  m <- diag(1, 3)
  m[1, 2] <- m[2, 1] <- 0.99
  m[1, 3] <- m[3, 1] <- 0.99
  m[2, 3] <- m[3, 2] <- 0.99
  colnames(m) <- c("A", "B", "C")

  # With threshold = 1, all pairs are valid
  res <- MatSelect(m, threshold = 1, method = "bron-kerbosch")

  expect_true(inherits(res, "CorrCombo"))
  expect_equal(length(res@subset_list), 1)
  expect_setequal(res@subset_list[[1]], c("A", "B", "C"))
})

test_that("BK handles invalid threshold (> 1)", {
  m <- diag(1, 3)

  expect_error(
    MatSelect(m, threshold = 1.5, method = "bron-kerbosch"),
    "threshold.*must be in the range"
  )
})

test_that("BK handles non-numeric matrix", {
  m <- matrix("a", nrow = 3, ncol = 3)

  expect_error(
    MatSelect(m, threshold = 0.5, method = "bron-kerbosch"),
    "numeric matrix"
  )
})

test_that("BK stores use_pivot attribute correctly", {
  m <- diag(1, 3)

  res1 <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch", use_pivot = TRUE)
  res2 <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch", use_pivot = FALSE)

  expect_true(attr(res1, "use_pivot"))
  expect_false(attr(res2, "use_pivot"))
})

test_that("BK default pivot is TRUE", {
  m <- diag(1, 3)

  res <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch")

  expect_true(attr(res, "use_pivot"))
})

test_that("BK handles use_pivot as various truthy values", {
  m <- diag(1, 3)

  # use_pivot = 1 should work
  res <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch", use_pivot = 1)
  expect_true(attr(res, "use_pivot"))

  # use_pivot = 0 should work
  res <- MatSelect(m, threshold = 0.5, method = "bron-kerbosch", use_pivot = 0)
  expect_false(attr(res, "use_pivot"))
})


test_that("MatSelect handles NULL force_in with method bron-kerbosch", {
  mat <- matrix(c(1, 0.2, 0.2, 1), 2, 2)
  colnames(mat) <- rownames(mat) <- c("a", "b")

  result <- MatSelect(mat, threshold = 0.5, method = "bron-kerbosch", force_in = NULL)
  expect_true(inherits(result, "CorrCombo"))
})

test_that("MatSelect returns size-1 maximal subsets when all variables are mutually correlated (#30)", {
  # Regression test for issue #30: MatSelect() used to unconditionally drop
  # every size-1 maximal subset, so when no pair of variables is compatible
  # under the threshold, it returned zero subsets instead of reporting each
  # variable as its own (trivially valid) maximal subset.
  mat <- matrix(0.999, 3, 3)
  diag(mat) <- 1
  colnames(mat) <- rownames(mat) <- c("A", "B", "C")

  result <- MatSelect(mat, threshold = 0.5)

  expect_equal(length(result@subset_list), 3)
  expect_setequal(vapply(result@subset_list, identity, character(1)), c("A", "B", "C"))
  expect_true(all(is.na(result@min_corr)))
  expect_true(all(is.na(result@max_corr)))
})

test_that("MatSelect keeps a size-1 force_in subset when all variables are mutually correlated (#30)", {
  mat <- matrix(0.999, 3, 3)
  diag(mat) <- 1
  colnames(mat) <- rownames(mat) <- c("A", "B", "C")

  result <- MatSelect(mat, threshold = 0.5, force_in = "A")

  expect_equal(length(result@subset_list), 1)
  expect_equal(result@subset_list[[1]], "A")
})
