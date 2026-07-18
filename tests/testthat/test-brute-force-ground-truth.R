# Ground-truth verification for the combinatorial core (#49): independently
# enumerate all maximal subsets by brute force (exponential, so only used at
# small n) and check that every exact backend (ELS, Bron-Kerbosch with and
# without pivoting) agrees with it exactly -- this validates maximality and
# exhaustiveness simultaneously, and is the strongest correctness signal
# available for this kind of package: two independently implemented exact
# enumerators agreeing on identical random input.

.random_symmetric_matrix <- function(n) {
  m <- diag(1, n)
  m[upper.tri(m)] <- stats::runif(choose(n, 2), -1, 1)
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  colnames(m) <- rownames(m) <- paste0("V", seq_len(n))
  m
}

# All maximal subsets whose pairwise |value| <= threshold, optionally
# restricted to subsets containing every index in `force_in`. Exponential in
# n via combn() -- only used at small n (<= 9) in the tests below.
.brute_force_maximal_subsets <- function(mat, threshold, force_in = integer(0)) {
  n <- ncol(mat)
  idx <- seq_len(n)

  is_valid <- function(combo) {
    if (length(combo) < 2) return(TRUE)
    sub <- abs(mat[combo, combo, drop = FALSE])
    all(sub[upper.tri(sub)] <= threshold)
  }

  valid_sets <- list()
  for (k in seq_len(n)) {
    if (k < length(force_in)) next
    for (cb in combn(idx, k, simplify = FALSE)) {
      if (length(force_in) && !all(force_in %in% cb)) next
      if (is_valid(cb)) valid_sets[[length(valid_sets) + 1L]] <- cb
    }
  }

  keep <- rep(TRUE, length(valid_sets))
  for (i in seq_along(valid_sets)) {
    for (j in seq_along(valid_sets)) {
      if (i == j || !keep[i]) next
      if (length(valid_sets[[j]]) > length(valid_sets[[i]]) &&
          all(valid_sets[[i]] %in% valid_sets[[j]])) {
        keep[i] <- FALSE
        break
      }
    }
  }
  valid_sets[keep]
}

.combo_keys <- function(sets) {
  sort(vapply(sets, function(s) paste(sort(s), collapse = ","), character(1)))
}

.result_keys <- function(res, varnames) {
  sort(vapply(res@subset_list, function(s) paste(sort(match(s, varnames)), collapse = ","), character(1)))
}

test_that("ELS and Bron-Kerbosch (pivot/no-pivot) exactly match brute-force ground truth", {
  n_seeds <- 40
  for (seed in seq_len(n_seeds)) {
    set.seed(10000 + seed)
    n <- sample(5:9, 1)
    mat <- .random_symmetric_matrix(n)
    threshold <- stats::runif(1, 0.2, 0.8)

    truth <- .combo_keys(.brute_force_maximal_subsets(mat, threshold))

    res_bk_pivot   <- MatSelect(mat, threshold, method = "bron-kerbosch", use_pivot = TRUE)
    res_bk_nopivot <- MatSelect(mat, threshold, method = "bron-kerbosch", use_pivot = FALSE)
    res_els        <- MatSelect(mat, threshold, method = "els")

    expect_identical(.result_keys(res_bk_pivot, colnames(mat)), truth,
                      info = sprintf("seed=%d n=%d threshold=%.3f: Bron-Kerbosch (pivot) vs brute force", seed, n, threshold))
    expect_identical(.result_keys(res_bk_nopivot, colnames(mat)), truth,
                      info = sprintf("seed=%d n=%d threshold=%.3f: Bron-Kerbosch (no pivot) vs brute force", seed, n, threshold))
    expect_identical(.result_keys(res_els, colnames(mat)), truth,
                      info = sprintf("seed=%d n=%d threshold=%.3f: ELS vs brute force", seed, n, threshold))
  }
})

test_that("ELS and Bron-Kerbosch exactly match brute-force ground truth under force_in", {
  n_seeds <- 25
  run <- 0L
  seed <- 0L
  while (run < n_seeds) {
    seed <- seed + 1L
    set.seed(20000 + seed)
    n <- sample(5:8, 1)
    mat <- .random_symmetric_matrix(n)
    threshold <- stats::runif(1, 0.3, 0.8)
    forced <- sort(sample(seq_len(n), sample(1:2, 1)))

    # A force_in set whose own members violate the threshold isn't a case
    # brute-force "valid maximal subset" semantics apply to (MatSelect()
    # forces such variables in regardless, by design, with a warning) --
    # only test force_in combinations that are themselves compatible.
    if (length(forced) > 1) {
      sub <- abs(mat[forced, forced, drop = FALSE])
      if (any(sub[upper.tri(sub)] > threshold)) next
    }
    run <- run + 1L

    truth <- .combo_keys(.brute_force_maximal_subsets(mat, threshold, force_in = forced))

    res_bk  <- MatSelect(mat, threshold, method = "bron-kerbosch", force_in = forced)
    res_els <- MatSelect(mat, threshold, method = "els", force_in = forced)

    expect_identical(.result_keys(res_bk, colnames(mat)), truth,
                      info = sprintf("seed=%d n=%d force_in=%s: Bron-Kerbosch vs brute force", seed, n, paste(forced, collapse = ",")))
    expect_identical(.result_keys(res_els, colnames(mat)), truth,
                      info = sprintf("seed=%d n=%d force_in=%s: ELS vs brute force", seed, n, paste(forced, collapse = ",")))
  }
})
