# ===========================================================================
# Reference-verified tests for the association dispatch tables (#38).
#
# assocSelect()'s get_assoc() and corrPrune()'s .compute_single_assoc_matrix()
# are near-identical, independently-maintained switch() blocks implementing
# the same eta-squared / Cramer's V logic (see the corrPrune constant-factor
# divergence fixed in #33). Neither dispatch table is reachable from outside
# its enclosing function (both are local closures, not package-namespace
# functions), so values are recovered indirectly:
#
#   - assocSelect(): with exactly two columns and threshold = 1 (eta and
#     Cramer's V are both bounded in [0, 1], so the pair is always
#     compatible), the single maximal subset's avg_corr is exactly the
#     pairwise association value.
#   - corrPrune(): has no equivalent readout, so its internal value is
#     instead verified by checking that mode = "greedy" crosses the
#     kept-vs-removed boundary at the same reference value assocSelect()
#     reports (threshold just below -> one variable dropped; threshold just
#     above -> both kept).
#
# Reference values are computed independently in each test via the textbook
# eta-squared formula (sum-of-squares decomposition) or stats::chisq.test()
# (an independently-tested base-R implementation of the chi-square
# statistic), not by re-calling corrselect's own dispatch code.
# ===========================================================================

test_that("eta-squared matches a hand-computed sum-of-squares reference", {
  num <- c(1, 2, 3, 7, 8, 9)
  cat <- factor(c("A", "A", "A", "B", "B", "B"))
  df  <- data.frame(num = num, cat = cat)

  ss_tot <- sum((num - mean(num))^2)
  ss_bet <- sum(tapply(num, cat, function(z) length(z) * (mean(z) - mean(num))^2))
  eta_ref <- sqrt(ss_bet / ss_tot)

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_setequal(res@subset_list[[1]], c("num", "cat"))
  expect_equal(res@avg_corr[1], eta_ref, tolerance = 1e-8)

  below <- corrPrune(df, threshold = eta_ref - 0.02, mode = "greedy")
  above <- corrPrune(df, threshold = eta_ref + 0.02, mode = "greedy")
  expect_equal(ncol(below), 1)
  expect_equal(ncol(above), 2)
})

test_that("Cramer's V matches a stats::chisq.test()-derived reference", {
  x <- factor(c(rep("A", 20), rep("B", 20)))
  y <- factor(c(rep("P", 15), rep("Q", 5), rep("P", 5), rep("Q", 15)))
  df <- data.frame(x = x, y = y)

  tbl <- table(x, y)
  chi2 <- chisq.test(tbl, correct = FALSE)$statistic
  v_ref <- sqrt(as.numeric(chi2) / (sum(tbl) * (min(dim(tbl)) - 1)))
  expect_equal(v_ref, 0.5, tolerance = 1e-8)  # sanity-check the reference itself

  res <- assocSelect(df, threshold = 1)
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], v_ref, tolerance = 1e-8)

  below <- corrPrune(df, threshold = v_ref - 0.05, mode = "greedy")
  above <- corrPrune(df, threshold = v_ref + 0.05, mode = "greedy")
  expect_equal(ncol(below), 1)
  expect_equal(ncol(above), 2)
})

test_that("a constant numeric column has exactly zero association with a factor, in both functions", {
  set.seed(9380)
  df <- data.frame(num = rep(5, 20), cat = factor(sample(c("A", "B", "C"), 20, replace = TRUE)))

  res <- assocSelect(df, threshold = 1)
  expect_equal(res@avg_corr[1], 0)

  # A zero association means both variables are always kept, at any
  # (positive) threshold.
  pruned <- corrPrune(df, threshold = 0.01, mode = "greedy")
  expect_equal(ncol(pruned), 2)
})

test_that("a single-level factor has exactly zero eta association with a numeric variable, in both functions (#65)", {
  # Consolidates ~6 near-duplicate single-level-factor tests that were
  # scattered across test-assocSelect.R, each only asserting
  # inherits(res, "CorrCombo") -- never that the eta value itself is
  # correctly 0 rather than NaN (0/0, since a single-level factor makes the
  # eta formula's between-group sum of squares identically 0).
  set.seed(9384)
  df <- data.frame(num = rnorm(20), cat = factor(rep("only_level", 20)))

  res <- assocSelect(df, threshold = 1)
  expect_equal(res@avg_corr[1], 0)

  pruned <- corrPrune(df, threshold = 0.01, mode = "greedy")
  expect_equal(ncol(pruned), 2)
})

test_that("a near-constant numeric column's eta matches the reference, checked in both functions", {
  near_const <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5.5)
  cat <- factor(c(rep("A", 5), rep("B", 5)))
  df <- data.frame(num = near_const, cat = cat)

  ss_tot <- sum((near_const - mean(near_const))^2)
  ss_bet <- sum(tapply(near_const, cat, function(z) length(z) * (mean(z) - mean(near_const))^2))
  eta_ref <- sqrt(ss_bet / ss_tot)

  res <- assocSelect(df, threshold = 1)
  expect_equal(res@avg_corr[1], eta_ref, tolerance = 1e-8)

  below <- corrPrune(df, threshold = eta_ref - 0.05, mode = "greedy")
  above <- corrPrune(df, threshold = eta_ref + 0.05, mode = "greedy")
  expect_equal(ncol(below), 1)
  expect_equal(ncol(above), 2)
})

test_that("a constant numeric column gets exactly zero bicor association via corrPrune, not NA (#64)", {
  skip_if_not(requireNamespace("WGCNA", quietly = TRUE))
  # corrSelect() pre-filters constant columns before ever computing a
  # correlation matrix (see corrSelect.R's own is_const exclusion step), so
  # it never actually reaches .numeric_assoc_matrix()'s constant-column
  # zero-out override -- only corrPrune()'s all-numeric branch does (it has
  # no equivalent pre-filter). If the override didn't fire, WGCNA::bicor()
  # would return NA/NaN for the constant pair, which corrPrune()'s Step 4b
  # rejects with an explicit "undefined (NA) values" error -- so the
  # override firing is exactly what makes this call succeed at all.
  set.seed(9381)
  df <- data.frame(const = rep(3, 20), x = rnorm(20))

  pruned <- corrPrune(df, threshold = 0.01, mode = "greedy", measure = "bicor")
  expect_equal(sort(colnames(pruned)), c("const", "x"))
})

test_that("a constant numeric column gets exactly zero distance-correlation association via corrPrune, not NA (#64)", {
  skip_if_not(requireNamespace("energy", quietly = TRUE))
  set.seed(9382)
  df <- data.frame(const = rep(3, 20), x = rnorm(20))

  pruned <- corrPrune(df, threshold = 0.01, mode = "greedy", measure = "distance")
  expect_equal(sort(colnames(pruned)), c("const", "x"))
})

test_that("a constant numeric column gets exactly zero maximal-information-coefficient association via corrPrune, not NA (#64)", {
  skip_if_not(requireNamespace("minerva", quietly = TRUE))
  set.seed(9383)
  df <- data.frame(const = rep(3, 20), x = rnorm(20))

  pruned <- corrPrune(df, threshold = 0.01, mode = "greedy", measure = "maximal")
  expect_equal(sort(colnames(pruned)), c("const", "x"))
})

# ===========================================================================
# Reference-verified tests for the six numeric correlation methods (#83).
#
# Unlike eta-squared/Cramer's V above, these six methods delegate their
# per-pair computation to an already-independently-tested implementation
# (stats::cor() for pearson/spearman/kendall; WGCNA::bicor(), energy::dcor(),
# minerva::mine() for the rest) -- so the risk here isn't the underlying
# formula being wrong, it's the *wiring*: a wrong method selected for a pair,
# or a value landing in the wrong matrix cell. The same "two columns,
# threshold = 1, avg_corr is exactly the pairwise value" readout used above
# for eta/Cramer's V catches that, cross-checked against a hand-derived (not
# corrselect-internal) reference for each method's textbook formula.
# ===========================================================================

test_that("pearson matches a hand-computed reference, checked via corrSelect and corrPrune", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  y <- c(2, 1, 4, 3, 6, 5, 9, 7)
  df <- data.frame(x = x, y = y)

  pearson_ref <- abs(sum((x - mean(x)) * (y - mean(y))) /
    sqrt(sum((x - mean(x))^2) * sum((y - mean(y))^2)))

  res <- corrSelect(df, threshold = 1, cor_method = "pearson")
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], pearson_ref, tolerance = 1e-8)

  below <- corrPrune(df, threshold = pearson_ref - 0.05, mode = "greedy", measure = "pearson")
  above <- corrPrune(df, threshold = pearson_ref + 0.05, mode = "greedy", measure = "pearson")
  expect_equal(ncol(below), 1)
  expect_equal(ncol(above), 2)
})

test_that("spearman matches a hand-computed rank-correlation reference, checked via corrSelect and corrPrune", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  y <- c(2, 1, 4, 3, 6, 5, 9, 7)
  df <- data.frame(x = x, y = y)

  rx <- rank(x); ry <- rank(y)
  spearman_ref <- abs(sum((rx - mean(rx)) * (ry - mean(ry))) /
    sqrt(sum((rx - mean(rx))^2) * sum((ry - mean(ry))^2)))

  res <- corrSelect(df, threshold = 1, cor_method = "spearman")
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], spearman_ref, tolerance = 1e-8)

  below <- corrPrune(df, threshold = spearman_ref - 0.05, mode = "greedy", measure = "spearman")
  above <- corrPrune(df, threshold = spearman_ref + 0.05, mode = "greedy", measure = "spearman")
  expect_equal(ncol(below), 1)
  expect_equal(ncol(above), 2)
})

test_that("kendall matches a hand-computed concordant-pair reference, checked via corrSelect and corrPrune", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  y <- c(2, 1, 4, 3, 6, 5, 9, 7)
  df <- data.frame(x = x, y = y)

  n <- length(x)
  conc <- 0L; disc <- 0L
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      s <- sign(x[i] - x[j]) * sign(y[i] - y[j])
      if (s > 0) conc <- conc + 1L
      if (s < 0) disc <- disc + 1L
    }
  }
  kendall_ref <- abs((conc - disc) / (n * (n - 1) / 2))

  res <- corrSelect(df, threshold = 1, cor_method = "kendall")
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], kendall_ref, tolerance = 1e-8)

  below <- corrPrune(df, threshold = kendall_ref - 0.05, mode = "greedy", measure = "kendall")
  above <- corrPrune(df, threshold = kendall_ref + 0.05, mode = "greedy", measure = "kendall")
  expect_equal(ncol(below), 1)
  expect_equal(ncol(above), 2)
})

test_that("bicor matches a hand-computed biweight midcorrelation reference, checked via corrSelect and corrPrune", {
  skip_if_not(requireNamespace("WGCNA", quietly = TRUE))

  set.seed(9385)
  x <- rnorm(30)
  y <- 0.6 * x + rnorm(30, sd = 0.8)
  df <- data.frame(x = x, y = y)

  # Textbook biweight midcorrelation (Wilcox 2012, Eq. as used by WGCNA::bicor
  # with default maxPOutliers = 1, i.e. no outlier-proportion capping):
  # weight each observation by a Tukey biweight based on its distance (in
  # MAD units) from the median, then correlate the weighted deviations.
  bicor_weighted <- function(v) {
    med <- median(v)
    mad_v <- mad(v)  # median absolute deviation, consistency-corrected
    u <- (v - med) / (9 * mad_v)
    w <- (1 - u^2)^2
    w[abs(u) >= 1] <- 0
    (v - med) * w
  }
  wx <- bicor_weighted(x)
  wy <- bicor_weighted(y)
  bicor_ref <- abs(sum(wx * wy) / sqrt(sum(wx^2) * sum(wy^2)))

  res <- corrSelect(df, threshold = 1, cor_method = "bicor")
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], bicor_ref, tolerance = 1e-6)

  below <- corrPrune(df, threshold = bicor_ref - 0.05, mode = "greedy", measure = "bicor")
  above <- corrPrune(df, threshold = bicor_ref + 0.05, mode = "greedy", measure = "bicor")
  expect_equal(ncol(below), 1)
  expect_equal(ncol(above), 2)
})

test_that("distance correlation matches a hand-computed reference, checked via corrSelect and corrPrune", {
  skip_if_not(requireNamespace("energy", quietly = TRUE))

  set.seed(9386)
  x <- rnorm(15)
  y <- 0.5 * x + rnorm(15, sd = 0.9)
  df <- data.frame(x = x, y = y)

  # Textbook distance correlation (Szekely, Rizzo & Bakirov 2007): doubly
  # centered pairwise-distance matrices, then dCor = dCov(x,y) / sqrt(dVar(x)*dVar(y)).
  dcor_ref_fn <- function(a, b) {
    n <- length(a)
    A <- as.matrix(dist(a)); B <- as.matrix(dist(b))
    center <- function(M) {
      rm_ <- rowMeans(M); cm_ <- colMeans(M); gm_ <- mean(M)
      sweep(sweep(M, 1, rm_), 2, cm_) + gm_
    }
    Ac <- center(A); Bc <- center(B)
    dcov2 <- sum(Ac * Bc) / n^2
    dvarx2 <- sum(Ac * Ac) / n^2
    dvary2 <- sum(Bc * Bc) / n^2
    sqrt(dcov2 / sqrt(dvarx2 * dvary2))
  }
  dcor_ref <- dcor_ref_fn(x, y)

  res <- corrSelect(df, threshold = 1, cor_method = "distance")
  expect_equal(length(res@subset_list), 1)
  expect_equal(res@avg_corr[1], dcor_ref, tolerance = 1e-6)

  below <- corrPrune(df, threshold = dcor_ref - 0.05, mode = "greedy", measure = "distance")
  above <- corrPrune(df, threshold = dcor_ref + 0.05, mode = "greedy", measure = "distance")
  expect_equal(ncol(below), 1)
  expect_equal(ncol(above), 2)
})

test_that("maximal information coefficient reports near-extremal values for near-deterministic/independent pairs, checked via corrSelect and corrPrune", {
  skip_if_not(requireNamespace("minerva", quietly = TRUE))

  # MIC has no simple closed-form reference (it is itself a data-driven
  # optimal-grid search), so unlike the other five methods this cannot be
  # checked against a hand-derived exact value. Instead this checks two
  # analytically-known bounds -- a near-perfectly deterministic relationship
  # must score close to its upper bound (1), and an independent pair must
  # score close to its lower bound (0) -- which still catches a wiring bug
  # (wrong method dispatched, or a value landing in the wrong matrix cell).
  set.seed(9387)
  n <- 60
  x <- seq_len(n)
  y_det <- x + rnorm(n, sd = 0.01)  # essentially deterministic, monotonic
  df_det <- data.frame(x = x, y = y_det)

  res_det <- corrSelect(df_det, threshold = 1, cor_method = "maximal")
  expect_equal(length(res_det@subset_list), 1)
  expect_gt(res_det@avg_corr[1], 0.9)

  x_ind <- rnorm(n)
  y_ind <- rnorm(n)
  df_ind <- data.frame(x = x_ind, y = y_ind)

  res_ind <- corrSelect(df_ind, threshold = 1, cor_method = "maximal")
  expect_equal(length(res_ind@subset_list), 1)
  expect_lt(res_ind@avg_corr[1], 0.5)
})

test_that("a sparse (but valid) contingency table's Cramer's V matches the reference, checked in both functions", {
  x <- factor(c(rep("A", 10), rep("B", 10), rep("C", 10)))
  y <- factor(c(rep("P", 9), rep("Q", 1),   # group A: 9 P, 1 Q
                rep("Q", 9), rep("P", 1),   # group B: 1 P, 9 Q
                rep("P", 5), rep("Q", 5)))  # group C: 5 P, 5 Q
  df <- data.frame(x = x, y = y)

  tbl <- table(x, y)
  expect_true(all(tbl > 0))  # sparse, but no fully-empty row/column
  chi2 <- chisq.test(tbl, correct = FALSE)$statistic
  v_ref <- sqrt(as.numeric(chi2) / (sum(tbl) * (min(dim(tbl)) - 1)))

  res <- assocSelect(df, threshold = 1)
  expect_equal(res@avg_corr[1], v_ref, tolerance = 1e-8)

  below <- corrPrune(df, threshold = v_ref - 0.05, mode = "greedy")
  above <- corrPrune(df, threshold = v_ref + 0.05, mode = "greedy")
  expect_equal(ncol(below), 1)
  expect_equal(ncol(above), 2)
})
