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
