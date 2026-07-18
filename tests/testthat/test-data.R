library(testthat)

# ===========================================================================
# Regression tests for the numeric claims documented (only as comments, never
# asserted) in R/data.R's @examples blocks for the bundled example datasets.
# A comment like "# Reduced from 19 to 12 variables" is not checked by R CMD
# check's example execution even if it drifts -- these tests turn the
# documented numbers into real assertions (#101).
# ===========================================================================

test_that("bioclim_example: corrPrune() reduces 19 predictors to 12 at threshold 0.7", {
  data(bioclim_example)
  pruned <- corrPrune(bioclim_example[, -1], threshold = 0.7)
  expect_equal(ncol(pruned), 12L)
})

test_that("genes_example: greedy corrPrune() reduces 200 genes to 177 at threshold 0.8", {
  data(genes_example)
  gene_data <- genes_example[, -(1:2)]
  pruned <- corrPrune(gene_data, threshold = 0.8, mode = "greedy")
  expect_equal(ncol(pruned), 177L)
})

test_that("survey_example: assocSelect()'s best subset keeps 25 of 34 items at threshold 0.8", {
  data(survey_example)
  pruned <- assocSelect(survey_example[, -1], threshold = 0.8, method_ord_ord = "spearman")
  expect_equal(length(pruned@subset_list[[1]]), 25L)
})

test_that("cor_example: MatSelect()'s largest maximal subset grows with threshold (12/16/20 of 20 vars)", {
  data(cor_example)
  expect_equal(dim(cor_example), c(20L, 20L))

  expected <- c("0.5" = 12L, "0.7" = 16L, "0.9" = 20L)
  for (th in c(0.5, 0.7, 0.9)) {
    res <- MatSelect(cor_example, threshold = th)
    sizes <- vapply(res@subset_list, length, integer(1))
    expect_equal(max(sizes), expected[[as.character(th)]])
  }
})
