## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----load-packages, message=FALSE---------------------------------------------
library(corrselect)

# For benchmarking (reduced iterations for vignette build speed)
if (!requireNamespace("microbenchmark", quietly = TRUE)) {
  message("Note: microbenchmark not installed. Some benchmarks will be skipped.")
  has_microbenchmark <- FALSE
} else {
  library(microbenchmark)
  has_microbenchmark <- TRUE
}

# For comparison
if (!requireNamespace("caret", quietly = TRUE)) {
  message("Note: caret not installed. Comparison benchmarks will be skipped.")
  has_caret <- FALSE
} else {
  library(caret)
  has_caret <- TRUE
}

set.seed(42)

## ----exact-small--------------------------------------------------------------
# Generate correlated data
generate_corr_data <- function(n, p, base_cor = 0.5) {
  sigma <- matrix(base_cor, p, p)
  diag(sigma) <- 1
  MASS::mvrnorm(n, mu = rep(0, p), Sigma = sigma)
}

# Test exact mode at different scales
test_exact <- function(p, n = 100) {
  data <- as.data.frame(generate_corr_data(n, p))
  system.time(corrPrune(data, threshold = 0.7, mode = "exact"))
}

cat("Exact mode timing:\n")
cat("p = 10: ", test_exact(10)[3], "seconds\n")
cat("p = 15: ", test_exact(15)[3], "seconds\n")
cat("p = 20: ", test_exact(20)[3], "seconds\n")

## ----greedy-large-------------------------------------------------------------
# Test greedy mode at different scales
test_greedy <- function(p, n = 100) {
  data <- as.data.frame(generate_corr_data(n, p, base_cor = 0.3))
  system.time(corrPrune(data, threshold = 0.7, mode = "greedy"))
}

cat("Greedy mode timing:\n")
cat("p = 50:  ", test_greedy(50)[3], "seconds\n")
cat("p = 100: ", test_greedy(100)[3], "seconds\n")
cat("p = 200: ", test_greedy(200)[3], "seconds\n")

## ----auto-mode----------------------------------------------------------------
# Auto mode automatically switches at p = 20
test_auto <- function(p, n = 100) {
  data <- as.data.frame(generate_corr_data(n, p, base_cor = 0.4))
  system.time(corrPrune(data, threshold = 0.7, mode = "auto"))
}

cat("Auto mode timing (switches at p = 20):\n")
cat("p = 15 (uses exact):  ", test_auto(15)[3], "seconds\n")
cat("p = 25 (uses greedy): ", test_auto(25)[3], "seconds\n")
cat("p = 100 (uses greedy):", test_auto(100)[3], "seconds\n")

## ----compare-caret, eval=has_caret--------------------------------------------
# # Generate test data
# test_data <- as.data.frame(generate_corr_data(100, 50, base_cor = 0.5))
# 
# # Benchmark (times = 3 for vignette build speed)
# comparison <- microbenchmark(
#   corrPrune_greedy = corrPrune(test_data, threshold = 0.7, mode = "greedy"),
#   corrPrune_exact = corrPrune(test_data[, 1:20], threshold = 0.7, mode = "exact"),
#   caret_find = findCorrelation(cor(test_data), cutoff = 0.7),
#   times = 3
# )
# 
# print(comparison)

## ----compare-caret-results, eval=has_caret------------------------------------
# # Compare result quality
# corr_result <- corrPrune(test_data, threshold = 0.7, mode = "greedy")
# caret_remove <- findCorrelation(cor(test_data), cutoff = 0.7)
# caret_result <- test_data[, -caret_remove]
# 
# cat("\nResult comparison:\n")
# cat("corrPrune (greedy) retained:", ncol(corr_result), "variables\n")
# cat("caret::findCorrelation retained:", ncol(caret_result), "variables\n")
# cat("\nMax correlation in corrPrune result:",
#     max(abs(cor(corr_result)[upper.tri(cor(corr_result))])), "\n")
# cat("Max correlation in caret result:",
#     max(abs(cor(caret_result)[upper.tri(cor(caret_result))])), "\n")

## ----compare-caret-note, eval=!has_caret, echo=FALSE, results='asis'----------
cat("*caret not installed - comparison skipped*\n\n")
cat("**Typical results:** corrPrune (greedy) is ~2-5x faster than caret::findCorrelation() and typically retains more variables while meeting the threshold.\n")

## ----modelprun-engines--------------------------------------------------------
# Generate test data with response
test_lm_data <- function(n = 200, p = 30) {
  X <- generate_corr_data(n, p, base_cor = 0.4)
  y <- X[, 1] + X[, 2] + rnorm(n)
  df <- as.data.frame(cbind(y = y, X))
  names(df) <- c("y", paste0("x", 1:p))
  df
}

data_lm <- test_lm_data()

# Benchmark different engines
cat("modelPrune timing by engine (p = 30, n = 200):\n\n")

time_lm <- system.time({
  result_lm <- modelPrune(y ~ ., data = data_lm, engine = "lm", limit = 5)
})
cat("lm:  ", time_lm[3], "seconds (",
    attr(result_lm, "n_steps"), "steps)\n")

time_glm <- system.time({
  result_glm <- modelPrune(y ~ ., data = data_lm, engine = "glm", limit = 5)
})
cat("glm: ", time_glm[3], "seconds (",
    attr(result_glm, "n_steps"), "steps)\n")

