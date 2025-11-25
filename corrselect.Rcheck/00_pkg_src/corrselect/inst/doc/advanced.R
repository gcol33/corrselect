## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(corrselect)
data(mtcars)

# Exact mode: guaranteed optimal
exact_result <- corrPrune(mtcars, threshold = 0.7, mode = "exact")
cat("Exact mode kept:", ncol(exact_result), "variables\n")

## -----------------------------------------------------------------------------
# Greedy mode: fast approximation
greedy_result <- corrPrune(mtcars, threshold = 0.7, mode = "greedy")
cat("Greedy mode kept:", ncol(greedy_result), "variables\n")

## -----------------------------------------------------------------------------
# Auto mode: smart switching (exact if p ≤ 20, greedy otherwise)
auto_result <- corrPrune(mtcars, threshold = 0.7, mode = "auto")
cat("Auto mode kept:", ncol(auto_result), "variables\n")

## -----------------------------------------------------------------------------
# Generate datasets with increasing p
library(microbenchmark)

benchmark_corrPrune <- function(p_values) {
  results <- data.frame(
    p = integer(),
    exact_time_ms = numeric(),
    greedy_time_ms = numeric()
  )

  for (p in p_values) {
    # Generate correlated data
    set.seed(123)
    cor_mat <- 0.5^abs(outer(1:p, 1:p, "-"))
    data <- as.data.frame(MASS::mvrnorm(n = 100, mu = rep(0, p), Sigma = cor_mat))

    # Exact mode (skip if p too large)
    exact_time_ms <- if (p <= 500) {
      median(microbenchmark(
        corrPrune(data, threshold = 0.7, mode = "exact"),
        times = 3,  # Few iterations for speed
        unit = "ms"
      )$time) / 1e6  # Convert nanoseconds to milliseconds
    } else {
      NA
    }

    # Greedy mode
    greedy_time_ms <- median(microbenchmark(
      corrPrune(data, threshold = 0.7, mode = "greedy"),
      times = 3,  # Few iterations for speed
      unit = "ms"
    )$time) / 1e6  # Convert nanoseconds to milliseconds

    results <- rbind(results, data.frame(
      p = p,
      exact_time_ms = round(exact_time_ms, 1),
      greedy_time_ms = round(greedy_time_ms, 1)
    ))
  }

  results
}

# Benchmark (extended range to show comprehensive scaling behavior)
p_values <- c(10, 20, 50, 100, 200, 300, 500, 1000)
benchmark <- benchmark_corrPrune(p_values)
print(benchmark)

## ----fig.width=8, fig.height=5, fig.alt="Log-scale plot showing runtime (milliseconds) versus number of predictors for exact mode (red line) and greedy mode (blue line). Exact mode shows exponential growth with runtime increasing sharply after p=100, becoming impractical beyond p=500 (marked by vertical gray dashed line). Greedy mode shows linear scaling, remaining fast even at p=1000, demonstrating orders-of-magnitude performance advantage for high-dimensional data."----
# Visualize scaling
# Separate data for exact mode (only where available) and greedy mode (all points)
exact_valid <- !is.na(benchmark$exact_time_ms) & benchmark$exact_time_ms > 0
greedy_valid <- !is.na(benchmark$greedy_time_ms) & benchmark$greedy_time_ms > 0

# Replace any zeros with small positive value for log scale
exact_times <- benchmark$exact_time_ms
greedy_times <- benchmark$greedy_time_ms
exact_times[exact_times <= 0 & !is.na(exact_times)] <- 0.01
greedy_times[greedy_times <= 0 & !is.na(greedy_times)] <- 0.01

# Determine y-axis limits from valid data
all_valid_times <- c(exact_times[exact_valid], greedy_times[greedy_valid])
ylim <- c(min(all_valid_times) * 0.5, max(all_valid_times) * 2)

# Plot greedy mode (all points)
plot(benchmark$p[greedy_valid], greedy_times[greedy_valid],
     type = "b", col = rgb(0.2, 0.5, 0.8, 1), pch = 19, lwd = 2,
     xlab = "Number of Predictors (p)",
     ylab = "Time (milliseconds, log scale)",
     main = "Exact vs Greedy Scaling",
     ylim = ylim,
     xlim = range(benchmark$p),
     log = "y")

# Add exact mode (only where available)
lines(benchmark$p[exact_valid], exact_times[exact_valid],
      type = "b", col = rgb(0.8, 0.2, 0.2, 1), pch = 19, lwd = 2)

# Mark exact mode limit
abline(v = 500, lty = 2, col = "gray50")
text(500, ylim[2] * 0.5, "Exact mode limit", pos = 4, col = "gray30")

legend("topleft",
       legend = c("Exact", "Greedy"),
       col = c(rgb(0.8, 0.2, 0.2, 1), rgb(0.2, 0.5, 0.8, 1)),
       pch = 19,
       lwd = 2,
       bty = "n")

## -----------------------------------------------------------------------------
# Create data with identical correlations
set.seed(123)
x1 <- rnorm(100)
x2 <- x1 + rnorm(100, sd = 0.1)  # Almost identical to x1
x3 <- x1 + rnorm(100, sd = 0.1)  # Also almost identical to x1
x4 <- rnorm(100)                  # Independent

data_ties <- data.frame(x1, x2, x3, x4)

# Run multiple times - always same result
result1 <- corrPrune(data_ties, threshold = 0.95)
result2 <- corrPrune(data_ties, threshold = 0.95)

cat("Run 1 selected:", names(result1), "\n")
cat("Run 2 selected:", names(result2), "\n")
cat("Identical:", identical(names(result1), names(result2)), "\n")

## ----eval=FALSE---------------------------------------------------------------
# my_engine <- list(
#   # Required: How to fit the model
#   fit = function(formula, data, ...) {
#     # Your model fitting code
#     # Must return a fitted model object
#   },
# 
#   # Required: How to compute diagnostics
#   diagnostics = function(model, fixed_effects) {
#     # Compute diagnostic scores for each fixed effect
#     # Higher values = worse (more likely to be removed)
#     # Must return a named numeric vector
#   },
# 
#   # Optional: Name for error messages
#   name = "my_custom_engine"  # Defaults to "custom"
# )

## ----eval=FALSE---------------------------------------------------------------
# # Custom engine for INLA
# inla_engine <- list(
#   name = "inla",
# 
#   fit = function(formula, data, ...) {
#     # Fit INLA model
#     INLA::inla(
#       formula = formula,
#       data = data,
#       family = list(...)$family %||% "gaussian",
#       control.compute = list(config = TRUE),
#       ...
#     )
#   },
# 
#   diagnostics = function(model, fixed_effects) {
#     # Use posterior SD as "badness" metric
#     # Higher SD = more uncertain = candidate for removal
#     summary_fixed <- model$summary.fixed
#     scores <- summary_fixed[, "sd"]
#     names(scores) <- rownames(summary_fixed)
# 
#     # Return scores for fixed effects only
#     scores[fixed_effects]
#   }
# )
# 
# # Usage example
# pruned <- modelPrune(
#   y ~ x1 + x2 + x3 + x4,
#   data = my_data,
#   engine = inla_engine,
#   limit = 0.5  # Remove if posterior SD > 0.5
# )

## ----eval=FALSE---------------------------------------------------------------
# # Custom engine for mgcv GAMs
# mgcv_engine <- list(
#   name = "mgcv_gam",
# 
#   fit = function(formula, data, ...) {
#     mgcv::gam(formula, data = data, ...)
#   },
# 
#   diagnostics = function(model, fixed_effects) {
#     # Use p-values as badness metric
#     # Higher p-value = less significant = candidate for removal
#     summary_obj <- summary(model)
# 
#     # Extract parametric term p-values
#     pvals <- summary_obj$p.pv
# 
#     # Return p-values for fixed effects
#     pvals[fixed_effects]
#   }
# )
# 
# # Usage example
# pruned <- modelPrune(
#   y ~ x1 + x2 + s(x3),  # s() for smooth terms
#   data = my_data,
#   engine = mgcv_engine,
#   limit = 0.05  # Remove if p > 0.05
# )

## ----eval=FALSE---------------------------------------------------------------
# # AIC-based pruning engine
# aic_engine <- list(
#   name = "aic_pruner",
# 
#   fit = function(formula, data, ...) {
#     lm(formula, data = data)
#   },
# 
#   diagnostics = function(model, fixed_effects) {
#     # For each predictor, compute ΔAIC if removed
#     full_aic <- AIC(model)
# 
#     scores <- numeric(length(fixed_effects))
#     names(scores) <- fixed_effects
# 
#     for (var in fixed_effects) {
#       # Refit without this variable
#       reduced_formula <- update(formula(model), paste("~ . -", var))
#       reduced_model <- lm(reduced_formula, data = model$model)
# 
#       # ΔAIC: negative means removing improves model
#       # We negate so "higher = worse" convention holds
#       scores[var] <- -(AIC(reduced_model) - full_aic)
#     }
# 
#     scores
#   }
# )
# 
# # Usage: Remove predictors with ΔAIC < -2 (improve AIC by > 2 when removed)
# pruned <- modelPrune(
#   y ~ x1 + x2 + x3,
#   data = my_data,
#   engine = aic_engine,
#   limit = -2
# )

## ----error=TRUE---------------------------------------------------------------
try({
# Invalid engine: missing 'diagnostics'
bad_engine <- list(
  fit = function(formula, data, ...) lm(formula, data = data)
  # Missing 'diagnostics'
)

tryCatch({
  modelPrune(mpg ~ ., data = mtcars, engine = bad_engine, limit = 5)
}, error = function(e) {
  cat("Error:", e$message, "\n")
})
})

## ----eval=FALSE---------------------------------------------------------------
# # Test your fit function in isolation
# test_model <- my_engine$fit(y ~ x1 + x2, data = my_data)
# summary(test_model)  # Does it work?
# 
# # Test your diagnostics function
# test_diag <- my_engine$diagnostics(test_model, c("x1", "x2"))
# print(test_diag)  # Numeric? Named? Correct length?
# 
# # Check that "higher = worse" convention is satisfied
# # The variable with the highest score should be the one you'd remove first

## -----------------------------------------------------------------------------
# corrPrune: Single subset
single <- corrPrune(mtcars, threshold = 0.7)
cat("corrPrune returned:", ncol(single), "variables\n")

# corrSelect: ALL maximal subsets (use higher threshold to ensure subsets exist)
all_subsets <- corrSelect(mtcars, threshold = 0.9)
show(all_subsets)

## -----------------------------------------------------------------------------
if (length(all_subsets@subset_list) > 0) {
  # Display first few subsets
  cat("First few maximal subsets:\n")
  for (i in seq_len(min(3, length(all_subsets@subset_list)))) {
    cat(sprintf("\nSubset %d (avg corr = %.3f):\n", i, all_subsets@avg_corr[i]))
    cat(" ", paste(all_subsets@subset_list[[i]], collapse = ", "), "\n")
  }

  # Analyze subset characteristics
  subset_sizes <- lengths(all_subsets@subset_list)
  cat("\nSubset sizes:\n")
  print(table(subset_sizes))

  cat("\nAverage correlations:\n")
  print(summary(all_subsets@avg_corr))
} else {
  cat("No subsets found at threshold 0.9\n")
}

## -----------------------------------------------------------------------------
if (length(all_subsets@subset_list) > 0) {
  # Extract subset with lowest average correlation
  best_idx <- which.min(all_subsets@avg_corr)
  best_subset <- corrSubset(all_subsets, mtcars, which = best_idx)

  cat("Best subset (lowest avg correlation):\n")
  print(names(best_subset))

  # Extract subset with most predictors
  subset_sizes <- lengths(all_subsets@subset_list)
  largest_idx <- which.max(subset_sizes)
  largest_subset <- corrSubset(all_subsets, mtcars, which = largest_idx)

  cat("\nLargest subset:\n")
  print(names(largest_subset))
} else {
  cat("No subsets to extract at threshold 0.9\n")
}

## -----------------------------------------------------------------------------
if (length(all_subsets@subset_list) > 0) {
  # Custom criterion: Prefer subsets with specific variables
  preferred_vars <- c("mpg", "hp", "wt")

  # Compute score: number of preferred variables in each subset
  scores <- sapply(all_subsets@subset_list, function(vars) {
    sum(preferred_vars %in% vars)
  })

  # Select subset with most preferred variables
  best_idx <- which.max(scores)
  cat("Subset with most preferred variables (score:", scores[best_idx], "):\n")
  cat(paste(all_subsets@subset_list[[best_idx]], collapse = ", "), "\n")

  # Extract as data frame
  preferred_subset <- corrSubset(all_subsets, mtcars, which = best_idx)
  print(head(preferred_subset))
} else {
  cat("No subsets available for custom selection\n")
}

## -----------------------------------------------------------------------------
# Create larger dataset for meaningful timing comparison
set.seed(123)
large_data <- as.data.frame(matrix(rnorm(100 * 50), ncol = 50))

# Benchmark: Recompute correlation every time
time1 <- median(microbenchmark(
  {
    result1 <- corrPrune(large_data, threshold = 0.7)
    result2 <- corrPrune(large_data, threshold = 0.8)
    result3 <- corrPrune(large_data, threshold = 0.9)
  },
  times = 3,
  unit = "ms"
)$time) / 1e6  # Convert nanoseconds to milliseconds

# Benchmark: Compute correlation once, reuse
cor_matrix <- cor(large_data)
time2 <- median(microbenchmark(
  {
    result1 <- MatSelect(cor_matrix, threshold = 0.7)
    result2 <- MatSelect(cor_matrix, threshold = 0.8)
    result3 <- MatSelect(cor_matrix, threshold = 0.9)
  },
  times = 3,
  unit = "ms"
)$time) / 1e6  # Convert nanoseconds to milliseconds

cat(sprintf("Recomputing each time: %.1f ms\n", time1))
cat(sprintf("Precomputed matrix: %.1f ms\n", time2))
cat(sprintf("Speedup: %.1fx faster\n", time1 / time2))

## ----eval=FALSE---------------------------------------------------------------
# # Standard (memory-intensive for large n)
# cor_matrix <- cor(large_data)
# 
# # Memory-efficient alternative (for very large n)
# # Process in chunks if needed
# compute_cor_chunked <- function(data, chunk_size = 1000) {
#   n <- nrow(data)
#   n_chunks <- ceiling(n / chunk_size)
# 
#   # Use online algorithm or chunked computation
#   # (Implementation depends on your data size)
# }

## ----eval=FALSE---------------------------------------------------------------
# # Convert to sparse format (requires Matrix package)
# library(Matrix)
# 
# # Compute correlation
# cor_mat <- cor(data)
# 
# # Threshold and convert to sparse
# cor_sparse <- cor_mat
# cor_sparse[abs(cor_sparse) < 0.3] <- 0  # Set low correlations to 0
# cor_sparse <- Matrix(cor_sparse, sparse = TRUE)
# 
# # Memory savings
# object.size(cor_mat)
# object.size(cor_sparse)

## ----eval=FALSE---------------------------------------------------------------
# library(parallel)
# 
# # Create cluster
# cl <- makeCluster(detectCores() - 1)
# 
# # Export functions to cluster
# clusterEvalQ(cl, library(corrselect))
# 
# # Parallel pruning with different thresholds
# thresholds <- seq(0.5, 0.9, by = 0.1)
# results <- parLapply(cl, thresholds, function(thresh) {
#   corrPrune(my_data, threshold = thresh)
# })
# 
# # Clean up
# stopCluster(cl)

## ----error=TRUE---------------------------------------------------------------
try({
# Example: All correlations > 0.9
set.seed(123)
x <- rnorm(100)
high_cor_data <- data.frame(
  x1 = x,
  x2 = x + rnorm(100, sd = 0.01),
  x3 = x + rnorm(100, sd = 0.01)
)

tryCatch({
  corrPrune(high_cor_data, threshold = 0.5)
}, error = function(e) {
  cat("Error:", e$message, "\n")
})
})

## ----error=TRUE---------------------------------------------------------------
try({
# Solution 1: Increase threshold
result <- corrPrune(high_cor_data, threshold = 0.95)
print(names(result))

# Solution 2: Force keep one variable
result <- corrPrune(high_cor_data, threshold = 0.5, force_in = "x1")
print(names(result))
})

## ----error=TRUE---------------------------------------------------------------
try({
# x1 and x2 are highly correlated
tryCatch({
  corrPrune(high_cor_data, threshold = 0.5, force_in = c("x1", "x2"))
}, error = function(e) {
  cat("Error:", e$message, "\n")
})
})

## ----error=TRUE---------------------------------------------------------------
try({
# Create perfect multicollinearity
perfect_data <- data.frame(
  y = rnorm(100),
  x1 = rnorm(100),
  x2 = rnorm(100)
)
perfect_data$x3 <- perfect_data$x1 + perfect_data$x2  # Perfect collinearity

tryCatch({
  modelPrune(y ~ ., data = perfect_data, limit = 5)
}, error = function(e) {
  cat("Error:", e$message, "\n")
})
})

## -----------------------------------------------------------------------------
# Two-step approach
step1 <- corrPrune(perfect_data[, -1], threshold = 0.99)
step2_data <- data.frame(y = perfect_data$y, step1)
result <- modelPrune(y ~ ., data = step2_data, limit = 5)
print(attr(result, "selected_vars"))

## ----fig.width=10, fig.height=4, fig.alt="Two side-by-side plots for threshold selection guidance. Left panel: histogram of absolute correlations in mtcars showing distribution with vertical lines marking strict (0.5, red), moderate (0.7, blue), and lenient (0.9, green) thresholds. Right panel: line plot showing number of variables retained versus threshold, demonstrating sensitivity analysis with plateau beginning around 0.7, helping identify optimal threshold for balancing redundancy reduction and information retention."----
data(mtcars)

# Visualize correlation distribution
cor_mat <- cor(mtcars)
cor_vec <- cor_mat[upper.tri(cor_mat)]

par(mfrow = c(1, 2))

# Histogram of correlations
hist(abs(cor_vec), breaks = 30,
     main = "Distribution of |Correlations|",
     xlab = "|Correlation|",
     col = "lightblue")
abline(v = c(0.5, 0.7, 0.9), col = c("red", "blue", "green"), lwd = 2, lty = 2)
legend("topright",
       legend = c("0.5 (strict)", "0.7 (moderate)", "0.9 (lenient)"),
       col = c("red", "blue", "green"), lwd = 2, lty = 2)

# Subset size vs threshold
thresholds <- seq(0.3, 0.95, by = 0.05)
sizes <- sapply(thresholds, function(t) {
  tryCatch({
    ncol(corrPrune(mtcars, threshold = t))
  }, error = function(e) NA)
})

plot(thresholds, sizes, type = "b",
     xlab = "Threshold",
     ylab = "Number of Variables Retained",
     main = "Threshold Sensitivity",
     col = "blue", lwd = 2)
abline(h = ncol(mtcars), lty = 2, col = "gray")
text(0.3, ncol(mtcars), "Original", pos = 3)

## -----------------------------------------------------------------------------
# Very strict threshold may leave only 1 variable
strict_result <- corrPrune(mtcars, threshold = 0.3)
cat("Variables remaining:", ncol(strict_result), "\n")

# Check if result is usable
if (ncol(strict_result) < 2) {
  cat("Warning: Only 1 variable remaining. Consider:\n")
  cat("  1. Increasing threshold\n")
  cat("  2. Using force_in to keep important variables\n")
}

## ----error=TRUE---------------------------------------------------------------
try({
# Impossible threshold
tryCatch({
  corrPrune(mtcars, threshold = 0.0)
}, error = function(e) {
  cat("Error:", e$message, "\n")
})
})

## -----------------------------------------------------------------------------
# Create mixed data
mixed_data <- mtcars
mixed_data$cyl <- factor(mixed_data$cyl)
mixed_data$am <- factor(mixed_data$am)

# Use assocSelect for mixed types
result <- assocSelect(mixed_data, threshold = 0.6)
show(result)

## ----eval=FALSE---------------------------------------------------------------
# # 1. Visualize correlations
# corrplot::corrplot(cor(data), method = "circle")
# 
# # 2. Try multiple thresholds
# results <- lapply(c(0.5, 0.7, 0.9), function(t) {
#   corrPrune(data, threshold = t)
# })
# 
# # 3. Compare subset sizes
# sapply(results, ncol)
# 
# # 4. Choose based on your needs
# final_data <- results[[2]]  # threshold = 0.7

## ----eval=FALSE---------------------------------------------------------------
# # 1. Use exact mode for reproducibility and optimality
# data_pruned <- corrPrune(data, threshold = 0.7, mode = "exact")
# 
# # 2. Document in methods section
# cat(sprintf(
#   "Variables were pruned using corrselect::corrPrune() with threshold = 0.7, ",
#   "exact mode, retaining %d of %d original predictors.",
#   ncol(data_pruned), ncol(data)
# ))
# 
# # 3. Report which variables were removed
# removed <- attr(data_pruned, "removed_vars")
# cat("Removed variables:", paste(removed, collapse = ", "))

## ----eval=FALSE---------------------------------------------------------------
# # Comprehensive variable selection pipeline
# pipeline <- function(data, response) {
#   # Step 1: Remove correlations
#   step1 <- corrPrune(data, threshold = 0.7, mode = "auto")
# 
#   # Step 2: VIF cleanup
#   step2_data <- data.frame(response = response, step1)
#   step2 <- modelPrune(response ~ ., data = step2_data, limit = 5)
# 
#   # Step 3: Feature importance (optional)
#   if (requireNamespace("Boruta", quietly = TRUE)) {
#     boruta_result <- Boruta::Boruta(response ~ ., data = step2)
#     important <- Boruta::getSelectedAttributes(boruta_result)
#     final_data <- step2[, c("response", important)]
#   } else {
#     final_data <- step2
#   }
# 
#   final_data
# }

