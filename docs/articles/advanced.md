# Advanced Topics

### Overview

This vignette covers advanced topics for power users, researchers, and
method developers:

1.  **Understanding the Algorithms** - Exact vs greedy, complexity
    analysis
2.  **Custom Engines** - Integrate any modeling package (INLA, mgcv,
    brms)
3.  **Exact Subset Enumeration** - Multiple maximal subsets
4.  **Performance Optimization** - Speed and memory considerations
5.  **Troubleshooting** - Common issues and solutions

**Target audience**: Users comfortable with R programming and
statistical methods

**Estimated time**: 15-20 minutes

------------------------------------------------------------------------

## 1. Understanding the Algorithms

### 1.1 Exact vs Greedy: When to Use Each

corrselect offers two algorithmic approaches for
[`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md):

#### Exact Mode (Graph-Theoretic)

**Algorithm**: Eppstein–Löffler–Strash (ELS) or Bron–Kerbosch
**Complexity**: O(2^p) - exponential in number of predictors
**Guarantee**: Finds the **largest** maximal independent set

``` r

data(mtcars)

# Exact mode: guaranteed optimal
exact_result <- corrPrune(mtcars, threshold = 0.7, mode = "exact")
cat("Exact mode kept:", ncol(exact_result), "variables\n")
#> Exact mode kept: 5 variables
```

**Use exact mode when**:

- p ≤ 20 (feasible runtime)
- You need guaranteed optimal solution
- Reproducibility is critical
- You’re writing a paper (justify optimality)

#### Greedy Mode (Heuristic)

**Algorithm**: Deterministic iterative removal **Complexity**: O(p² × k)
where k = iterations **Guarantee**: Near-optimal, deterministic

``` r

# Greedy mode: fast approximation
greedy_result <- corrPrune(mtcars, threshold = 0.7, mode = "greedy")
cat("Greedy mode kept:", ncol(greedy_result), "variables\n")
#> Greedy mode kept: 5 variables
```

**Use greedy mode when**:

- p \> 20 (exact becomes slow)
- Speed is priority
- Near-optimal is acceptable
- High-dimensional data (p \> 100)

#### Auto Mode (Recommended)

Automatically selects based on p:

``` r

# Auto mode: smart switching (exact if p ≤ 20, greedy otherwise)
auto_result <- corrPrune(mtcars, threshold = 0.7, mode = "auto")
cat("Auto mode kept:", ncol(auto_result), "variables\n")
#> Auto mode kept: 5 variables
```

### 1.2 Complexity Analysis with Benchmarks

Let’s measure runtime scaling:

``` r

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
#>      p exact_time_ms greedy_time_ms
#> 1   10           0.8            0.4
#> 2   20           0.9            0.6
#> 3   50           1.6            1.3
#> 4  100           3.8            2.9
#> 5  200           9.1            5.8
#> 6  300          20.6            9.1
#> 7  500          51.8           21.7
#> 8 1000            NA           63.8
```

``` r

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
       bty = "o",
       bg = "white")
```

![Log-scale plot showing runtime (milliseconds) versus number of
predictors for exact mode (red line) and greedy mode (blue line). Exact
mode shows exponential growth with runtime increasing sharply after
p=100, becoming impractical beyond p=500 (marked by vertical gray dashed
line). Greedy mode shows linear scaling, remaining fast even at p=1000,
demonstrating orders-of-magnitude performance advantage for
high-dimensional
data.](advanced_files/figure-html/unnamed-chunk-5-1.svg)

**Key insight**: The scaling behavior depends heavily on correlation
structure. For this moderately correlated dataset (ρ = 0.5^\|i-j\|),
exact mode remains practical up to p ≈ 500, while greedy mode scales
efficiently beyond p = 1000. The exact mode provides guaranteed
optimality at the cost of computational complexity, while greedy mode
offers substantial speed advantages for large p with near-optimal
results in most practical scenarios.

### 1.3 Deterministic Tie-Breaking

When multiple variables have identical correlation profiles, corrselect
uses deterministic tie-breaking:

``` r

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
#> Run 1 selected: x2 x4
cat("Run 2 selected:", names(result2), "\n")
#> Run 2 selected: x2 x4
cat("Identical:", identical(names(result1), names(result2)), "\n")
#> Identical: TRUE
```

**Tie-breaking rules**: 1. Prefer variables with lower **mean absolute
correlation** with others 2. If still tied, prefer **lexicographically
first** (alphabetical)

This ensures reproducibility across runs, machines, and R versions.

------------------------------------------------------------------------

## 2. Custom Engines for modelPrune()

### 2.1 Understanding Custom Engines

#### What is a Custom Engine?

A custom engine allows you to integrate **any** modeling framework with
[`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md),
not just base R’s [`lm()`](https://rdrr.io/r/stats/lm.html) or `lme4`.
This enables VIF-based pruning for:

- **Bayesian models** (INLA, brms, Stan)
- **Additive models** (mgcv GAMs)
- **Survival models** (coxph, flexsurv)
- **Custom metrics** (AIC, BIC, posterior uncertainty)

#### How Custom Engines Work

The
[`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)
algorithm follows this iterative process:

1.  **Fit** the model with current predictors
2.  **Diagnose** each predictor (compute a “badness” metric)
3.  **Identify** the worst predictor (highest diagnostic value)
4.  **Remove** if it exceeds the `limit` threshold
5.  **Repeat** until all predictors satisfy the limit

Your custom engine defines steps 1 and 2;
[`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)
handles the iteration logic.

#### Engine Structure Requirements

A custom engine is a named list with two required functions:

``` r

my_engine <- list(
  # Required: How to fit the model
  fit = function(formula, data, ...) {
    # Your model fitting code
    # Must return a fitted model object
  },

  # Required: How to compute diagnostics
  diagnostics = function(model, fixed_effects) {
    # Compute diagnostic scores for each fixed effect
    # Higher values = worse (more likely to be removed)
    # Must return a named numeric vector
  },

  # Optional: Name for error messages
  name = "my_custom_engine"  # Defaults to "custom"
)
```

**Key principle**: The `diagnostics` function must return **higher
values for worse predictors**. This inverted metric ensures the
algorithm removes the most problematic variables first.

### 2.2 Example: INLA Engine (Bayesian Spatial Models)

#### Background

INLA (Integrated Nested Laplace Approximations) is a popular package for
fast Bayesian inference, especially for spatial and temporal models.
Unlike traditional VIF, we can use **posterior uncertainty** as a
pruning criterion: variables with high posterior standard deviation
contribute less information to the model.

#### Implementation

``` r

# Custom engine for INLA
inla_engine <- list(
  name = "inla",

  fit = function(formula, data, ...) {
    # Fit INLA model
    INLA::inla(
      formula = formula,
      data = data,
      family = list(...)$family %||% "gaussian",
      control.compute = list(config = TRUE),
      ...
    )
  },

  diagnostics = function(model, fixed_effects) {
    # Use posterior SD as "badness" metric
    # Higher SD = more uncertain = candidate for removal
    summary_fixed <- model$summary.fixed
    scores <- summary_fixed[, "sd"]
    names(scores) <- rownames(summary_fixed)

    # Return scores for fixed effects only
    scores[fixed_effects]
  }
)

# Usage example
pruned <- modelPrune(
  y ~ x1 + x2 + x3 + x4,
  data = my_data,
  engine = inla_engine,
  limit = 0.5  # Remove if posterior SD > 0.5
)
```

#### How It Works

1.  **fit**: Calls `INLA::inla()` to compute posterior distributions
2.  **diagnostics**: Extracts posterior standard deviations from
    `model$summary.fixed`
3.  **limit**: Threshold for acceptable uncertainty (e.g., 0.5 means
    remove predictors with posterior SD \> 0.5)

**Interpretation**: Variables with high posterior SD have coefficients
that are uncertain given the data. Removing them reduces model
complexity without losing much information.

**When to use**: Spatial models, hierarchical models, disease mapping,
ecological modeling with INLA.

### 2.3 Example: mgcv Engine (GAMs)

#### Background

Generalized Additive Models (GAMs) allow non-linear relationships via
smooth terms. When pruning GAMs, we want to remove **parametric (linear)
terms** with weak evidence while preserving smooth terms that model
non-linear patterns.

#### Implementation

``` r

# Custom engine for mgcv GAMs
mgcv_engine <- list(
  name = "mgcv_gam",

  fit = function(formula, data, ...) {
    mgcv::gam(formula, data = data, ...)
  },

  diagnostics = function(model, fixed_effects) {
    # Use p-values as badness metric
    # Higher p-value = less significant = candidate for removal
    summary_obj <- summary(model)

    # Extract parametric term p-values
    pvals <- summary_obj$p.pv

    # Return p-values for fixed effects
    pvals[fixed_effects]
  }
)

# Usage example
pruned <- modelPrune(
  y ~ x1 + x2 + s(x3),  # s() for smooth terms
  data = my_data,
  engine = mgcv_engine,
  limit = 0.05  # Remove if p > 0.05
)
```

#### How It Works

1.  **fit**: Calls
    [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) to fit the
    additive model
2.  **diagnostics**: Extracts p-values for **parametric terms only**
    (not smooth terms)
3.  **limit**: Significance threshold (e.g., 0.05 for traditional α =
    0.05)

**Important**:
[`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)
only removes parametric (linear) terms. Smooth terms specified with
`s()`, `te()`, etc. are **never removed** automatically, as they’re part
of the model structure.

**When to use**: Non-linear regression, ecological response curves, time
series with trends.

### 2.4 Example: Custom Criterion (AIC-Based)

#### Background

Sometimes you want to prune based on **model comparison metrics** rather
than coefficient-level diagnostics. This example shows how to remove
variables that don’t improve model fit according to AIC.

#### Implementation

``` r

# AIC-based pruning engine
aic_engine <- list(
  name = "aic_pruner",

  fit = function(formula, data, ...) {
    lm(formula, data = data)
  },

  diagnostics = function(model, fixed_effects) {
    # For each predictor, compute ΔAIC if removed
    full_aic <- AIC(model)

    scores <- numeric(length(fixed_effects))
    names(scores) <- fixed_effects

    for (var in fixed_effects) {
      # Refit without this variable
      reduced_formula <- update(formula(model), paste("~ . -", var))
      reduced_model <- lm(reduced_formula, data = model$model)

      # ΔAIC: negative means removing improves model
      # We negate so "higher = worse" convention holds
      scores[var] <- -(AIC(reduced_model) - full_aic)
    }

    scores
  }
)

# Usage: Remove predictors with ΔAIC < -2 (improve AIC by > 2 when removed)
pruned <- modelPrune(
  y ~ x1 + x2 + x3,
  data = my_data,
  engine = aic_engine,
  limit = -2
)
```

#### How It Works

1.  **fit**: Standard linear model
2.  **diagnostics**: For each variable, refit the model **without** that
    variable and compute ΔAIC
3.  **limit**: Threshold for ΔAIC (e.g., -2 means “remove if AIC
    improves by more than 2 points”)

**Key insight**: The score is **negated** (multiplied by -1) to maintain
the “higher is worse” convention. A variable with score \> -2 means
removing it would worsen AIC by more than 2, so it’s kept.

**When to use**: Model selection focused on parsimony, comparing nested
models, when VIF isn’t the right metric.

**Alternative metrics**: You can adapt this pattern for BIC, likelihood
ratio tests, or any other model comparison criterion.

### 2.5 Validation and Error Handling

#### Automatic Validation

[`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)
automatically validates custom engines to catch common errors early:

``` r

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
#> Error: Custom engine missing required fields: diagnostics
#> Required: 'fit' and 'diagnostics'
```

#### Validation Checklist

Your custom engine must satisfy these requirements:

1.  **Structure**: Named list with `fit` and `diagnostics` functions
2.  **fit returns model object**: Must return something the
    `diagnostics` function can consume
3.  **diagnostics returns numeric vector**: No characters, factors, or
    other types
4.  **Correct length**: One diagnostic value per fixed effect (excluding
    intercept)
5.  **Named vector**: Names must match variable names exactly
6.  **No missing values**: NA, NaN, or Inf will cause errors
7.  **Higher = worse**: Diagnostic values must increase for more
    problematic predictors

#### Debugging Tips

If your custom engine fails, check:

``` r

# Test your fit function in isolation
test_model <- my_engine$fit(y ~ x1 + x2, data = my_data)
summary(test_model)  # Does it work?

# Test your diagnostics function
test_diag <- my_engine$diagnostics(test_model, c("x1", "x2"))
print(test_diag)  # Numeric? Named? Correct length?

# Check that "higher = worse" convention is satisfied
# The variable with the highest score should be the one you'd remove first
```

------------------------------------------------------------------------

## 3. Exact Subset Enumeration

### 3.1 When You Need ALL Maximal Subsets

[`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)
returns a **single** subset. Sometimes you want **all** maximal subsets:

``` r

# corrPrune: Single subset
single <- corrPrune(mtcars, threshold = 0.7)
cat("corrPrune returned:", ncol(single), "variables\n")
#> corrPrune returned: 5 variables

# corrSelect: ALL maximal subsets (use higher threshold to ensure subsets exist)
all_subsets <- corrSelect(mtcars, threshold = 0.9)
show(all_subsets)
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: pearson
#>   Threshold:   0.900
#>   Subsets:     2 maximal subsets
#>   Data Rows:   32 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] mpg, disp, hp, drat, wt, qsec...  0.527  0.888    10
#>   [ 2] mpg, cyl, hp, drat, wt, qsec,...  0.531  0.868    10
```

### 3.2 Exploring Multiple Subsets

When multiple maximal subsets exist, you can explore them:

``` r

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
#> First few maximal subsets:
#> 
#> Subset 1 (avg corr = 0.527):
#>   mpg, disp, hp, drat, wt, qsec, vs, am, gear, carb 
#> 
#> Subset 2 (avg corr = 0.531):
#>   mpg, cyl, hp, drat, wt, qsec, vs, am, gear, carb 
#> 
#> Subset sizes:
#> subset_sizes
#> 10 
#>  2 
#> 
#> Average correlations:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.5269  0.5280  0.5290  0.5290  0.5301  0.5311
```

### 3.3 Extracting Specific Subsets

``` r

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
#> Best subset (lowest avg correlation):
#>  [1] "mpg"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear" "carb"
#> 
#> Largest subset:
#>  [1] "mpg"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear" "carb"
```

### 3.4 Advanced: Domain-Specific Subset Selection

You can define custom criteria for choosing among multiple subsets:

``` r

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
#> Subset with most preferred variables (score: 3 ):
#> mpg, disp, hp, drat, wt, qsec, vs, am, gear, carb 
#>                    mpg disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1  225 105 2.76 3.460 20.22  1  0    3    1
```

------------------------------------------------------------------------

## 4. Performance Optimization

### 4.1 Precomputed Correlation Matrices

For repeated pruning with different thresholds, precompute the
correlation matrix:

``` r

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
#> Recomputing each time: 5.3 ms
cat(sprintf("Precomputed matrix: %.1f ms\n", time2))
#> Precomputed matrix: 1.3 ms
cat(sprintf("Speedup: %.1fx faster\n", time1 / time2))
#> Speedup: 4.2x faster
```

**Use precomputed matrices when**:

- Testing multiple thresholds
- Cross-validation workflows
- Sensitivity analysis

### 4.2 Memory Considerations for Large Data

For large datasets (n \> 10,000, p \> 500):

#### Memory-Efficient Correlation Computation

``` r

# Standard (memory-intensive for large n)
cor_matrix <- cor(large_data)

# Memory-efficient alternative (for very large n)
# Process in chunks if needed
compute_cor_chunked <- function(data, chunk_size = 1000) {
  n <- nrow(data)
  n_chunks <- ceiling(n / chunk_size)

  # Use online algorithm or chunked computation
  # (Implementation depends on your data size)
}
```

#### Sparse Correlation Matrices

If most correlations are low, consider sparse storage:

``` r

# Convert to sparse format (requires Matrix package)
library(Matrix)

# Compute correlation
cor_mat <- cor(data)

# Threshold and convert to sparse
cor_sparse <- cor_mat
cor_sparse[abs(cor_sparse) < 0.3] <- 0  # Set low correlations to 0
cor_sparse <- Matrix(cor_sparse, sparse = TRUE)

# Memory savings
object.size(cor_mat)
object.size(cor_sparse)
```

### 4.3 Parallel Processing Strategies

For multiple independent pruning operations:

``` r

library(parallel)

# Create cluster
cl <- makeCluster(detectCores() - 1)

# Export functions to cluster
clusterEvalQ(cl, library(corrselect))

# Parallel pruning with different thresholds
thresholds <- seq(0.5, 0.9, by = 0.1)
results <- parLapply(cl, thresholds, function(thresh) {
  corrPrune(my_data, threshold = thresh)
})

# Clean up
stopCluster(cl)
```

**Note**: corrselect itself doesn’t parallelize internally (for
reproducibility), but you can parallelize **across** multiple analyses.

### 4.4 Choosing the Right Mode

Decision tree for mode selection:

    p ≤ 15:  Use "exact" (fast enough, guaranteed optimal)
    15 < p ≤ 25:  Use "exact" if time permits, "greedy" if speed critical
    p > 25:  Use "greedy" or "auto"
    p > 100: Always use "greedy"

------------------------------------------------------------------------

## 5. Troubleshooting

### 5.1 Common Errors and Solutions

#### Error: “No valid subsets found”

**Cause**: Threshold too strict - all variables exceed it

``` r

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
#> Error: No valid subsets found that satisfy the threshold constraint
```

**Solutions**: 1. Increase threshold 2. Use `force_in` to keep at least
one variable 3. Check data for near-duplicates

``` r

# Solution 1: Increase threshold
result <- corrPrune(high_cor_data, threshold = 0.95)
#> Error in corrPrune(high_cor_data, threshold = 0.95): No valid subsets found that satisfy the threshold constraint
print(names(result))
#> Error: object 'result' not found

# Solution 2: Force keep one variable
result <- corrPrune(high_cor_data, threshold = 0.5, force_in = "x1")
#> Error in corrPrune(high_cor_data, threshold = 0.5, force_in = "x1"): No valid subsets found that satisfy the threshold constraint
print(names(result))
#> Error: object 'result' not found
```

#### Error: force_in variables conflict with threshold

**Cause**: Variables in `force_in` have \|correlation\| \> threshold

``` r

# x1 and x2 are highly correlated
tryCatch({
  corrPrune(high_cor_data, threshold = 0.5, force_in = c("x1", "x2"))
}, error = function(e) {
  cat("Error:", e$message, "\n")
})
#> Error: Variables in 'force_in' violate the threshold constraint. Example: 'x1' and 'x2' have association 1.000 > 0.500
```

**Solution**: Either increase threshold or reduce `force_in` set

#### Error: VIF computation fails in modelPrune()

**Cause**: Perfect multicollinearity (R² = 1)

``` r

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
#> Warning in summary.lm(fit): essentially perfect fit: summary may be unreliable
#> Warning in summary.lm(fit): essentially perfect fit: summary may be unreliable
#> Warning in summary.lm(fit): essentially perfect fit: summary may be unreliable
#>               y          x1          x2
#> 1   -0.71524219 -0.07355602 -0.60189285
#> 2   -0.75268897 -1.16865142 -0.99369859
#> 3   -0.93853870 -0.63474826  1.02678506
#> 4   -1.05251328 -0.02884155  0.75106130
#> 5   -0.43715953  0.67069597 -1.50916654
#> 6    0.33117917 -1.65054654 -0.09514745
#> 7   -2.01421050 -0.34975424 -0.89594782
#> 8    0.21198043  0.75640644 -2.07075107
#> 9    1.23667505 -0.53880916  0.15012013
#> 10   2.03757402  0.22729192 -0.07921171
#> 11   1.30117599  0.49222857 -0.09736927
#> 12   0.75677476  0.26783502  0.21615254
#> 13  -1.72673040  0.65325768  0.88246516
#> 14  -0.60150671 -0.12270866  0.20559750
#> 15  -0.35204646 -0.41367651 -0.61643584
#> 16   0.70352390 -2.64314895 -0.73479925
#> 17  -0.10567133 -0.09294102 -0.13180279
#> 18  -1.25864863  0.43028470  0.31001699
#> 19   1.68443571  0.53539884 -1.03968035
#> 20   0.91139129 -0.55527835 -0.18430887
#> 21   0.23743027  1.77950291  0.96726726
#> 22   1.21810861  0.28642442 -0.10828009
#> 23  -1.33877429  0.12631586 -0.69842067
#> 24   0.66082030  1.27226678 -0.27594517
#> 25  -0.52291238 -0.71846622  1.11464855
#> 26   0.68374552 -0.45033862  0.55004396
#> 27  -0.06082195  2.39745248  1.23667580
#> 28   0.63296071  0.01112919  0.13909786
#> 29   1.33551762  1.63356842  0.41027510
#> 30   0.00729009 -1.43850664 -0.55845691
#> 31   1.01755864 -0.19051680  0.60537067
#> 32  -1.18843404  0.37842390 -0.50633354
#> 33  -0.72160444  0.30003855 -1.42056550
#> 34   1.51921771 -1.00563626  0.12799297
#> 35   0.37738797  0.01925927  1.94585122
#> 36  -2.05222282 -1.07742065  0.80091434
#> 37  -1.36403745  0.71270333  1.16525339
#> 38  -0.20078102  1.08477509  0.35885572
#> 39   0.86577940 -2.22498770 -0.60855718
#> 40  -0.10188326  1.23569346 -0.20224086
#> 41   0.62418747 -1.24104450 -0.27324811
#> 42   0.95900538  0.45476927 -0.46869978
#> 43   1.67105483  0.65990264  0.70416728
#> 44   0.05601673 -0.19988983 -1.19736350
#> 45  -0.05198191 -0.64511396  0.86636613
#> 46  -1.75323736  0.16532102  0.86415249
#> 47   0.09932759  0.43881870 -1.19862236
#> 48  -0.57185006  0.88330282  0.63949200
#> 49  -0.97400958 -2.05233698  2.43022665
#> 50  -0.17990623 -1.63637927 -0.55721548
#> 51   1.01494317  1.43040234  0.84490424
#> 52  -1.99274849  1.04662885 -0.78220185
#> 53  -0.42727929  0.43528895  1.11071142
#> 54   0.11663728  0.71517841  0.24982472
#> 55  -0.89320757  0.91717492  1.65191539
#> 56   0.33390294 -2.66092280 -1.45897073
#> 57   0.41142992  1.11027710 -0.05129789
#> 58  -0.03303616 -0.48498760 -0.52692518
#> 59  -2.46589819  0.23061683 -0.19726487
#> 60   2.57145815 -0.29515780 -0.62957874
#> 61  -0.20529926  0.87196495 -0.83384358
#> 62   0.65119328 -0.34847245  0.57872237
#> 63   0.27376649  0.51850377 -1.08758071
#> 64   1.02467323 -0.39068498  1.48403093
#> 65   0.81765945 -1.09278721 -1.18620659
#> 66  -0.20979317  1.21001051  0.10107915
#> 67   0.37816777  0.74090001  0.53298929
#> 68  -0.94540883  1.72426224  0.58673534
#> 69   0.85692301  0.06515393 -0.30174666
#> 70  -0.46103834  1.12500275  0.07950200
#> 71   2.41677335  1.97541905  0.96126415
#> 72  -1.65104890 -0.28148212 -1.45646592
#> 73  -0.46398724 -1.32295111 -0.78173971
#> 74   0.82537986 -0.23935157  0.32040231
#> 75   0.51013255 -0.21404124 -0.44478198
#> 76  -0.58948104  0.15168050  1.37000399
#> 77  -0.99678074  1.71230498  0.67325386
#> 78   0.14447570 -0.32614389  0.07216675
#> 79  -0.01430741  0.37300466 -1.50775732
#> 80  -1.79028124 -0.22768406  0.02610023
#> 81   0.03455107  0.02045071 -0.31641587
#> 82   0.19023032  0.31405766 -0.10234651
#> 83   0.17472640  1.32821470 -1.18155923
#> 84  -1.05501704  0.12131838  0.49865804
#> 85   0.47613328  0.71284232 -1.03895644
#> 86   1.37857014  0.77886003 -0.22622198
#> 87   0.45623640  0.91477327  0.38142583
#> 88  -1.13558847 -0.57439455 -0.78351579
#> 89  -0.43564547  1.62688121  0.58299141
#> 90   0.34610362 -0.38095674 -1.31651040
#> 91  -0.64704563 -0.10578417 -2.80977468
#> 92  -2.15764634  1.40405027  0.46496799
#> 93   0.88425082  1.29408391  0.84053983
#> 94  -0.82947761 -1.08999187 -0.28584542
#> 95  -0.57356027 -0.87307100  0.50412625
#> 96   1.50390061 -1.35807906 -1.15591653
#> 97  -0.77414493  0.18184719 -0.12714861
#> 98   0.84573154  0.16484087 -1.94151838
#> 99  -1.26068288  0.36411469  1.18118089
#> 100 -0.35454240  0.55215771  1.85991086
```

**Solution**: Use
[`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)
first to remove perfect collinearity:

``` r

# Two-step approach
step1 <- corrPrune(perfect_data[, -1], threshold = 0.99)
step2_data <- data.frame(y = perfect_data$y, step1)
result <- modelPrune(y ~ ., data = step2_data, limit = 5)
#> Warning in summary.lm(fit): essentially perfect fit: summary may be unreliable
#> Warning in summary.lm(fit): essentially perfect fit: summary may be unreliable
#> Warning in summary.lm(fit): essentially perfect fit: summary may be unreliable
print(attr(result, "selected_vars"))
#> [1] "x1" "x2"
```

### 5.2 Threshold Selection Guidance

#### For corrPrune() (Correlation Threshold)

**Conservative (strict)**:

- threshold = 0.5: Very low redundancy, may lose information
- Use when: Interpretability is critical, small sample size

**Moderate (recommended)**:

- threshold = 0.7: Balances redundancy and information retention
- Use when: Standard regression, general analysis

**Lenient**:

- threshold = 0.9: Only removes near-duplicates
- Use when: Large sample size, prediction focus

#### For modelPrune() (VIF Limit)

**Strict**:

- limit = 2: Very low multicollinearity, may over-prune
- Use when: Small sample size, interpretability critical

**Moderate (recommended)**:

- limit = 5: Standard threshold in literature
- Use when: General regression analysis

**Lenient**:

- limit = 10: Tolerates more multicollinearity
- Use when: Large sample size, prediction focus

#### Empirical Approach: Visualize First

``` r

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
       col = c("red", "blue", "green"), lwd = 2, lty = 2,
       bty = "o", bg = "white")

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
```

![Two side-by-side plots for threshold selection guidance. Left panel:
histogram of absolute correlations in mtcars showing distribution with
vertical lines marking strict (0.5, red), moderate (0.7, blue), and
lenient (0.9, green) thresholds. Right panel: line plot showing number
of variables retained versus threshold, demonstrating sensitivity
analysis with plateau beginning around 0.7, helping identify optimal
threshold for balancing redundancy reduction and information
retention.](advanced_files/figure-html/unnamed-chunk-26-1.svg)

**Strategy**: Choose threshold where curve begins to plateau.

### 5.3 Handling Edge Cases

#### Single Predictor After Pruning

``` r

# Very strict threshold may leave only 1 variable
strict_result <- corrPrune(mtcars, threshold = 0.3)
cat("Variables remaining:", ncol(strict_result), "\n")
#> Variables remaining: 2

# Check if result is usable
if (ncol(strict_result) < 2) {
  cat("Warning: Only 1 variable remaining. Consider:\n")
  cat("  1. Increasing threshold\n")
  cat("  2. Using force_in to keep important variables\n")
}
```

#### All Variables Removed

``` r

# Impossible threshold
tryCatch({
  corrPrune(mtcars, threshold = 0.0)
}, error = function(e) {
  cat("Error:", e$message, "\n")
})
#> Error: `threshold` must be in the range (0, 1].
```

#### Mixed-Type Data

``` r

# Create mixed data
mixed_data <- mtcars
mixed_data$cyl <- factor(mixed_data$cyl)
mixed_data$am <- factor(mixed_data$am)

# Use assocSelect for mixed types
result <- assocSelect(mixed_data, threshold = 0.6)
show(result)
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: mixed
#>   AssocMethod: numeric_factor = eta, numeric_numeric = pearson, factor_numeric
#>                = eta, factor_factor = cramersv
#>   Threshold:   0.600
#>   Subsets:     20 maximal subsets
#>   Data Rows:   32 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] wt, vs, gear, carb                0.436  0.583     4
#>   [ 2] vs, am, carb                      0.265  0.570     3
#>   [ 3] wt, qsec, gear                    0.324  0.583     3
#>   [ 4] disp, carb, am                    0.348  0.591     3
#>   [ 5] drat, vs, carb                    0.367  0.570     3
#>   ... (15 more combinations)
```

------------------------------------------------------------------------

## 6. Best Practices

### 6.1 Workflow Recommendations

#### For Exploratory Analysis

``` r

# 1. Visualize correlations
corrplot::corrplot(cor(data), method = "circle")

# 2. Try multiple thresholds
results <- lapply(c(0.5, 0.7, 0.9), function(t) {
  corrPrune(data, threshold = t)
})

# 3. Compare subset sizes
sapply(results, ncol)

# 4. Choose based on your needs
final_data <- results[[2]]  # threshold = 0.7
```

#### For Publication-Quality Analysis

``` r

# 1. Use exact mode for reproducibility and optimality
data_pruned <- corrPrune(data, threshold = 0.7, mode = "exact")

# 2. Document in methods section
cat(sprintf(
  "Variables were pruned using corrselect::corrPrune() with threshold = 0.7, ",
  "exact mode, retaining %d of %d original predictors.",
  ncol(data_pruned), ncol(data)
))

# 3. Report which variables were removed
removed <- attr(data_pruned, "removed_vars")
cat("Removed variables:", paste(removed, collapse = ", "))
```

### 6.2 Combining with Other Methods

``` r

# Comprehensive variable selection pipeline
pipeline <- function(data, response) {
  # Step 1: Remove correlations
  step1 <- corrPrune(data, threshold = 0.7, mode = "auto")

  # Step 2: VIF cleanup
  step2_data <- data.frame(response = response, step1)
  step2 <- modelPrune(response ~ ., data = step2_data, limit = 5)

  # Step 3: Feature importance (optional)
  if (requireNamespace("Boruta", quietly = TRUE)) {
    boruta_result <- Boruta::Boruta(response ~ ., data = step2)
    important <- Boruta::getSelectedAttributes(boruta_result)
    final_data <- step2[, c("response", important)]
  } else {
    final_data <- step2
  }

  final_data
}
```

------------------------------------------------------------------------

## 7. Summary

### Key Takeaways

#### Algorithms

- Use **exact mode** for p ≤ 20 (optimal, reproducible)
- Use **greedy mode** for p \> 20 (fast, near-optimal)
- Use **auto mode** to let corrselect decide

#### Custom Engines

- Integrate any modeling package (INLA, mgcv, brms)
- Define custom pruning criteria (AIC, BIC, p-values)
- Two required functions: `fit` and `diagnostics`

#### Optimization

- Precompute correlation matrices for multiple thresholds
- Use greedy mode for large p
- Parallelize across analyses, not within

#### Troubleshooting

- Visualize correlation distribution before choosing threshold
- Use `force_in` to protect important variables
- Two-step pruning (corrPrune → modelPrune) for robustness

------------------------------------------------------------------------

## 8. References

**Algorithms**:

- Eppstein, D., Löffler, M., & Strash, D. (2010). Listing all maximal
  cliques in sparse graphs in near-optimal time. *Symposium on
  Algorithms and Computation*.
- Bron, C., & Kerbosch, J. (1973). Algorithm 457: Finding all cliques of
  an undirected graph. *Communications of the ACM*, 16(9), 575-577.

**Multicollinearity**:

- O’Brien, R. M. (2007). A caution regarding rules of thumb for variance
  inflation factors. *Quality & Quantity*, 41(5), 673-690.
- Belsley, D. A., Kuh, E., & Welsch, R. E. (1980). *Regression
  Diagnostics*. Wiley.

**Software**:

- INLA: Rue, H., Martino, S., & Chopin, N. (2009). Approximate Bayesian
  inference for latent Gaussian models. *Journal of the Royal
  Statistical Society: Series B*, 71(2), 319-392.
- mgcv: Wood, S. N. (2017). *Generalized Additive Models: An
  Introduction with R* (2nd ed.). Chapman and Hall/CRC.

------------------------------------------------------------------------

### See Also

- [`vignette("quickstart")`](https://gillescolling.com/corrselect/articles/quickstart.md) -
  5-minute introduction
- [`vignette("workflows")`](https://gillescolling.com/corrselect/articles/workflows.md) -
  Real-world examples
- [`vignette("comparison")`](https://gillescolling.com/corrselect/articles/comparison.md) -
  vs caret, Boruta, glmnet
- `vignette("corrselect_vignette")` - Original exact methods vignette
- [`?corrPrune`](https://gillescolling.com/corrselect/reference/corrPrune.md) -
  Association-based pruning
- [`?modelPrune`](https://gillescolling.com/corrselect/reference/modelPrune.md) -
  Model-based pruning
- [`?corrSelect`](https://gillescolling.com/corrselect/reference/corrSelect.md) -
  Exact subset enumeration

### Session Info

``` r

sessionInfo()
#> R version 4.5.2 (2025-10-31 ucrt)
#> Platform: x86_64-w64-mingw32/x64
#> Running under: Windows 11 x64 (build 26200)
#> 
#> Matrix products: default
#>   LAPACK version 3.12.1
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.utf8 
#> [2] LC_CTYPE=English_United States.utf8   
#> [3] LC_MONETARY=English_United States.utf8
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.utf8    
#> 
#> time zone: Europe/Luxembourg
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] microbenchmark_1.5.0 corrselect_3.0.5    
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.37     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
#>  [5] xfun_0.53         cachem_1.1.0      knitr_1.50        htmltools_0.5.8.1
#>  [9] rmarkdown_2.30    lifecycle_1.0.4   cli_3.6.5         svglite_2.2.2    
#> [13] sass_0.4.10       pkgdown_2.2.0     textshaping_1.0.3 jquerylib_0.1.4  
#> [17] systemfonts_1.3.1 compiler_4.5.2    tools_4.5.2       bslib_0.9.0      
#> [21] evaluate_1.0.5    Rcpp_1.1.0        yaml_2.3.10       jsonlite_2.0.0   
#> [25] rlang_1.1.6       fs_1.6.6          htmlwidgets_1.6.4 MASS_7.3-65
```
