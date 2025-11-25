# Quick Start

## What corrselect Does

corrselect identifies and removes redundant variables based on pairwise
correlation or association. Given a threshold $`\tau`$, it finds subsets
where all pairwise associations satisfy $`|a_{ij}| < \tau`$ (see
[`vignette("theory")`](https://gillescolling.com/corrselect/articles/theory.md)
for mathematical formulation).

## Interface Hierarchy

corrselect provides three levels of interface:

### Level 1: Simple Pruning

**corrPrune()** - Removes redundant predictors based on pairwise
correlation:

- Returns a single pruned dataset
- No response variable required
- Fast greedy or exact search

**modelPrune()** - Reduces VIF in regression models:

- Returns a single pruned dataset with response
- Iteratively removes high-VIF predictors
- Works with lm, glm, lme4, glmmTMB

### Level 2: Structured Subset Selection

**corrSelect()** - Returns all maximal subsets (numeric data):

- Enumerates all maximal valid subsets satisfying threshold (see
  [`vignette("theory")`](https://gillescolling.com/corrselect/articles/theory.md))
- Provides full metadata (size, avg_corr, max_corr, min_corr)
- Exact or greedy search

**assocSelect()** - Returns all maximal subsets (mixed-type data):

- Handles numeric, factor, and ordered variables
- Uses appropriate association measures per variable pair
- Exact or greedy search

### Level 3: Low-Level Matrix Interface

**MatSelect()** - Direct matrix input:

- Accepts precomputed correlation/association matrices
- No data preprocessing
- Useful for repeated analyses

------------------------------------------------------------------------

## Quick Examples

### corrPrune(): Association-Based Pruning

``` r

library(corrselect)
data(mtcars)

# Remove correlated predictors (threshold = 0.7)
pruned <- corrPrune(mtcars, threshold = 0.7)

# Results
cat(sprintf("Reduced from %d to %d variables\n", ncol(mtcars), ncol(pruned)))
#> Reduced from 11 to 5 variables
names(pruned)
#> [1] "mpg"  "drat" "qsec" "gear" "carb"
```

Variables removed:

``` r

attr(pruned, "removed_vars")
#> [1] "cyl"  "disp" "hp"   "wt"   "vs"   "am"
```

**How corrPrune() selects among multiple maximal subsets**:

When multiple maximal subsets exist (which is common),
[`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)
returns the subset with the **lowest average absolute correlation**.
This selection criterion balances three goals:

1.  **Minimize redundancy**: Lower average correlation means more
    independent variables
2.  **Maximize information**: Prefers diverse variable combinations over
    tightly clustered ones
3.  **Deterministic behavior**: Always returns the same result for the
    same data

To explore **all** maximal subsets instead of just the optimal one, use
[`corrSelect()`](https://gillescolling.com/corrselect/reference/corrSelect.md)
(see below).

### modelPrune(): VIF-Based Pruning

``` r

# Prune based on VIF (limit = 5)
model_data <- modelPrune(
  formula = mpg ~ .,
  data = mtcars,
  limit = 5
)

# Results
cat("Variables kept:", paste(attr(model_data, "selected_vars"), collapse = ", "), "\n")
#> Variables kept: drat, qsec, vs, am, gear, carb
cat("Variables removed:", paste(attr(model_data, "removed_vars"), collapse = ", "), "\n")
#> Variables removed: disp, cyl, wt, hp
```

### corrSelect(): Enumerate All Maximal Subsets

``` r

results <- corrSelect(mtcars, threshold = 0.7)
show(results)
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: pearson
#>   Threshold:   0.700
#>   Subsets:     15 valid combinations
#>   Data Rows:   32 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] mpg, drat, qsec, gear, carb       0.416  0.700     5
#>   [ 2] cyl, drat, qsec, gear, carb       0.434  0.700     5
#>   [ 3] mpg, drat, vs, gear, carb         0.466  0.700     5
#>   [ 4] wt, qsec, am, carb                0.373  0.692     4
#>   [ 5] wt, qsec, gear, carb              0.388  0.656     4
#>   ... (10 more combinations)
```

Inspect subsets:

``` r

as.data.frame(results)[1:5, ]  # First 5 subsets
#>                      VarName01 VarName02 VarName03 VarName04 VarName05
#> Subset01 [avg=0.416]       mpg      drat      qsec      gear      carb
#> Subset02 [avg=0.434]       cyl      drat      qsec      gear      carb
#> Subset03 [avg=0.466]       mpg      drat        vs      gear      carb
#> Subset04 [avg=0.373]        wt      qsec        am      carb      <NA>
#> Subset05 [avg=0.388]        wt      qsec      gear      carb      <NA>
```

Extract a specific subset:

``` r

subset_data <- corrSubset(results, mtcars, which = 1)
names(subset_data)
#> [1] "mpg"  "drat" "qsec" "gear" "carb"
```

### assocSelect(): Mixed-Type Data

``` r

# Create mixed-type data
df <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  cat1 = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
  ord1 = ordered(sample(1:5, 100, replace = TRUE))
)

# Handle mixed types automatically
results_mixed <- assocSelect(df, threshold = 0.5)
show(results_mixed)
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: mixed
#>   AssocMethod: numeric_numeric = pearson, numeric_factor = eta, numeric_ordered
#>                = spearman, factor_ordered = cramersv
#>   Threshold:   0.500
#>   Subsets:     1 valid combinations
#>   Data Rows:   100 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] x1, x2, cat1, ord1                0.077  0.184     4
```

------------------------------------------------------------------------

## Protecting Variables

Use `force_in` to ensure specific variables are always retained:

``` r

# Force "mpg" to remain in all subsets
pruned_force <- corrPrune(
  data = mtcars,
  threshold = 0.7,
  force_in = "mpg"
)

# Verify forced variable is present
"mpg" %in% names(pruned_force)
#> [1] TRUE
```

------------------------------------------------------------------------

## Threshold Selection

Visualize correlation distribution to choose an appropriate threshold:

``` r

cor_mat <- cor(mtcars)
cor_vec <- cor_mat[upper.tri(cor_mat)]

hist(abs(cor_vec), breaks = 20,
     main = "Distribution of Absolute Correlations",
     xlab = "Absolute Correlation",
     col = rgb(0.2, 0.5, 0.8, 0.6),
     border = "white",
     xlim = c(0, 1))

# Threshold lines
abline(v = c(0.5, 0.7, 0.9),
       col = c("#d73027", "#4575b4", "#91cf60"),
       lwd = 2, lty = 2)

legend("topright",
       legend = c("0.5 (strict)", "0.7 (moderate)", "0.9 (lenient)"),
       col = c("#d73027", "#4575b4", "#91cf60"),
       lwd = 2, lty = 2,
       bty = "o",
       bg = "white")
```

![Histogram of absolute correlation distribution with three colored
vertical dashed lines indicating threshold levels: strict at 0.5 (red),
moderate at 0.7 (blue), and lenient at 0.9 (green). The distribution
helps users visualize correlation structure and choose an appropriate
threshold based on the data's correlation
characteristics.](quickstart_files/figure-html/unnamed-chunk-9-1.png)

------------------------------------------------------------------------

## Interface Selection Guide

| Scenario | Function | Key Parameters |
|----|----|----|
| Quick dimensionality reduction | [`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md) | `threshold`, `mode` |
| Model-based refinement | [`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md) | `limit` (VIF threshold), `engine` |
| Enumerate all valid subsets | [`corrSelect()`](https://gillescolling.com/corrselect/reference/corrSelect.md) | `threshold` |
| Mixed-type data | [`assocSelect()`](https://gillescolling.com/corrselect/reference/assocSelect.md) | `threshold` |
| Precomputed matrices | [`MatSelect()`](https://gillescolling.com/corrselect/reference/MatSelect.md) | `threshold`, `method` |
| Protect key variables | Any function | `force_in` |

------------------------------------------------------------------------

## See Also

- [`vignette("workflows")`](https://gillescolling.com/corrselect/articles/workflows.md) -
  Complete real-world workflows (ecological, survey, genomic, mixed
  models)
- [`vignette("advanced")`](https://gillescolling.com/corrselect/articles/advanced.md) -
  Algorithmic control and custom engines
- [`vignette("comparison")`](https://gillescolling.com/corrselect/articles/comparison.md) -
  Comparison with caret, Boruta, glmnet
- [`vignette("theory")`](https://gillescolling.com/corrselect/articles/theory.md) -
  Theoretical foundations and formulation
- [`?corrPrune`](https://gillescolling.com/corrselect/reference/corrPrune.md),
  [`?modelPrune`](https://gillescolling.com/corrselect/reference/modelPrune.md),
  [`?corrSelect`](https://gillescolling.com/corrselect/reference/corrSelect.md),
  [`?assocSelect`](https://gillescolling.com/corrselect/reference/assocSelect.md),
  [`?MatSelect`](https://gillescolling.com/corrselect/reference/MatSelect.md)
