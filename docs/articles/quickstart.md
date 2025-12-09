# Quick Start

## Installation

``` r

# Install from CRAN
install.packages("corrselect")

# Or install development version from GitHub
# install.packages("devtools")
devtools::install_github("GillesColling/corrselect")
```

**Suggested packages** (for extended functionality):

- `lme4`, `glmmTMB`: Mixed-effects models in
  [`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)
- `WGCNA`: Biweight midcorrelation (`bicor`)
- `energy`: Distance correlation
- `minerva`: Maximal information coefficient

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

## Quick Examples

### corrPrune(): Association-Based Pruning

``` r

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
#>   Subsets:     15 maximal subsets
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
#>   Subsets:     1 maximal subsets
#>   Data Rows:   100 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] x1, x2, cat1, ord1                0.077  0.184     4

# Verify all pairwise associations are below threshold
cat("Max pairwise association:", max(results_mixed@max_corr), "\n")
#> Max pairwise association: 0.1842642
```

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

## Threshold Selection

Common thresholds: **0.5** (strict), **0.7** (moderate, recommended
default), **0.9** (lenient).

Lower thresholds are stricter because they allow fewer variable pairs to
coexist, resulting in smaller subsets. Higher thresholds permit stronger
correlations, retaining more variables.

For detailed threshold selection strategies including visualization
techniques, VIF guidelines, and sensitivity analysis, see
[`vignette("advanced")`](https://gillescolling.com/corrselect/articles/advanced.md).

## Interface Selection Guide

| Scenario | Function | Key Parameters |
|----|----|----|
| Quick dimensionality reduction | [`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md) | `threshold`, `mode` |
| Model-based refinement | [`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md) | `limit` (VIF threshold), `engine` |
| Enumerate all maximal subsets | [`corrSelect()`](https://gillescolling.com/corrselect/reference/corrSelect.md) | `threshold` |
| Mixed-type data | [`assocSelect()`](https://gillescolling.com/corrselect/reference/assocSelect.md) | `threshold` |
| Precomputed matrices | [`MatSelect()`](https://gillescolling.com/corrselect/reference/MatSelect.md) | `threshold`, `method` |
| Protect key variables | Any function | `force_in` |

## Quick Reference

### corrPrune()

Removes redundant predictors based on pairwise correlation.

``` r

corrPrune(data, threshold = 0.7, measure = "auto", mode = "auto",
          force_in = NULL, by = NULL, group_q = 1, max_exact_p = 100)
```

| Parameter | Description | Default |
|----|----|----|
| `data` | Data frame or matrix | *required* |
| `threshold` | Maximum allowed correlation | `0.7` |
| `measure` | Correlation type: `"auto"`, `"pearson"`, `"spearman"`, `"kendall"` | `"auto"` |
| `mode` | Algorithm: `"auto"`, `"exact"`, `"greedy"` | `"auto"` |
| `force_in` | Variables that must be retained | `NULL` |

**Returns**: Data frame with pruned variables. Attributes:
`selected_vars`, `removed_vars`.

### modelPrune()

Iteratively removes predictors with high VIF from a regression model.

``` r

modelPrune(formula, data, engine = "lm", criterion = "vif",
           limit = 5, force_in = NULL, max_steps = NULL, ...)
```

| Parameter  | Description                                       | Default    |
|------------|---------------------------------------------------|------------|
| `formula`  | Model formula (e.g., `y ~ .`)                     | *required* |
| `data`     | Data frame                                        | *required* |
| `engine`   | `"lm"`, `"glm"`, `"lme4"`, `"glmmTMB"`, or custom | `"lm"`     |
| `limit`    | Maximum allowed VIF                               | `5`        |
| `force_in` | Variables that must be retained                   | `NULL`     |

**Returns**: Pruned data frame. Attributes: `selected_vars`,
`removed_vars`, `final_model`.

### corrSelect()

Enumerates all maximal subsets satisfying correlation threshold (numeric
data).

``` r

corrSelect(df, threshold = 0.7, method = NULL, force_in = NULL,
           cor_method = "pearson", ...)
```

| Parameter | Description | Default |
|----|----|----|
| `df` | Data frame (numeric columns only) | *required* |
| `threshold` | Maximum allowed correlation | `0.7` |
| `method` | Algorithm: `"bron-kerbosch"`, `"els"` | auto |
| `cor_method` | `"pearson"`, `"spearman"`, `"kendall"`, `"bicor"`, `"distance"`, `"maximal"` | `"pearson"` |
| `force_in` | Variables required in all subsets | `NULL` |

**Returns**: `CorrCombo` S4 object with slots: `subset_list`,
`avg_corr`, `min_corr`, `max_corr`.

### assocSelect()

Enumerates all maximal subsets for mixed-type data (numeric, factor,
ordered).

``` r

assocSelect(df, threshold = 0.7, method = NULL, force_in = NULL,
            method_num_num = "pearson", method_num_ord = "spearman",
            method_ord_ord = "spearman", ...)
```

| Parameter | Description | Default |
|----|----|----|
| `df` | Data frame (any column types) | *required* |
| `threshold` | Maximum allowed association | `0.7` |
| `method_num_num` | Numeric-numeric: `"pearson"`, `"spearman"`, etc. | `"pearson"` |
| `method_num_ord` | Numeric-ordered: `"spearman"`, `"kendall"` | `"spearman"` |
| `method_ord_ord` | Ordered-ordered: `"spearman"`, `"kendall"` | `"spearman"` |

**Returns**: `CorrCombo` S4 object.

### MatSelect()

Direct matrix interface for precomputed correlation/association
matrices.

``` r

MatSelect(mat, threshold = 0.7, method = NULL, force_in = NULL, ...)
```

| Parameter   | Description                              | Default    |
|-------------|------------------------------------------|------------|
| `mat`       | Symmetric correlation/association matrix | *required* |
| `threshold` | Maximum allowed value                    | `0.7`      |
| `method`    | Algorithm: `"bron-kerbosch"`, `"els"`    | auto       |
| `force_in`  | Variables required in all subsets        | `NULL`     |

**Returns**: `CorrCombo` S4 object.

### corrSubset()

Extracts a specific subset from a `CorrCombo` result.

``` r

corrSubset(res, df, which = "best", keepExtra = FALSE)
```

| Parameter | Description | Default |
|----|----|----|
| `res` | `CorrCombo` object from `corrSelect`/`assocSelect`/`MatSelect` | *required* |
| `df` | Original data frame | *required* |
| `which` | Subset index or `"best"` (lowest avg correlation) | `"best"` |
| `keepExtra` | Include non-numeric columns in output? | `FALSE` |

**Returns**: Data frame containing only the selected variables.

## Troubleshooting

**“No valid subsets found” error** - Threshold too strict—all variable
pairs exceed it - Solution: Increase threshold or use `force_in` to keep
at least one variable

**VIF computation fails in modelPrune()** - Perfect multicollinearity
(R² = 1) present - Solution: Use `corrPrune(threshold = 0.99)` first to
remove near-duplicates

**Forced variables conflict** - Variables in `force_in` are too highly
correlated with each other - Solution: Increase threshold or reduce
`force_in` set

**Slow performance with many variables** - Exact mode is exponential for
large p - Solution: Use `mode = "greedy"` for p \> 25

For comprehensive troubleshooting with code examples, see
[`vignette("advanced")`](https://gillescolling.com/corrselect/articles/advanced.md),
Section 5.

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

## Session Info

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
#> [1] corrselect_3.0.2
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.37     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
#>  [5] xfun_0.53         cachem_1.1.0      knitr_1.50        htmltools_0.5.8.1
#>  [9] rmarkdown_2.30    lifecycle_1.0.4   cli_3.6.5         svglite_2.2.2    
#> [13] sass_0.4.10       pkgdown_2.2.0     textshaping_1.0.3 jquerylib_0.1.4  
#> [17] systemfonts_1.3.1 compiler_4.5.2    tools_4.5.2       bslib_0.9.0      
#> [21] evaluate_1.0.5    Rcpp_1.1.0        yaml_2.3.10       jsonlite_2.0.0   
#> [25] rlang_1.1.6       fs_1.6.6          htmlwidgets_1.6.4
```
