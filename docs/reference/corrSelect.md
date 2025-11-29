# Select Variable Subsets with Low Correlation (Data Frame Interface)

Identifies combinations of numeric variables in a data frame such that
all pairwise absolute correlations fall below a specified threshold.
This function is a wrapper around
[`MatSelect()`](https://gillescolling.com/corrselect/reference/MatSelect.md)
and accepts data frames, tibbles, or data tables with automatic
preprocessing.

## Usage

``` r
corrSelect(
  df,
  threshold = 0.7,
  method = NULL,
  force_in = NULL,
  cor_method = c("pearson", "spearman", "kendall", "bicor", "distance", "maximal"),
  ...
)
```

## Arguments

- df:

  A data frame. Only numeric columns are used.

- threshold:

  A numeric value in (0, 1). Maximum allowed absolute correlation.
  Defaults to 0.7.

- method:

  Character. Selection algorithm to use. One of `"els"` or
  `"bron-kerbosch"`. If not specified, the function chooses
  automatically: `"els"` when `force_in` is provided, otherwise
  `"bron-kerbosch"`.

- force_in:

  Optional character vector or numeric indices of columns to force into
  all subsets.

- cor_method:

  Character string indicating which correlation method to use. One of
  `"pearson"` (default), `"spearman"`, `"kendall"`, `"bicor"`,
  `"distance"`, or `"maximal"`.

- ...:

  Additional arguments passed to
  [`MatSelect()`](https://gillescolling.com/corrselect/reference/MatSelect.md),
  e.g., `use_pivot`.

## Value

An object of class
[`CorrCombo`](https://gillescolling.com/corrselect/reference/CorrCombo.md),
containing selected subsets and correlation statistics.

## Details

Only numeric columns are used for correlation analysis. Non‐numeric
columns (factors, characters, logicals, etc.) are ignored, and their
names and types are printed to inform the user. These can be optionally
reattached later using
[`corrSubset()`](https://gillescolling.com/corrselect/reference/corrSubset.md)
with `keepExtra = TRUE`.

Rows with missing values are removed before computing correlations. A
warning is issued if any rows are dropped.

The `cor_method` controls how the correlation matrix is computed:

- `"pearson"`: Standard linear correlation.

- `"spearman"`: Rank-based monotonic correlation.

- `"kendall"`: Kendall's tau.

- `"bicor"`: Biweight midcorrelation (WGCNA::bicor).

- `"distance"`: Distance correlation (energy::dcor).

- `"maximal"`: Maximal information coefficient (minerva::mine).

For `"bicor"`, `"distance"`, and `"maximal"`, the corresponding package
must be installed.

## See also

[`assocSelect()`](https://gillescolling.com/corrselect/reference/assocSelect.md),
[`MatSelect()`](https://gillescolling.com/corrselect/reference/MatSelect.md),
[`corrSubset()`](https://gillescolling.com/corrselect/reference/corrSubset.md)

## Examples

``` r
set.seed(42)
n <- 100

# Create 20 variables: 5 blocks of correlated variables + some noise
block1 <- matrix(rnorm(n * 4), ncol = 4)
block2 <- matrix(rnorm(n), ncol = 1)
block2 <- matrix(rep(block2, 4), ncol = 4) + matrix(rnorm(n * 4, sd = 0.1), ncol = 4)
block3 <- matrix(rnorm(n * 4), ncol = 4)
block4 <- matrix(rnorm(n * 4), ncol = 4)
block5 <- matrix(rnorm(n * 4), ncol = 4)

df <- as.data.frame(cbind(block1, block2, block3, block4, block5))
colnames(df) <- paste0("V", 1:20)

# Add a non-numeric column to be ignored
df$label <- factor(sample(c("A", "B"), n, replace = TRUE))

# Basic usage
corrSelect(df, threshold = 0.8)
#> The following variables were excluded from the correlation analysis:
#>   - label: unordered factor (excluded)
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: pearson
#>   Threshold:   0.800
#>   Subsets:     4 valid combinations
#>   Data Rows:   100 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] V1, V2, V3, V4, V7, V9, ...       0.075  0.241    17
#>   [ 2] V1, V2, V3, V4, V6, V9, ...       0.075  0.259    17
#>   [ 3] V1, V2, V3, V4, V8, V9, ...       0.075  0.269    17
#>   [ 4] V1, V2, V3, V4, V5, V9, ...       0.076  0.288    17

# Try Bron–Kerbosch with pivoting
corrSelect(df, threshold = 0.6, method = "bron-kerbosch", use_pivot = TRUE)
#> The following variables were excluded from the correlation analysis:
#>   - label: unordered factor (excluded)
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: pearson
#>   Threshold:   0.600
#>   Subsets:     4 valid combinations
#>   Data Rows:   100 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] V1, V2, V3, V4, V7, V9, ...       0.075  0.241    17
#>   [ 2] V1, V2, V3, V4, V6, V9, ...       0.075  0.259    17
#>   [ 3] V1, V2, V3, V4, V8, V9, ...       0.075  0.269    17
#>   [ 4] V1, V2, V3, V4, V5, V9, ...       0.076  0.288    17

# Force in a specific variable and use Spearman correlation
corrSelect(df, threshold = 0.6, force_in = "V10", cor_method = "spearman")
#> The following variables were excluded from the correlation analysis:
#>   - label: unordered factor (excluded)
#> CorrCombo object
#> -----------------
#>   Method:      els
#>   Correlation: spearman
#>   Threshold:   0.600
#>   Subsets:     4 valid combinations
#>   Data Rows:   100 used in correlation
#>   Forced-in:   V10
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] V1, V2, V3, V4, V7, V9, ...       0.076  0.239    17
#>   [ 2] V1, V2, V3, V4, V5, V9, ...       0.076  0.269    17
#>   [ 3] V1, V2, V3, V4, V8, V9, ...       0.076  0.246    17
#>   [ 4] V1, V2, V3, V4, V6, V9, ...       0.076  0.252    17
```
