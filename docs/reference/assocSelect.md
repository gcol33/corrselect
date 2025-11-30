# Select Variable Subsets with Low Association (Mixed-Type Data Frame Interface)

Identifies combinations of variables of any common data type (numeric,
ordered factors, or unordered) factors—whose *pair-wise association*
does not exceed a user-supplied threshold. The routine wraps
[`MatSelect()`](https://gillescolling.com/corrselect/reference/MatSelect.md)
and handles all pre-processing (type conversion, missing-row removal,
constant-column checks) for typical data-frame/tibble/data-table inputs.

## Usage

``` r
assocSelect(
  df,
  threshold = 0.7,
  method = NULL,
  force_in = NULL,
  method_num_num = c("pearson", "spearman", "kendall", "bicor", "distance", "maximal"),
  method_num_ord = c("spearman", "kendall"),
  method_ord_ord = c("spearman", "kendall"),
  ...
)
```

## Arguments

- df:

  A data frame (or tibble / data.table). May contain any mix of:

  - numeric / integer (treated as numeric)

  - ordered factors

  - unordered factors (character vectors are coerced to factors)

- threshold:

  Numeric in \\(0,1)\\. Maximum allowed pair-wise *absolute*
  association. Default `0.7`.

- method:

  Character; the subset-search algorithm. One of `"els"` or
  `"bron-kerbosch"`. If `NULL` (default) the function selects
  automatically: ELS when `force_in` is supplied, otherwise
  Bron–Kerbosch.

- force_in:

  Optional character vector or column indices specifying variables that
  must appear in every returned subset.

- method_num_num:

  Association measure for numeric–numeric pairs. One of `"pearson"`
  (default), `"spearman"`, `"kendall"`, `"bicor"`, `"distance"`, or
  `"maximal"`.

- method_num_ord:

  Association measure for numeric–ordered pairs. One of `"spearman"`
  (default) or `"kendall"`.

- method_ord_ord:

  Association measure for ordered–ordered pairs. One of `"spearman"`
  (default) or `"kendall"`.

- ...:

  Additional arguments passed unchanged to
  [`MatSelect()`](https://gillescolling.com/corrselect/reference/MatSelect.md)
  (e.g., `use_pivot = TRUE` for Bron–Kerbosch).

## Value

A
[`CorrCombo`](https://gillescolling.com/corrselect/reference/CorrCombo.md)
S4 object containing:

- all valid subsets,

- their summary association statistics,

- metadata (algorithm used, rows kept, forced-in variables, etc.).

The object’s `show()` method prints the association metrics that were
*actually used* for this data set.

## Details

A single call can therefore screen a data set that mixes continuous and
categorical features and return every subset whose internal associations
are “sufficiently low” under the metric(s) you choose.

Rows containing `NA` are dropped with a warning; constant columns are
treated as having zero association with every other variable.

The default association measure for each variable-type combination is:

- numeric – numeric:

  `method_num_num` (default `"pearson"`)

- numeric – ordered:

  `method_num_ord`

- numeric – unordered:

  `"eta"` (ANOVA \\\eta^{2}\\)

- ordered – ordered:

  `method_ord_ord`

- ordered – unordered:

  `"cramersv"`

- unordered – unordered:

  `"cramersv"`

All association measures are rescaled to \\\[0,1\]\\ before
thresholding. External packages are required for `"bicor"` (WGCNA),
`"distance"` (energy), and `"maximal"` (minerva); an informative error
is thrown if they are missing.

## See also

[`corrSelect()`](https://gillescolling.com/corrselect/reference/corrSelect.md),
[`MatSelect()`](https://gillescolling.com/corrselect/reference/MatSelect.md),
[`corrSubset()`](https://gillescolling.com/corrselect/reference/corrSubset.md)

## Examples

``` r
set.seed(42)
df <- data.frame(
  height = rnorm(15, 170, 10),
  weight = rnorm(15, 70, 12),
  group  = factor(rep(LETTERS[1:3], each = 5)),
  score  = ordered(sample(c("low","med","high"), 15, TRUE))
)

## keep every subset whose internal associations <= 0.6
assocSelect(df, threshold = 0.6)
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: mixed
#>   AssocMethod: numeric_numeric = pearson, numeric_factor = eta, numeric_ordered
#>                = spearman, factor_ordered = cramersv
#>   Threshold:   0.600
#>   Subsets:     1 maximal subsets
#>   Data Rows:   15 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] height, weight, group, score      0.219  0.475     4

## use Kendall for all rank-based comparisons and force 'height' to appear
assocSelect(df,
            threshold       = 0.5,
            method_num_num  = "kendall",
            method_num_ord  = "kendall",
            method_ord_ord  = "kendall",
            force_in        = "height")
#> CorrCombo object
#> -----------------
#>   Method:      els
#>   Correlation: mixed
#>   AssocMethod: numeric_numeric = kendall, numeric_factor = eta, numeric_ordered
#>                = kendall, factor_ordered = cramersv
#>   Threshold:   0.500
#>   Subsets:     1 maximal subsets
#>   Data Rows:   15 used in correlation
#>   Forced-in:   height
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] height, weight, group, score      0.193  0.365     4
```
