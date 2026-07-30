# Select Variable Subsets with Low Correlation or Association (Matrix Interface)

Identifies all maximal subsets of variables from a symmetric matrix
(typically a correlation matrix) such that all pairwise absolute values
stay below a specified threshold. Implements exact algorithms such as
Eppstein–Löffler–Strash (ELS) and Bron–Kerbosch (with or without
pivoting).

## Usage

``` r
MatSelect(mat, threshold = 0.7, method = NULL, force_in = NULL, ...)
```

## Arguments

- mat:

  A numeric, symmetric matrix with 1s on the diagonal (e.g. correlation
  matrix). Column names (if present) are used to label output variables.

- threshold:

  A numeric scalar in (0, 1\]. Maximum allowed absolute pairwise value.
  Defaults to `0.7`.

- method:

  Character. Selection algorithm to use. One of `"els"` or
  `"bron-kerbosch"`. If not specified, the function chooses
  automatically: `"els"` when `force_in` is provided, otherwise
  `"bron-kerbosch"`.

- force_in:

  Optional integer vector of 1-based column indices to force into every
  subset. If the forced variables are themselves mutually correlated
  beyond `threshold`, `MatSelect()` warns (naming the offending pair)
  but still forces them into every returned subset – unlike
  [`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md),
  which treats this condition as infeasible and errors instead.

- ...:

  Additional arguments passed to the backend. The only supported
  argument is `use_pivot` (logical), for enabling pivoting in
  Bron–Kerbosch (ignored by ELS); any other named argument is an error.

## Value

An object of class
[`CorrCombo`](https://gillescolling.com/corrselect/reference/CorrCombo.md),
containing all valid subsets and their correlation statistics. If every
variable is pairwise correlated above `threshold`, the only valid
maximal subsets are single variables; these are returned with
`min_corr`/`max_corr` set to `NA` (there is no pair to summarize).

## Examples

``` r
set.seed(42)
mat <- matrix(rnorm(100), ncol = 10)
colnames(mat) <- paste0("V", 1:10)
cmat <- cor(mat)

# Default method (Bron-Kerbosch)
res1 <- MatSelect(cmat, threshold = 0.5)

# Bron–Kerbosch without pivot
res2 <- MatSelect(cmat, threshold = 0.5, method = "bron-kerbosch", use_pivot = FALSE)

# Bron–Kerbosch with pivoting
res3 <- MatSelect(cmat, threshold = 0.5, method = "bron-kerbosch", use_pivot = TRUE)

# Force variables 1 and 2 into every subset (warns if they are mutually
# correlated beyond the threshold; both are still forced in regardless)
res4 <- MatSelect(cmat, threshold = 0.5, force_in = c(1, 2))
```
