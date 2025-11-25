# CorrCombo S4 class

Holds the result of
[`corrSelect`](https://gcol33.github.io/corrselect/reference/corrSelect.md)
or
[`MatSelect`](https://gcol33.github.io/corrselect/reference/MatSelect.md):
a list of valid variable combinations and their correlation statistics.

This class stores all subsets of variables that meet the specified
correlation constraint, along with metadata such as the algorithm used,
correlation method(s), variables forced into every subset, and summary
statistics for each combination.

## Usage

``` r
# S4 method for class 'CorrCombo'
show(object)
```

## Arguments

- object:

  A `CorrCombo` object to be printed.

## Slots

- `subset_list`:

  A list of character vectors. Each vector is a valid subset (variable
  names).

- `avg_corr`:

  A numeric vector. Average absolute correlation within each subset.

- `min_corr`:

  A numeric vector. Minimum pairwise absolute correlation in each
  subset.

- `max_corr`:

  A numeric vector. Maximum pairwise absolute correlation within each
  subset.

- `names`:

  Character vector of all variable names used for decoding.

- `threshold`:

  Numeric scalar. The correlation threshold used during selection.

- `forced_in`:

  Character vector. Variable names that were forced into each subset.

- `search_type`:

  Character string. One of `"els"` or `"bron-kerbosch"`.

- `cor_method`:

  Character string. Either a single method (e.g. "pearson") or "mixed"
  if multiple methods used.

- `n_rows_used`:

  Integer. Number of rows used for computing the correlation matrix
  (after removing missing values).

## See also

[`corrSelect`](https://gcol33.github.io/corrselect/reference/corrSelect.md),
[`MatSelect`](https://gcol33.github.io/corrselect/reference/MatSelect.md),
[`corrSubset`](https://gcol33.github.io/corrselect/reference/corrSubset.md)

## Examples

``` r
show(new("CorrCombo",
  subset_list = list(c("A", "B"), c("A", "C")),
  avg_corr = c(0.2, 0.3),
  min_corr = c(0.1, 0.2),
  max_corr = c(0.3, 0.4),
  names = c("A", "B", "C"),
  threshold = 0.5,
  forced_in = character(),
  search_type = "els",
  cor_method = "mixed",
  n_rows_used = as.integer(5)
))
#> CorrCombo object
#> -----------------
#>   Method:      els
#>   Correlation: mixed
#>   Threshold:   0.500
#>   Subsets:     2 valid combinations
#>   Data Rows:   5 used in correlation
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] A, B                              0.200  0.300     2
#>   [ 2] A, C                              0.300  0.400     2
```
