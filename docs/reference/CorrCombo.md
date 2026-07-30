# CorrCombo Class

Holds the result of
[`corrSelect`](https://gillescolling.com/corrselect/reference/corrSelect.md)
or
[`MatSelect`](https://gillescolling.com/corrselect/reference/MatSelect.md):
a list of valid variable combinations and their correlation statistics.

This class stores all subsets of variables that meet the specified
correlation constraint, along with metadata such as the algorithm used,
correlation method(s), variables forced into every subset, and summary
statistics for each combination.

## Usage

``` r
CorrCombo(
  subset_list = list(),
  avg_corr = numeric(0),
  min_corr = numeric(0),
  max_corr = numeric(0),
  var_names = character(0),
  threshold = numeric(0),
  forced_in = character(0),
  search_type = character(0),
  cor_method = character(0),
  n_rows_used = integer(0)
)

# S3 method for class 'CorrCombo'
print(x, ...)

# S3 method for class 'CorrCombo'
summary(object, ...)

# S3 method for class 'summary.CorrCombo'
print(x, ...)
```

## Arguments

- subset_list:

  A list of character vectors. Each vector is a valid subset (variable
  names).

- avg_corr:

  A numeric vector. Average absolute correlation within each subset.

- min_corr:

  A numeric vector. Minimum pairwise absolute correlation in each
  subset.

- max_corr:

  A numeric vector. Maximum pairwise absolute correlation within each
  subset.

- var_names:

  Character vector of all variable names used for decoding.

- threshold:

  Numeric scalar. The correlation threshold used during selection.

- forced_in:

  Character vector. Variable names forced into each subset. Defaults to
  [`character()`](https://rdrr.io/r/base/character.html).

- search_type:

  Character string. One of `"els"` or `"bron-kerbosch"`.

- cor_method:

  Character string. Correlation method or `"mixed"`. Defaults to
  [`character()`](https://rdrr.io/r/base/character.html).

- n_rows_used:

  Integer. Number of rows used for computing the correlation matrix.
  `NA` for matrix input.

- x:

  A `summary.CorrCombo` object to be printed.

- ...:

  Additional arguments (ignored).

- object:

  A `CorrCombo` object to summarize.

## Value

`CorrCombo()` returns an S7 `CorrCombo` object holding the discovered
subsets and their correlation statistics, with properties `subset_list`,
`avg_corr`, `min_corr`, `max_corr`, `var_names`, `threshold`,
`forced_in`, `search_type`, `cor_method`, and `n_rows_used` as described
above.

`print.CorrCombo()` returns `x`, invisibly. Called to print a formatted
summary of a `CorrCombo` object to the console.

`summary.CorrCombo()` returns a list of class `summary.CorrCombo` with
10 elements:

- n_subsets:

  Integer. Number of maximal subsets found.

- search_type:

  Character string. One of `"els"` or `"bron-kerbosch"`.

- cor_method:

  Character string. Correlation method used, or `"mixed"` if multiple
  methods were used.

- threshold:

  Numeric scalar. The correlation threshold used during selection.

- n_rows_used:

  Integer. Number of rows used to compute the correlation matrix, or
  `NA` for matrix input.

- forced_in:

  Character vector. Variable names forced into every subset.

- size_range:

  Integer vector of length 2 giving the smallest and largest subset
  sizes found (`c(NA, NA)` if no subsets were found).

- size_median:

  Numeric scalar. Median subset size across all discovered subsets (`NA`
  if none found).

- avg_corr_range:

  Numeric vector of length 2 giving the smallest and largest average
  absolute correlation across subsets (`c(NA, NA)` if none found).

- n_max_size:

  Integer. Number of subsets that attain the largest size (`0` if none
  found).

`print.summary.CorrCombo()` returns `x`, invisibly. Called to print a
formatted `summary.CorrCombo` object to the console.

## Details

Properties:

- subset_list:

  A list of character vectors. Each vector is a valid subset (variable
  names).

- avg_corr:

  A numeric vector. Average absolute correlation within each subset.

- min_corr:

  A numeric vector. Minimum pairwise absolute correlation in each
  subset.

- max_corr:

  A numeric vector. Maximum pairwise absolute correlation within each
  subset.

- var_names:

  Character vector of all variable names used for decoding.

- threshold:

  Numeric scalar. The correlation threshold used during selection.

- forced_in:

  Character vector. Variable names that were forced into each subset.

- search_type:

  Character string. One of `"els"` or `"bron-kerbosch"`.

- cor_method:

  Character string. Either a single method (e.g. "pearson") or "mixed"
  if multiple methods used.

- n_rows_used:

  Integer. Number of rows used for computing the correlation matrix
  (after removing missing values). `NA` when constructed from a matrix
  directly (e.g. via
  [`MatSelect`](https://gillescolling.com/corrselect/reference/MatSelect.md)),
  since a matrix input has no associated row count.

## See also

[`corrSelect`](https://gillescolling.com/corrselect/reference/corrSelect.md),
[`MatSelect`](https://gillescolling.com/corrselect/reference/MatSelect.md),
[`corrSubset`](https://gillescolling.com/corrselect/reference/corrSubset.md)

## Examples

``` r
print(CorrCombo(
  subset_list = list(c("A", "B"), c("A", "C")),
  avg_corr = c(0.2, 0.3),
  min_corr = c(0.1, 0.2),
  max_corr = c(0.3, 0.4),
  var_names = c("A", "B", "C"),
  threshold = 0.5,
  forced_in = character(),
  search_type = "els",
  cor_method = "mixed",
  n_rows_used = 5L
))
```
