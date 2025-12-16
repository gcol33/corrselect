# Coerce CorrCombo to a Data Frame

Converts a `CorrCombo` object into a data frame of variable
combinations.

## Usage

``` r
# S3 method for class 'CorrCombo'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `CorrCombo` object.

- row.names:

  Optional row names for the output data frame.

- optional:

  Logical. Passed to
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

- ...:

  Additional arguments passed to
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

## Value

A data frame where each row corresponds to a subset of variables.
Columns are named `VarName01`, `VarName02`, ..., up to the size of the
largest subset. Subsets shorter than the maximum length are padded with
`NA`.

## See also

[`CorrCombo`](https://gillescolling.com/corrselect/reference/CorrCombo.md)

## Examples

``` r
set.seed(1)
mat <- matrix(rnorm(100), ncol = 10)
colnames(mat) <- paste0("V", 1:10)
res <- corrSelect(cor(mat), threshold = 0.5)
as.data.frame(res)
```
