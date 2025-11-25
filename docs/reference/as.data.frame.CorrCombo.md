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
#>                      VarName01 VarName02 VarName03
#> Subset01 [avg=0.261]        V3        V7       V10
#> Subset02 [avg=0.295]        V1        V7       V10
#> Subset03 [avg=0.300]        V8        V9       V10
#> Subset04 [avg=0.331]        V2        V7       V10
#> Subset05 [avg=0.343]        V9        V7       V10
#> Subset06 [avg=0.350]        V3        V8       V10
#> Subset07 [avg=0.374]        V2        V8       V10
#> Subset08 [avg=0.384]        V6        V7       V10
#> Subset09 [avg=0.388]        V6        V8       V10
#> Subset10 [avg=0.086]        V3        V4      <NA>
#> Subset11 [avg=0.098]        V5        V8      <NA>
#> Subset12 [avg=0.208]        V1        V4      <NA>
#> Subset13 [avg=0.220]        V2        V4      <NA>
#> Subset14 [avg=0.295]        V9        V4      <NA>
#> Subset15 [avg=0.319]        V6        V4      <NA>
#> Subset16 [avg=0.407]        V5        V7      <NA>
```
