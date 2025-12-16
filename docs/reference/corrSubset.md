# Extract Variable Subsets from a CorrCombo Object

Extracts one or more variable subsets from a
[`CorrCombo`](https://gillescolling.com/corrselect/reference/CorrCombo.md)
object as data frames. Typically used after
[`corrSelect`](https://gillescolling.com/corrselect/reference/corrSelect.md)
or
[`MatSelect`](https://gillescolling.com/corrselect/reference/MatSelect.md)
to obtain filtered versions of the original dataset containing only
low‐correlation variable combinations.

## Usage

``` r
corrSubset(res, df, which = "best", keepExtra = FALSE)
```

## Arguments

- res:

  A
  [`CorrCombo`](https://gillescolling.com/corrselect/reference/CorrCombo.md)
  object returned by `corrSelect` or `MatSelect`.

- df:

  A data frame or matrix. Must contain all variables listed in
  `res@names`. Columns not in `res@names` are ignored unless
  `keepExtra = TRUE`.

- which:

  Subsets to extract. One of:

  - `"best"` (default) or `1`: the top‐ranked subset.

  - A single integer (e.g. `2`): the nth ranked subset.

  - A vector of integers (e.g. `1:3`): multiple subsets.

  - `"all"`: all available subsets.

  Subsets are ranked by decreasing size, then increasing average
  correlation.

- keepExtra:

  Logical. If `TRUE`, columns in `df` not in `res@names` (e.g., factors,
  characters) are retained. Defaults to `FALSE`.

## Value

A data frame if a single subset is extracted, or a list of data frames
if multiple subsets are extracted. Each data frame contains the selected
variables (and optionally extras).

## Note

A warning is issued if any rows contain missing values in the selected
variables.

## See also

[`corrSelect`](https://gillescolling.com/corrselect/reference/corrSelect.md),
[`MatSelect`](https://gillescolling.com/corrselect/reference/MatSelect.md),
[`CorrCombo`](https://gillescolling.com/corrselect/reference/CorrCombo.md)

## Examples

``` r
# Simulate input data
set.seed(123)
df <- as.data.frame(matrix(rnorm(100), nrow = 10))
colnames(df) <- paste0("V", 1:10)

# Compute correlation matrix
cmat <- cor(df)

# Select subsets using corrSelect
res <- corrSelect(cmat, threshold = 0.5)

# Extract the best subset (default)
corrSubset(res, df)

# Extract the second-best subset
corrSubset(res, df, which = 2)

# Extract the first three subsets
corrSubset(res, df, which = 1:3)

# Extract all subsets
corrSubset(res, df, which = "all")

# Extract best subset and retain additional numeric column
df$CopyV1 <- df$V1
corrSubset(res, df, which = 1, keepExtra = TRUE)
```
