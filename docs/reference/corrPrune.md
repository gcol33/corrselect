# Association-Based Predictor Pruning

`corrPrune()` performs model-free variable subset selection by
iteratively removing predictors until all pairwise associations fall
below a specified threshold. It returns a single pruned data frame with
predictors that satisfy the association constraint.

## Usage

``` r
corrPrune(
  data,
  threshold = 0.7,
  measure = "auto",
  mode = "auto",
  force_in = NULL,
  by = NULL,
  group_q = 1,
  max_exact_p = 20,
  ...
)
```

## Arguments

- data:

  A data.frame containing candidate predictors.

- threshold:

  Numeric scalar. Maximum allowed pairwise association (default: 0.7).
  Must be non-negative.

- measure:

  Character string specifying the association measure to use. Options:
  `"auto"` (default), `"pearson"`, `"spearman"`, `"kendall"`,
  `"cramersv"`, `"eta"`, etc. When `"auto"`, Pearson correlation is used
  for all-numeric data, and appropriate measures are selected for
  mixed-type data.

- mode:

  Character string specifying the search algorithm. Options:

  - `"auto"` (default): uses exact search if number of predictors \<=
    `max_exact_p`, otherwise uses greedy search

  - `"exact"`: exhaustive search for maximal subsets (may be slow for
    large p)

  - `"greedy"`: fast approximate search using iterative removal

- force_in:

  Character vector of variable names that must be retained in the final
  subset. Default: NULL.

- by:

  Character vector naming one or more grouping variables. If provided,
  associations are computed separately within each group, then
  aggregated using the quantile specified by `group_q`. Default: NULL
  (no grouping).

- group_q:

  Numeric scalar in (0, 1\]. Quantile used to aggregate associations
  across groups when `by` is provided. Default: 1 (maximum, ensuring
  threshold holds in all groups). Use 0.9 for 90th percentile, etc.

- max_exact_p:

  Integer. Maximum number of predictors for which exact mode is used
  when `mode = "auto"`. Default: 20.

- ...:

  Additional arguments (reserved for future use).

## Value

A data.frame containing the pruned subset of predictors. The result has
the following attributes:

- selected_vars:

  Character vector of retained variable names

- removed_vars:

  Character vector of removed variable names

- mode:

  Character string indicating which mode was used ("exact" or "greedy")

- measure:

  Character string indicating which association measure was used

- threshold:

  The threshold value used

## Details

`corrPrune()` identifies a subset of predictors whose pairwise
associations are all below `threshold`. The function works in several
stages:

1.  **Variable type detection**: Identifies numeric vs. categorical
    predictors

2.  **Association measurement**: Computes appropriate pairwise
    associations

3.  **Grouping (optional)**: If `by` is specified, computes associations
    within each group and aggregates using the specified quantile

4.  **Feasibility check**: Verifies that `force_in` variables satisfy
    the threshold constraint

5.  **Subset selection**: Uses either exact or greedy search to find a
    valid subset

**Grouped Pruning**: When `by` is provided, the function ensures the
selected predictors satisfy the threshold constraint across groups. For
example, with `group_q = 1` (default), the returned predictors will have
pairwise associations below `threshold` in *all* groups. With
`group_q = 0.9`, they will satisfy the constraint in at least 90% of
groups.

**Mode Selection**: Exact mode guarantees finding all maximal subsets
and returns the largest one (with deterministic tie-breaking). Greedy
mode is faster but approximate, using a deterministic removal strategy
based on association scores.

## See also

[`corrSelect`](https://gillescolling.com/corrselect/reference/corrSelect.md)
for exhaustive subset enumeration,
[`assocSelect`](https://gillescolling.com/corrselect/reference/assocSelect.md)
for mixed-type data subset enumeration,
[`modelPrune`](https://gillescolling.com/corrselect/reference/modelPrune.md)
for model-based predictor pruning.

## Examples

``` r
# Basic numeric data pruning
data(mtcars)
pruned <- corrPrune(mtcars, threshold = 0.7)
names(pruned)
#> [1] "mpg"  "drat" "qsec" "gear" "carb"

# Force certain variables to be included
pruned <- corrPrune(mtcars, threshold = 0.7, force_in = "mpg")

# Use greedy mode for faster computation
pruned <- corrPrune(mtcars, threshold = 0.7, mode = "greedy")
```
