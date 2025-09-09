# corrselect

**Exhaustive, Model-Agnostic Variable Subset Selection Based on Pairwise Correlation or Association**

The `corrselect` package automatically identifies all *maximal subsets* of variables in your data whose pairwise correlations or associations remain below a user-defined threshold. This helps reduce multicollinearity and redundancy while retaining interpretability. The method is **model-agnostic**, making it applicable to regression, clustering, ecological modeling, and other workflows.

## Statement of Need

Variable selection is a central task in statistics and machine learning, particularly when working with high-dimensional or collinear data. In many applications, users aim to retain sets of variables that are weakly associated with one another to avoid redundancy and reduce overfitting. Common approaches such as greedy filtering or regularized regression either discard useful features or do not guarantee bounded pairwise associations.

This package addresses the **admissible set problem**: selecting all maximal subsets of variables such that no pair exceeds a user-defined threshold. It generalizes to mixed-type data, supports multiple association metrics, and allows constrained subset selection via `force_in` (e.g. always include key predictors).

These features make the package useful in domains like:

- ecological and bioclimatic modeling,
- trait-based species selection,
- interpretable machine learning pipelines.

## Features

- Exhaustive and **exact** subset enumeration using graph algorithms:  
  - Eppstein–Löffler–Strash (ELS)  
  - Bron–Kerbosch (with optional pivoting)


- Supports multiple correlation/association metrics:
  - `"pearson"`, `"spearman"`, `"kendall"`
  - `"bicor"` (WGCNA), `"distance"` (energy), `"maximal"` (minerva)
  - `"eta"`, `"cramersv"` for mixed-type associations


- Works with:
  - data frames (`corrSelect()` and `assocSelect()`),
  - correlation matrices (`MatSelect()`)


- Mixed-type support via `assocSelect()`:
  - numeric–factor → Eta squared
  - numeric–ordered → Spearman/Kendall
  - factor–factor → Cramér’s V
  - ordered–ordered → Spearman/Kendall

- `force_in`: specify variables that must be included in every subset


- Returns an extensible `CorrCombo` S4 object with:
  - subset metadata,
  - correlation/association summaries,
  - custom `show()` and `as.data.frame()` methods

## Installation

```r
# Install from GitHub
remotes::install_github("gcol33/corrselect")
```

## Basic Usage

```r
library(corrselect)

# Simulated numeric example
set.seed(1)
n <- 100
df <- data.frame(
  A = rnorm(n),
  B = rnorm(n),
  C = rnorm(n),
  D = rnorm(n),
  E = rnorm(n)
)
df$F <- df$A * 0.9 + rnorm(n, sd = 0.1)

# Find all maximal subsets with pairwise Pearson correlation <= 0.7
res <- corrSelect(df, threshold = 0.7)
show(res)

# Extract the top-ranked subset from the original data
subset1 <- corrSubset(res, df, which = 1)
head(subset1)

# Convert all subsets to a tidy data frame
as.data.frame(res)
```

## Mixed Data Frames

Use `assocSelect()` for data frames with numeric, factor, or ordered variables. The function automatically selects the appropriate metric for each pair:

```r
df2 <- data.frame(
  height = rnorm(30, 170, 10),
  weight = rnorm(30, 70, 12),
  group  = factor(sample(c("A","B"), 30, TRUE)),                # unordered
  rating = ordered(sample(c("low","med","high"), 30, TRUE))     # ordered
)

# Select variable subsets with association <= 0.6
res2 <- assocSelect(df2, threshold = 0.6)
show(res2)
```

## Example Output

```
CorrCombo object
-----------------
  Method:      bron-kerbosch
  Correlation: mixed
  Threshold:   0.600
  Subsets:     2 valid combinations
  Data Rows:   30 used in correlation
  Pivot:       TRUE
  AssocMethod: numeric_numeric  = pearson,
               numeric_factor   = eta,
               numeric_ordered  = spearman

Top combinations:
  No.  Variables                          Avg    Max    Size
  ------------------------------------------------------------
  [ 1] height, rating                    0.311  0.562     2
  [ 2] weight, rating                    0.317  0.580     2
```

## Advanced Use

To use a precomputed correlation matrix (e.g. with MIC or custom metrics):

```r
mat <- cor(df)
res <- MatSelect(mat, threshold = 0.7, method = "els", force_in = 1)
```

You can also extract the full list of subsets and use `corrSubset()` to apply them to your data with or without additional columns.

## License

MIT (see the LICENSE.md file)
