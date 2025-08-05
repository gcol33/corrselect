# corrselect

**Exhaustive, Model-Agnostic Variable Subset Selection Based on Pairwise Correlation**

`corrselect` automatically identifies all maximal subsets of numeric variables in your data whose pairwise correlations remain below a specified threshold. This reduces multicollinearity and redundancy before modeling, while retaining interpretability. The approach is model-agnostic: it operates purely on the data’s correlation structure, making it suitable for regression, clustering, and other analyses.

## Features

- Exhaustive, exact subset enumeration using graph algorithms (ELS and Bron–Kerbosch).  
- Multiple correlation measures: `"pearson"`, `"spearman"`, `"kendall"`, `"bicor"`, `"distance"`, `"maximal"`.  
- Works with data frames, tibbles, or correlation matrices.  
- Supports mixed data (numeric, factor, ordered) via `assocSelect()`.  
- Automatically applies suitable association metrics to each pair of column types.  
- Force-in variables required in every subset.  
- Returns a `CorrCombo` S4 object with subset statistics and automatic print method.  
- Model-agnostic: feature selection is independent of downstream modeling.

## Installation

```r
# Install from GitHub
remotes::install_github("gcol33/corrselect")
```

## Basic Usage

```r
library(corrselect)

# Simulated example
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

# Find all maximal low-correlation subsets (default: Pearson, threshold = 0.7)
res <- corrSelect(df, threshold = 0.7)
show(res)

# Extract the best subset from the original data
subset1 <- corrSubset(res, df, which = 1)
head(subset1)

# Convert to data frame
as.data.frame(subset1)
```

## Mixed Data Frames

Use `assocSelect()` to perform subset selection on data frames with numeric, factor, or ordered columns. The function automatically applies the most appropriate association measure to each pair of variables:

- numeric–numeric → Pearson (default)
- numeric–factor → Eta squared
- numeric–ordered → Spearman
- factor–factor → Cramér’s V
- ordered–ordered → Spearman or Kendall

```r
df2 <- data.frame(
  height = rnorm(30, 170, 10),
  weight = rnorm(30, 70, 12),
  group  = factor(sample(c("A","B"), 30, TRUE)),                # unordered
  rating = ordered(sample(c("low","med","high"), 30, TRUE))     # ordered
)

# Use association-aware selection
res2 <- assocSelect(df2, threshold = 0.6)
show(res2)
```

Example output:

```
CorrCombo object
-----------------
  Method:      bron-kerbosch
  Correlation: mixed
  Threshold:   0.600
  Subsets:     2 valid combinations
  Data Rows:   30 used in correlation
  Pivot:       TRUE
  AssocMethods: numeric_numeric  → pearson,
                 numeric_factor  → eta,
                 numeric_ordered → spearman

Top combinations:
  No.  Variables                          Avg    Max    Size
  ------------------------------------------------------------
  [ 1] height, rating                    0.311  0.562     2
  [ 2] weight, rating                    0.317  0.580     2
```

## License

MIT (see [LICENSE.md](LICENSE.md))
