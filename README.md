# corrselect

**Exhaustive, Model-Agnostic Variable Subset Selection Based on Pairwise Correlation**

`corrselect` automatically identifies all maximal subsets of numeric variables in your data whose pairwise correlations remain below a specified threshold. This reduces multicollinearity and redundancy before modeling, while retaining interpretability. The approach is model-agnostic: it operates purely on the data’s correlation structure, making it suitable for regression, clustering, and other analyses.

## Features

- Exhaustive, exact subset enumeration using graph algorithms (ELS and Bron–Kerbosch).  
- Multiple correlation measures: `"pearson"`, `"spearman"`, `"kendall"`, `"bicor"`, `"distance"`, `"maximal"`.  
- Flexible: works with data frames, tibbles, or correlation matrices.  
- Force-in variables required in every subset.  
- Returns a `CorrCombo` S4 object with summary statistics.  
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

## License

MIT (see [LICENSE.md](LICENSE.md))
