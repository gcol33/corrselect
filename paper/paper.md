---
title: "corrselect: Fast and flexible predictor pruning for data analysis and modeling"
authors:
  - name: Gilles Colling
    orcid: 0000-0003-3070-6066
    affiliation: 1
affiliations:
  - name: Department of Botany and Biodiversity Research, University of Vienna, Austria
    index: 1
date: 2025-09-09
repository: https://github.com/gcol33/corrselect
archive_doi: 10.5281/zenodo.17088305
bibliography: paper.bib
---

# Summary

`corrselect` [@cran] is an R package for reducing multicollinearity and redundancy in predictor sets. It provides two complementary approaches: (1) high-level pruning functions that return a single optimal subset, and (2) exhaustive enumeration of all maximal admissible subsets. The package handles both numeric and mixed-type data, supports forced inclusion of key predictors, and integrates with standard R modeling workflows including mixed-effects models.

Version 3.0 introduces `corrPrune()` for association-based pruning and `modelPrune()` for VIF-based model pruning, while retaining the original exhaustive enumeration functions (`corrSelect()`, `assocSelect()`, `MatSelect()`). A fast C++ greedy algorithm enables efficient pruning for large predictor sets (p > 100), while exact graph-theoretic algorithms guarantee complete enumeration when exhaustive search is feasible.

# Statement of Need

Collinearity among predictors is common in applied modeling and can degrade inference and prediction [@Dormann2013]. Popular utilities such as `caret::findCorrelation()` apply greedy, order-dependent filtering and return a single solution, typically removing the variable with the highest mean correlation at each step. This heuristic approach discards potentially useful subsets and provides no guarantee of optimality: where `caret` returns one subset, `corrselect` in exact mode might reveal a dozen equally valid alternatives. Having the full set of options helps when domain knowledge should guide final variable selection, or when researchers need to assess the sensitivity of their conclusions to predictor choice. Supervised filter methods such as FCBF [@YuLiu2003] address a related but distinct problem: selecting features correlated with a target variable while removing redundancy. Embedded and wrapper methods like the elastic net [@ZouHastie2005] or recursive feature elimination [@Witten2009] can be powerful but couple selection to a specific model and reduce transparency.

`corrselect` addresses these limitations through two interfaces. For routine workflows, `corrPrune()` and `modelPrune()` provide simple, deterministic pruning with a single function call. For exhaustive exploration, the package formulates a global admissible set problem: given variables $X_1,\dots,X_p$ and pairwise measures $r_{ij}$, find all maximal subsets $S$ such that

$$
|r_{ij}| \le t \quad \text{for all } i \ne j \in S ,
$$

with a user threshold $t \in (0,1)$. This is equivalent to finding all maximal cliques in the compatibility graph, a well-studied problem in computer science. Unlike greedy methods that return a single result, `corrselect` in exact mode enumerates *all* maximal admissible subsets, enabling researchers to explore the full solution space. This dual approach balances practicality with methodological rigor.

# Functionality

The package provides two complementary approaches to predictor pruning: model-agnostic methods that operate on predictors alone, and model-based methods that require fitting a model.

## Model-Agnostic Pruning

Model-agnostic pruning removes redundant predictors based on pairwise correlation or association measures, without requiring a response variable. This unsupervised approach is useful for pre-modeling dimensionality reduction or when the outcome is not yet defined. The package provides both a simple pruning interface and exhaustive enumeration functions:

- **`corrPrune()`**: Returns a pruned data frame with pairwise associations below a user-specified threshold. Supports exact mode (exhaustive search, recommended for $p \le 100$) and greedy mode (fast polynomial-time algorithm for large $p$). Automatic measure selection handles numeric, factor, and ordered variables. The `force_in` parameter protects key predictors from removal.

- **`corrSelect()`**, **`assocSelect()`**, **`MatSelect()`**: Exhaustive enumeration functions that return all maximal admissible subsets rather than a single solution. `corrSelect()` handles numeric data with correlations in $[-1,1]$; `assocSelect()` handles mixed-type data using normalized association measures in $[0,1]$, including Pearson, Spearman, and Kendall correlations, biweight midcorrelation [@Langfelder2008], distance correlation [@Szekely2007; @Szekely2009], the maximal information coefficient [@Reshef2011], ANOVA $\eta^2$, and Cramér's V; `MatSelect()` operates directly on a symmetric association matrix.

All enumeration functions return a `CorrCombo` object containing maximal subsets, summary statistics, and standard methods (`print`, `summary`, `as.data.frame`). The helper function `corrSubset()` extracts filtered data frames from results.

A typical workflow demonstrates the key difference from greedy methods:

```r
library(corrselect)
result <- corrSelect(mtcars, threshold = 0.7)
result
#> CorrCombo object with 12 maximal subsets
#>   Threshold: 0.7 | Correlation method: pearson
#>   Sizes: 6, 5, 5, 5, ... | Avg |r|: 0.30, 0.27, 0.30, 0.31, ...

as.data.frame(result)[1:3, c("subset", "size", "avg_corr")]
#>                          subset size avg_corr
#> 1 mpg, cyl, drat, qsec, vs, am    6     0.30
#> 2     mpg, drat, qsec, vs, gear    5     0.27
#> 3      mpg, hp, drat, qsec, am    5     0.30
```

Unlike `caret::findCorrelation()` which returns a single variable set, `corrSelect()` reveals all 12 equally valid solutions, enabling informed selection based on domain knowledge.

### Algorithms

The admissible set problem is mathematically equivalent to finding all maximal cliques in the "compatibility graph" where edges connect variable pairs with $|r_{ij}| \le t$, or equivalently, all maximal independent sets in the "conflict graph" where edges connect pairs exceeding the threshold. This connection to well-studied graph problems provides a solid theoretical foundation.

For exact enumeration, the package implements two algorithms natively in C++ (not as wrappers around external libraries such as igraph [@igraph]):

- **Bron-Kerbosch**: The classical maximal clique enumeration algorithm [@Bron1973], used by default for unrestricted enumeration.
- **Eppstein-Löffler-Strash (ELS)**: A near-optimal algorithm for sparse graphs [@Eppstein2010], used when `force_in` seeds are specified.

Both exact methods ensure non-redundant and complete enumeration of admissible subsets. However, maximal clique enumeration is NP-hard in the general case, and exact mode may become impractically slow on large or densely connected problems. On a modern desktop (Intel i9-14900K) with correlations clustered near the threshold, $p = 100$ completed in under 1 second, $p = 150$ in ~19 seconds, $p = 175$ in ~3 minutes, and $p = 200$ in ~17 minutes. Exact mode is therefore recommended only for moderate-sized problems ($p \le 100$).

For larger predictor sets or low thresholds where exact enumeration becomes infeasible, `corrPrune(mode = "greedy")` provides a fast polynomial-time alternative. Unlike `caret::findCorrelation()` which removes the variable with the highest mean correlation, this custom greedy algorithm iteratively removes the variable involved in the most threshold violations, with ties broken by maximum and then average association. This runs in $O(p^2 \times k)$ time where $k$ is the number of variables removed.

## Model-Based Pruning

In contrast to model-agnostic methods, `modelPrune()` addresses multicollinearity using variance inflation factors (VIF) computed within a modeling context. This supervised approach requires a response variable and considers joint relationships among predictors rather than pairwise associations alone.

- **`modelPrune()`**: Iteratively removes the predictor with the highest VIF until all remaining predictors fall below a user-specified limit. Supports multiple modeling engines (`lm`, `glm`, `lme4`, `glmmTMB`) and custom engine definitions for integration with any R modeling package (e.g., INLA, mgcv, brms). For mixed-effects models, only fixed effects are pruned while random effect structures are preserved.

# Related Work

Heuristic correlation filters are widely used but are order-dependent and return only a single result. `corrselect` extends this space by providing both fast deterministic pruning and exhaustive enumeration, support for mixed data types, VIF-based model pruning, and user control via `force_in`. The model-agnostic functions are interpretable and independent of any particular modeling framework, while the graph-theoretic foundation links admissible subsets to maximal cliques and independent sets.

Other feature selection methods include embedded approaches such as the elastic net [@ZouHastie2005], recursive feature elimination [@Witten2009], or permutation-based algorithms such as Boruta. These methods can be powerful but are tied to specific modeling frameworks, potentially non-deterministic, and less interpretable in the presence of multicollinearity. By contrast, the model-agnostic functions in `corrselect` are fast, deterministic, and formulate subset selection as a well-defined optimization problem.

# Applications

The package supports feature screening in high-dimensional modeling and exploratory mapping of alternative, equally valid predictor sets. With support for correlation and association measures such as biweight midcorrelation [@Langfelder2008], distance correlation [@Szekely2007; @Szekely2009], and the maximal information coefficient [@Reshef2011], `corrselect` is applicable across domains including genomics, network analysis, environmental modeling, and machine learning. The VIF-based `modelPrune()` function integrates directly with regression and mixed-effects modeling workflows, while the custom engine interface enables extension to specialized modeling packages.

# References
