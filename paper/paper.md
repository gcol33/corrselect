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

Collinearity among predictors is common in applied modeling and can degrade inference and prediction [@Dormann2013]. Popular utilities such as `caret::findCorrelation()` apply greedy, order-dependent filtering and return a single solution—typically removing the variable with the highest mean correlation at each step. This heuristic approach discards potentially useful subsets and provides no guarantee of optimality. Correlation-based filter methods such as FCBF [@YuLiu2003] similarly employ greedy strategies. Embedded and wrapper methods like the elastic net [@ZouHastie2005] or recursive feature elimination [@Witten2009] can be powerful but couple selection to a specific model and reduce transparency.

`corrselect` addresses these limitations through two interfaces. For routine workflows, `corrPrune()` and `modelPrune()` provide simple, deterministic pruning with a single function call. For exhaustive exploration, the package formulates a global admissible set problem: given variables $X_1,\dots,X_p$ and pairwise measures $r_{ij}$, find all maximal subsets $S$ such that

$$
|r_{ij}| \le t \quad \text{for all } i \ne j \in S ,
$$

with a user threshold $t \in (0,1)$. Unlike greedy methods that return a single result, `corrselect` in exact mode enumerates *all* maximal admissible subsets, enabling researchers to explore the full solution space. This dual approach balances practicality with methodological rigor.

# Functionality

## High-Level Pruning Functions

Two functions provide streamlined interfaces for common pruning tasks:

- **`corrPrune()`**: Association-based predictor pruning. Given a data frame and correlation threshold, returns a pruned data frame with pairwise associations below the threshold. Supports exact mode (exhaustive search, recommended for $p \le 100$) and greedy mode (fast polynomial-time algorithm for large $p$). Automatic measure selection handles numeric, factor, and ordered variables. The `force_in` parameter protects key predictors from removal.

- **`modelPrune()`**: Model-based pruning using variance inflation factors (VIF). Iteratively removes predictors with VIF exceeding a user-specified limit until all remaining predictors satisfy the constraint. Supports multiple modeling engines (`lm`, `glm`, `lme4`, `glmmTMB`) and custom engine definitions for integration with any R modeling package (e.g., INLA, mgcv, brms). For mixed-effects models, only fixed effects are pruned while random effect structures are preserved.

## Exhaustive Enumeration Functions

Three functions implement complete subset enumeration:

- **`corrSelect()`**: Takes a numeric data frame, computes pairwise correlations in $[-1,1]$, and enumerates all maximal admissible subsets at threshold $t$.
- **`assocSelect()`**: Handles mixed-type data using normalized association measures in $[0,1]$, including Pearson, Spearman, and Kendall correlations, biweight midcorrelation [@Langfelder2008], distance correlation [@Szekely2007; @Szekely2009], the maximal information coefficient [@Reshef2011], ANOVA $\eta^2$, and Cramér's V.
- **`MatSelect()`**: Operates directly on a symmetric association matrix.

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

## Algorithms

The admissible set problem is mathematically equivalent to finding all maximal cliques in the "compatibility graph" where edges connect variable pairs with $|r_{ij}| \le t$, or equivalently, all maximal independent sets in the "conflict graph" where edges connect pairs exceeding the threshold. This connection to well-studied graph problems provides a solid theoretical foundation.

The package implements three algorithms natively in C++ (not as wrappers around external libraries such as igraph [@igraph]):

- **Greedy pruning**: A fast deterministic algorithm with $O(p^2 \times k)$ complexity, where $k$ is the number of variables removed. Used by `corrPrune(mode = "greedy")` for large predictor sets.
- **Eppstein-Löffler-Strash (ELS)**: A near-optimal maximal clique enumeration algorithm for sparse graphs [@Eppstein2010], particularly effective when `force_in` seeds are specified.
- **Bron-Kerbosch**: The classical maximal clique enumeration algorithm [@Bron1973], often faster for unrestricted enumeration.

Both exact methods ensure non-redundant and complete enumeration of admissible subsets. However, maximal clique enumeration is NP-hard in the general case, so exact mode is recommended only for moderate-sized problems ($p \le 100$); for larger predictor sets or low thresholds, the greedy algorithm provides a fast polynomial-time alternative.

# Related Work

Heuristic correlation filters are widely used but are order-dependent and return only a single result. `corrselect` extends this space by providing both fast deterministic pruning and exhaustive enumeration, support for mixed data types, VIF-based model pruning, and user control via `force_in`. Compared with embedded or wrapper selection methods, it is model-agnostic and interpretable. Its graph-theoretic foundation links admissible subsets to maximal cliques and independent sets.

Other feature selection methods include embedded approaches such as the elastic net [@ZouHastie2005], recursive feature elimination [@Witten2009], or permutation-based algorithms such as Boruta. These methods can be powerful but are tied to specific modeling frameworks, potentially non-deterministic, and less interpretable in the presence of multicollinearity. By contrast, `corrselect` is fast, deterministic, and model-agnostic, formulating subset selection as a well-defined optimization problem.

# Applications

The package supports feature screening in high-dimensional modeling and exploratory mapping of alternative, equally valid predictor sets. With support for correlation and association measures such as biweight midcorrelation [@Langfelder2008], distance correlation [@Szekely2007; @Szekely2009], and the maximal information coefficient [@Reshef2011], `corrselect` is applicable across domains including genomics, network analysis, environmental modeling, and machine learning. The VIF-based `modelPrune()` function integrates directly with regression and mixed-effects modeling workflows, while the custom engine interface enables extension to specialized modeling packages.

# References
