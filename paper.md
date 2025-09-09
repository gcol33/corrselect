---
title: "corrselect: Exhaustive variable subset selection based on correlation and association matrices"
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

`corrselect` [@cran] is a model-agnostic R package for selecting variable subsets whose pairwise correlations or associations do not exceed a user-defined threshold. Instead of returning a single heuristic solution, it enumerates all maximal admissible subsets. This allows users to select subsets before model fitting, avoiding the common problems of highly correlated or associated predictors, which inflate variance estimates, destabilize coefficient estimates, and obscure the relative importance of variables. The package also supports forced inclusion of user-specified predictors (`forced_in`), ensuring that key variables are retained while admissibility constraints govern the remainder.

The package supports both numeric and mixed-type data. Correlation-based workflows include measures such as Pearson, Spearman, Kendall, and biweight midcorrelation [@Langfelder2008], which take values in $[-1,1]$. Association-based workflows use measures normalized to $[0,1]$ for consistent thresholding, including distance correlation [@Szekely2007; @Szekely2009], the maximal information coefficient [@Reshef2011], ANOVA $\eta^2$, and Cramér’s V.

# Statement of Need

Collinearity among predictors is common in applied modeling and can degrade inference and prediction [@Dormann2013]. Popular utilities such as `caret::findCorrelation()` apply greedy, order-dependent filtering and return a single solution. Embedded and wrapper methods like the elastic net [@ZouHastie2005] or recursive feature elimination [@Witten2009] can be powerful but couple selection to a specific model and reduce transparency.

`corrselect` instead formulates a global admissible set problem. Given variables $X_1,\dots,X_p$ and pairwise measures $r_{ij}$, the goal is to find all maximal subsets $S$ such that

$$
|r_{ij}| \le t \quad \text{for all } i \ne j \in S ,
$$

with a user threshold $t \in (0,1)$. The software supports mixed variable types, optional forced inclusion of key predictors, and exhaustive coverage of all maximal solutions.

# Functionality

Three core functions implement the main subset selection tasks:

- `corrSelect()` takes a numeric data frame, computes pairwise correlations, and selects admissible subsets at threshold $t$.
- `assocSelect()` handles mixed-type data, computes normalized association measures in $[0,1]$, and selects admissible subsets at threshold $t$.
- `MatSelect()` provides a lower-level interface for users who already have a precomputed correlation or association matrix.

All return a `CorrCombo` object containing maximal subsets, summary statistics, and standard methods `print`, `summary` and `as.data.frame`. For example, given a data frame `df` in wide format (variables in columns, observations in rows), `corrSelect(df, t = 0.7)` returns all maximal subsets of numeric variables whose pairwise correlations are below 0.7. The function `assocSelect(df, t = 0.7)` generalizes this to mixed-type variables (numeric, binary, or categorical) using normalized association measures.

Internally, the package implements two algorithms for exhaustive enumeration:

- **Efficient Local Search (ELS)**: a recursive branch-and-bound algorithm that expands admissible subsets while pruning early, particularly effective when `forced_in` seeds are specified.  
- **Bron–Kerbosch**: classical maximal clique enumeration on the complement of the thresholded association graph [@Bron1973], guaranteeing exhaustive coverage and performing well when the graph is sparse.  

Both methods ensure non-redundant and complete enumeration of admissible subsets.

# Related Work

Heuristic correlation filters are widely used but are order dependent and return only a single result. `corrselect` extends this space by providing exhaustive enumeration, support for mixed data, and user control via `forced_in`. Compared with embedded or wrapper selection, it is model agnostic and interpretable. Its graph-theoretic foundation links admissible subsets to maximal cliques and independent sets, with ELS offering a complementary search strategy.

Other feature selection methods include embedded approaches such as the elastic net [@ZouHastie2005], recursive feature elimination [@Witten2009], or permutation-based algorithms such as Boruta. These methods can be powerful but are tied to specific modeling frameworks, non-deterministic, and less interpretable in the presence of multicollinearity. By contrast, `corrselect` is fast, deterministic, and model agnostic, formulating subset selection as a well-defined graph optimization problem.

# Applications

The approach supports feature screening in high-dimensional modelling and exploratory mapping of alternative, equally valid predictor sets. With support for correlation and association measures such as biweight midcorrelation [@Langfelder2008], distance correlation [@Szekely2007; @Szekely2009], and the maximal information coefficient [@Reshef2011], `corrselect` is applicable across domains including genomics, network analysis, environmental modeling, and machine learning.

# References
