---
title: "corrselect: Flexible and exact variable subset selection based on correlation and association matrices"
authors:
  - name: Gilles Colling
    orcid: 0000-0003-3070-6066
    affiliation: 1
affiliations:
  - name: Department of Botany and Biodiversity Research, University of Vienna, Austria
    index: 1
date: 2025-09-09
repository: https://github.com/gcol33/corrselect
archive: pending
bibliography: paper.bib
---

# Summary

`corrselect` [@cran] is an R package for selecting variable subsets whose pairwise correlations or associations do not exceed a user-defined threshold. Instead of applying greedy heuristics, it enumerates **all maximal admissible subsets** of variables, ensuring that no valid solution is missed. This allows transparent and reproducible preprocessing in statistical and machine learning workflows where redundancy harms interpretability, predictive stability, or generalizability.

The package handles both numeric and mixed-type data. It automatically selects association measures appropriate for each variable pair: Pearson, Spearman, Kendall, biweight midcorrelation, distance correlation, the maximal information coefficient, ANOVA $\eta^2$, and Cramér’s V. All measures are mapped to the $[0,1]$ interval for consistent thresholding. Version 2.0.0 introduced full mixed-type support, expanding applications to domains such as ecology, genomics, and survey analysis.

# Statement of Need

Collinearity among predictors is a pervasive issue across applied sciences [@Dormann2013]. Suppose we have variables $X_1,\dots,X_p$ and wish to identify subsets $S$ such that for all $i, j \in S$,

$$
|r_{ij}| \le t ,
$$

where $r_{ij}$ is a symmetric association measure and $t \in (0,1)$ is a user-specified threshold. Existing functions such as `caret::findCorrelation()` apply greedy, order-dependent filtering and yield only one solution. Embedded or wrapper methods such as the elastic net [@ZouHastie2005] or recursive feature elimination [@Witten2009] select features tied to a specific model and often lack transparency.

`corrselect` formulates this as the **admissible set problem**, providing exhaustive coverage of all maximal subsets, optional forced inclusion of user-specified predictors, and support for mixed-type variables. This makes it suitable as a general-purpose, model-agnostic preprocessing step.

# Functionality

The package exposes two main functions:

- `assocSelect(df, threshold, ...)`: computes pairwise associations for mixed-type data frames and enumerates admissible subsets.
- `corrSelect(cmat, threshold, ...)`: takes a precomputed correlation or association matrix.

Both return a `CorrCombo` object storing the subsets, correlation summaries, and metadata. Results can be inspected via `summary()`, printed, or exported with `as.data.frame()` for tidy workflows.

Two exact algorithms are implemented:

- **Efficient Local Search (ELS):** a recursive branch-and-bound method that incrementally grows admissible subsets and prunes invalid paths early. It is particularly efficient when some variables are forced into all subsets.
- **Bron–Kerbosch:** a classical algorithm for enumerating maximal cliques [@Bron1973], applied to the complement of the thresholded association graph. It guarantees completeness and is efficient when associations are sparse.

Both methods ensure **non-redundant and exhaustive** enumeration of admissible subsets.

# Related Work

Correlation-based filtering is a longstanding practice, with `caret::findCorrelation()` the most common R implementation. However, it is order dependent, returns a single solution, and does not handle mixed data or forced inclusion. `corrselect` improves on this by adopting a global graph-theoretic formulation that guarantees completeness.

Other approaches to feature selection include embedded methods such as the elastic net [@ZouHastie2005], recursive feature elimination, or permutation-based methods like Boruta. These are powerful but tied to specific modeling frameworks, non-deterministic, and less interpretable in the presence of multicollinearity. By contrast, `corrselect` is fast, deterministic, and model agnostic. Its link to graph theory—through maximal clique and independent set enumeration—connects statistical association to well-studied optimization problems.

# Applications

The package supports diverse use cases:

- **Bioclimatic modeling:** filtering climate predictors reduces collinearity and improves stability in species distribution models.
- **Machine learning pipelines:** pre-filtering redundant variables simplifies models while retaining interpretability.
- **Exploratory analysis:** enumerating admissible subsets reveals alternative, equally valid predictor sets.
- **Education:** the admissible set problem provides a teaching example linking correlation, graph theory, and algorithmic search.

# References
