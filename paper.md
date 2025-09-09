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

`corrselect` [@cran] is an R package that selects variable subsets whose pairwise correlations or associations do not exceed a user-defined threshold. It enumerates **all maximal admissible subsets** rather than returning a single heuristic solution, which supports transparent and reproducible preprocessing when redundancy harms interpretability or stability.

The package handles both numeric and mixed-type data. It automatically selects association measures appropriate for each variable pair, including Pearson, Spearman, Kendall, biweight midcorrelation [@Langfelder2008], distance correlation [@Szekely2007; @Szekely2009], the maximal information coefficient [@Reshef2011], ANOVA $\eta^2$, and Cramér’s V. All measures are mapped to the $[0,1]$ interval for consistent thresholding.

# Statement of Need

Collinearity among predictors is widespread and can degrade model inference and prediction [@Dormann2013]. Popular utilities such as `caret::findCorrelation()` apply greedy, order dependent filtering and return a single solution. Embedded and wrapper methods like the elastic net [@ZouHastie2005] or recursive feature elimination [@Witten2009] can be powerful but couple selection to a specific model and add opacity.

`corrselect` formulates a global **admissible set** problem. Given variables $X_1,\dots,X_p$ and pairwise association measures $r_{ij}$, find all maximal subsets $S$ such that

$$
|r_{ij}| \le t \quad \text{for all } i \ne j \in S ,
$$

with a user threshold $t \in (0,1)$. The software supports mixed variable types, optional forced inclusion of key predictors, and exact coverage of all maximal solutions.

# Functionality

Two user functions cover common workflows:

- `assocSelect()` computes pairwise associations for a mixed-type data frame, then selects admissible subsets at threshold $t$.
- `corrSelect()` takes a precomputed correlation or association matrix.

Both return a `CorrCombo` object with the list of maximal subsets and summary statistics, along with `print`, `summary`, and `as.data.frame` methods.

Internally, the package implements two exact algorithms:

- **Efficient Local Search** (ELS): recursive branch-and-bound that grows admissible subsets and prunes early. It is effective when users supply `forced_in` seeds.
- **Bron–Kerbosch**: classical maximal clique enumeration on the complement of the thresholded association graph [@Bron1973]. This guarantees exhaustive coverage and performs well when the graph is sparse.

Both methods ensure non-redundant and exhaustive enumeration of admissible subsets.

# Related Work

Heuristic correlation filters are simple and common but are order dependent and yield a single result. `corrselect` extends this space by providing exhaustive enumeration, support for mixed data, and user control via `forced_in`. Compared with embedded or wrapper selection, it is model agnostic and interpretable. The graph-theoretic basis links admissible subsets to maximal cliques and independent sets, with ELS offering a complementary search strategy.

Other approaches to feature selection include embedded methods such as the elastic net [@ZouHastie2005], recursive feature elimination [@Witten2009], or permutation-based approaches such as Boruta. These methods can be powerful but are tied to specific modeling frameworks, non-deterministic, and less interpretable in the presence of multicollinearity. By contrast, `corrselect` is fast, deterministic, and model agnostic, linking statistical association to well-studied optimization problems.

# Applications

The approach supports bioclimatic predictor filtering, feature screening in high-dimensional settings, and exploratory mapping of alternative, equally valid predictor sets. The inclusion of biweight midcorrelation [@Langfelder2008], distance correlation [@Szekely2007; @Szekely2009], and the maximal information coefficient [@Reshef2011] further extends its applicability to genomics, network analysis, and large heterogeneous datasets.

# References