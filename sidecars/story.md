# Story: corrselect

## Claim
Removes redundant predictors before modelling: exact enumeration of every maximal predictor subset under a correlation threshold, plus VIF-based pruning inside a model.

## Audience
Analysts fitting regression or species distribution models with many correlated predictors; know `lm()`/`glm()`, may know `caret::findCorrelation()` or manual VIF removal.

## Relation
- to: none (sits beside caret::findCorrelation(), Boruta, glmnet, manual VIF removal)
- home: vignettes/comparison.Rmd ("Comparison with Alternatives")

## Arcs

### A1 One pruned data frame in one call
- question: which predictors do I keep?
- answer: corrPrune() returns one data frame whose pairwise correlations stay at or below the threshold
- evidence: tests/testthat, vignettes/quickstart.Rmd
- home: vignettes/quickstart.Rmd
- also: README (Usage), DESCRIPTION

### A2 Every valid subset, not only one
- question: are there other equally good choices?
- answer: corrSelect()/assocSelect()/MatSelect() enumerate every maximal clique of the threshold graph (Bron-Kerbosch, ELS in C++)
- evidence: vignettes/theory.Rmd
- home: vignettes/theory.Rmd
- also: README (opening, Usage)

### A3 Model-based pruning
- question: my model still has high VIF
- answer: modelPrune() removes by VIF or condition number with lm/glm/lme4/glmmTMB or custom engines
- home: vignettes/advanced.Rmd (custom engines), vignettes/workflows.Rmd (mixed models)
- also: README

### A4 Exact versus greedy
- question: does the result depend on the method or the run?
- answer: exact mode returns the largest subset and is deterministic; greedy mode is quadratic and used beyond max_exact_p
- limits: exact enumeration is exponential in the worst case
- home: vignettes/advanced.Rmd, vignettes/comparison.Rmd
- also: README (Exact and greedy search, Compared with caret::findCorrelation())

### A5 Constraints and mixed data
- answer: force_in, by/group_q, per-pair association measures for numeric, factor and ordered columns
- home: vignettes/advanced.Rmd, ?corrPrune
- also: README

## Documents
- DESCRIPTION: claim, A1, A3, A2
- README / landing: claim, mechanism, Usage (A1-A3), one section per remaining arc, routing
- Quick Start: tutorial, each function once
- Workflows, Comparison, Advanced, Theory: how-to and explanation

## Open
- caret determinism: the former README said findCorrelation() "can vary between runs"; the comparison vignette says "Non-deterministic". Not verified; the README now states caret's procedure only.
