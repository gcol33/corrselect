# corrselect: Correlation-Based and Model-Based Predictor Pruning

Provides tools for reducing multicollinearity in predictor sets through
association-based and model-based approaches. The package offers both
fast greedy algorithms for quick pruning and exact graph-theoretic
algorithms for exhaustive subset enumeration.

## Association-Based Pruning

These functions identify variable subsets where all pairwise
correlations or associations remain below a user-defined threshold:

- [`corrPrune`](https://gillescolling.com/corrselect/reference/corrPrune.md):

  Fast greedy pruning for numeric data

- [`corrSelect`](https://gillescolling.com/corrselect/reference/corrSelect.md):

  Exhaustive enumeration for numeric data frames

- [`assocSelect`](https://gillescolling.com/corrselect/reference/assocSelect.md):

  Exhaustive enumeration for mixed-type data (numeric, factor, ordered)

- [`MatSelect`](https://gillescolling.com/corrselect/reference/MatSelect.md):

  Direct interface using a pre-computed correlation matrix

## Model-Based Pruning

These functions use variance inflation factors (VIF) to iteratively
remove collinear predictors from regression models:

- [`modelPrune`](https://gillescolling.com/corrselect/reference/modelPrune.md):

  VIF-based pruning for lm, glm, lme4, and glmmTMB models

## Algorithms

The exact enumeration functions (`corrSelect`, `assocSelect`,
`MatSelect`) use two graph-theoretic algorithms:

- Eppstein-Loffler-Strash (ELS):

  Recommended when using `force_in` constraints

- Bron-Kerbosch:

  Default algorithm, with optional pivoting for performance

## Helpers

- [`corrSubset`](https://gillescolling.com/corrselect/reference/corrSubset.md):

  Extract specific subsets from results

- [`CorrCombo-class`](https://gillescolling.com/corrselect/reference/CorrCombo.md):

  S4 class holding enumeration results

## See also

Vignettes:
[`vignette("quickstart", package = "corrselect")`](https://gillescolling.com/corrselect/articles/quickstart.md),
[`vignette("advanced", package = "corrselect")`](https://gillescolling.com/corrselect/articles/advanced.md)

## Author

**Maintainer**: Gilles Colling <gilles.colling051@gmail.com>
