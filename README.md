# corrselect

*every uncorrelated subset of predictors*

[![CRAN status](https://www.r-pkg.org/badges/version/corrselect)](https://CRAN.R-project.org/package=corrselect)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/corrselect)](https://cran.r-project.org/package=corrselect)
[![Monthly downloads](https://cranlogs.r-pkg.org/badges/corrselect)](https://cran.r-project.org/package=corrselect)
[![R-CMD-check](https://github.com/gcol33/corrselect/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/gcol33/corrselect/actions/workflows/R-CMD-check.yml)
[![Codecov test coverage](https://codecov.io/gh/gcol33/corrselect/graph/badge.svg)](https://app.codecov.io/gh/gcol33/corrselect)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.09539/status.svg)](https://doi.org/10.21105/joss.09539)

**Exact enumeration of maximal predictor sets under a correlation threshold.**

Feed it your predictors. `corrselect` returns the maximal sets whose pairwise
correlations all stay under your threshold, found exactly by graph enumeration
(Bron–Kerbosch / Eppstein–Löffler–Strash) in C++. A greedy filter hands you one
set and hides the rest. This keeps every variable it can, and shows you all the
valid choices.

```r
library(corrselect)

# prune to one low-correlation set, in a single call
corrPrune(mtcars, threshold = 0.7)

# or enumerate *every* maximal low-correlation set
corrSelect(mtcars, threshold = 0.7)
```

## Exact, not greedy

The usual tool, `caret::findCorrelation()`, removes variables greedily: it is
order-dependent, non-deterministic, and typically drops more than it needs to.
`corrselect` solves the same threshold constraint by maximal-clique enumeration,
so it retains at least as many variables and returns the same answer every run.

```r
m <- cor(mtcars)

caret::findCorrelation(m, cutoff = 0.7)              # greedy, ordering-dependent
corrPrune(mtcars, threshold = 0.7, mode = "exact")   # exact, deterministic
```

## What's in the box

- **`corrPrune()`**: association-based pruning, model-free. Exact mode for `p <= 100`,
  greedy mode for larger `p`, protect variables with `force_in`.
- **`modelPrune()`**: VIF-based pruning for `lm`, `glm`, `lme4`, `glmmTMB`, or any
  custom engine (INLA, mgcv, brms, ...).
- **`corrSelect()` / `MatSelect()`**: exhaustive enumeration of all maximal sets,
  on a data frame or directly on a correlation matrix.
- **`assocSelect()`**: mixed-type data (numeric, factor, ordered), with the right
  association metric chosen per pair.

Multiple association metrics are supported: `"pearson"`, `"spearman"`, `"kendall"`,
`"bicor"` (WGCNA), `"distance"` (energy), `"maximal"` (minerva), and `"eta"` /
`"cramersv"` for mixed-type data.

## `corrPrune` or `modelPrune`?

|  | `corrPrune()` | `modelPrune()` |
|---|---|---|
| Needs a model? | No | Yes |
| Based on | Pairwise correlation / association | Model diagnostics (VIF) |
| Works without a response? | Yes | No |
| Mixed models? | No | Yes (`lme4`, `glmmTMB`) |
| Best for | Exploratory analysis, large `p` | Regression workflows, VIF reduction |

Use `corrPrune()` first to cut dimensionality, then `modelPrune()` for final cleanup
inside a modeling framework.

## Model-based pruning with any engine

`modelPrune()` works with `lm`, `glm`, `lme4`, `glmmTMB`, or any package you wire in:

```r
# linear model, VIF threshold
modelPrune(mpg ~ cyl + disp + hp + wt, data = mtcars, limit = 5)

# any modeling package, via a custom engine (here: INLA)
inla_engine <- list(
  name = "inla",
  fit  = function(formula, data, ...) {
    INLA::inla(formula, data = data, family = "gaussian", ...)
  },
  diagnostics = function(model, fixed_effects) {
    scores <- model$summary.fixed[, "sd"]          # posterior SD as the badness score
    setNames(scores, rownames(model$summary.fixed))[fixed_effects]
  }
)
modelPrune(y ~ x1 + x2, data = df, engine = inla_engine, limit = 0.5)
```

## Mixed-type data

`assocSelect()` chooses the right association metric per pair (Pearson, Spearman,
eta-squared, Cramér's V):

```r
df <- data.frame(
  height = rnorm(30, 170, 10),
  weight = rnorm(30, 70, 12),
  group  = factor(sample(c("A", "B"), 30, TRUE)),
  rating = ordered(sample(c("low", "med", "high"), 30, TRUE))
)

assocSelect(df, threshold = 0.6)
```

## Installation

```r
install.packages("corrselect")            # CRAN

install.packages("pak")                   # development version
pak::pak("gcol33/corrselect")
```

## Documentation

- [Quick Start](https://gillescolling.com/corrselect/articles/quickstart.html)
- [Workflows](https://gillescolling.com/corrselect/articles/workflows.html)
- [Comparison](https://gillescolling.com/corrselect/articles/comparison.html)
- [Advanced Usage](https://gillescolling.com/corrselect/articles/advanced.html)
- [Theory](https://gillescolling.com/corrselect/articles/theory.html)

## Support

> "Software is like sex: it's better when it's free." — Linus Torvalds

I'm a PhD student who builds R packages in my free time because I believe good tools
should be free and open. I started these projects for my own work and figured others
might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to say thanks.
It helps with my coffee addiction.

[![Buy Me A Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

MIT (see the LICENSE.md file)

## Citation

```bibtex
@article{corrselect,
  author  = {Colling, Gilles},
  title   = {corrselect: Fast and flexible predictor pruning for data analysis and modeling},
  journal = {Journal of Open Source Software},
  year    = {2025},
  doi     = {10.21105/joss.09539},
  url     = {https://joss.theoj.org/papers/10.21105/joss.09539}
}
```
