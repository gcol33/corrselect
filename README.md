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

Hand it a data frame and a threshold. `corrselect` finds every maximal set of
predictors whose pairwise correlations all stay under that threshold, by
graph enumeration (Bron–Kerbosch / Eppstein–Löffler–Strash) in C++, and
returns whichever view you need: a single pruned data frame, or every valid
subset to choose from.

```r
library(corrselect)

# prune to one low-correlation set, in a single call
corrPrune(mtcars, threshold = 0.7)

# or enumerate *every* maximal low-correlation set
corrSelect(mtcars, threshold = 0.7)
```

## Every valid choice, ranked

`corrSelect()` returns every maximal low-correlation subset, ranked by size then
average correlation:

```r
res <- corrSelect(mtcars, threshold = 0.7)
res
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: pearson
#>   Threshold:   0.700
#>   Subsets:     15 maximal subsets
#>   Data Rows:   32 used in correlation
#>   Pivot:       TRUE
#>
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] mpg, drat, qsec, gear, carb       0.416  0.700     5
#>   [ 2] cyl, drat, qsec, gear, carb       0.434  0.700     5
#>   [ 3] mpg, drat, vs, gear, carb         0.466  0.700     5
#>   ... (12 more combinations)

corrSubset(res, mtcars, which = "best")   # pull the top-ranked subset back out as a data frame
```

## Keeping variables you already trust, across groups

`force_in` protects variables that must survive pruning regardless of what else gets
dropped; `by` requires the threshold to hold separately inside every group:

```r
# hp and wt are kept no matter what else is removed
corrPrune(mtcars, threshold = 0.7, force_in = c("hp", "wt"))

# threshold holds within every level of `site`, checked group by group
corrPrune(longitudinal_example[, c("x1", "x2", "x3", "x4", "x5", "site")],
          threshold = 0.6, by = "site")
```

## Mixed-type data

`assocSelect()` picks the right association measure for each pair of columns --
Pearson for numeric-numeric, eta-squared for numeric-factor, Cramer's V for
factor-factor -- and enumerates maximal subsets under all of them at once:

```r
df <- data.frame(
  height = rnorm(30, 170, 10),
  weight = rnorm(30, 70, 12),
  group  = factor(sample(c("A", "B"), 30, TRUE)),
  rating = ordered(sample(c("low", "med", "high"), 30, TRUE))
)

assocSelect(df, threshold = 0.6)
```

`corrPrune()` runs the same mixed-type dispatch under the hood, so a single
pruning call works whether the input is all-numeric or a mix of numeric,
factor, and ordered columns. Six numeric-numeric measures are available --
`"pearson"`, `"spearman"`, `"kendall"`, `"bicor"` (WGCNA), `"distance"`
(energy), and `"maximal"` (minerva) -- selectable via `measure` /
`method_num_num`.

## Model-based pruning with any engine

`modelPrune()` removes predictors by VIF or condition number, refitting after
each removal. Built-in engines cover `lm`, `glm`, `lme4`, and `glmmTMB`;
anything else plugs in as a two-function engine:

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

## Exact where it matters, greedy where it scales

`corrPrune()` picks a search mode for you: exact enumeration up to `max_exact_p`
predictors (100 by default), and a greedy C++ backend beyond that. Exact mode
guarantees the largest subset satisfying the threshold; greedy mode trades that
guarantee for speed on wide data. Both are available directly, so you can pin
one explicitly:

```r
corrPrune(mtcars, threshold = 0.7, mode = "exact")   # guaranteed-maximal, small/medium p
corrPrune(mtcars, threshold = 0.7, mode = "greedy")  # fast, approximate, any p
```

`caret::findCorrelation()` solves this with greedy iterative removal, which
is order-dependent and can vary between runs on the same data.
`corrPrune(mode = "exact")` solves the same threshold constraint by
maximal-clique enumeration, retaining at least as many variables and
returning the same answer every run. The
[Comparison vignette](https://gillescolling.com/corrselect/articles/comparison.html)
walks through this and three other alternatives (Boruta, glmnet, manual VIF
removal) side by side on the same dataset.

## What's in the box

- **`corrPrune()`**: association-based pruning, model-free. Exact mode for `p <= 100`,
  greedy mode for larger `p`, protect variables with `force_in`, hold the threshold
  across groups with `by`.
- **`modelPrune()`**: VIF-based pruning for `lm`, `glm`, `lme4`, `glmmTMB`, or any
  custom engine (INLA, mgcv, brms, ...).
- **`corrSelect()` / `MatSelect()`**: exhaustive enumeration of all maximal sets,
  on a data frame or directly on a correlation matrix.
- **`corrSubset()`**: pull one or more maximal subsets back out of a `CorrCombo`
  as data frames.
- **`assocSelect()`**: mixed-type data (numeric, factor, ordered), with the right
  association metric chosen per pair.

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
- [Function Reference](https://gillescolling.com/corrselect/reference/index.html)

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
  year    = {2026},
  volume  = {11},
  number  = {118},
  pages   = {9539},
  doi     = {10.21105/joss.09539},
  url     = {https://joss.theoj.org/papers/10.21105/joss.09539}
}
```
