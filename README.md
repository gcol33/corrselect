
# corrselect

*every uncorrelated subset of predictors*

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/corrselect)](https://CRAN.R-project.org/package=corrselect)
[![R-CMD-check](https://github.com/gcol33/corrselect/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/gcol33/corrselect/actions/workflows/R-CMD-check.yml)
[![Codecov test
coverage](https://codecov.io/gh/gcol33/corrselect/graph/badge.svg)](https://app.codecov.io/gh/gcol33/corrselect)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.09539/status.svg)](https://doi.org/10.21105/joss.09539)
<!-- badges: end -->

corrselect removes redundant predictors before modelling, either by
keeping only predictors whose pairwise correlations do not exceed a
threshold or by dropping predictors with a high variance inflation
factor (VIF) from a regression model.

The correlation search treats each predictor as a node of a graph and
joins two predictors when their correlation is at or below the
threshold. Every set of predictors that can be kept together is then a
clique of this graph. corrselect enumerates the maximal ones, the
subsets to which no further predictor can be added, with the
Bron–Kerbosch and Eppstein–Löffler–Strash algorithms in C++. It returns
either one pruned data frame or every maximal subset, so you can choose
the subset that keeps the variables your analysis needs.

corrselect supports:

- **Pruning in one call.**
  [`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.html)
  returns one data frame. It searches exactly up to 100 predictors
  (`max_exact_p`) and switches to a greedy search in C++ beyond that.
- **Choosing among every valid subset.**
  [`corrSelect()`](https://gillescolling.com/corrselect/reference/corrSelect.html)
  returns every maximal subset, ranked by size and then by average
  correlation.
- **Mixed-type data.** Numeric, factor and ordered columns are compared
  with a measure suited to each pair: Pearson correlation for two
  numeric columns, the correlation ratio eta for a numeric column and a
  factor, and Cramér’s V for two factors.
- **Fixed variables and grouped data.** `force_in` keeps named
  predictors in every result, and `by` requires the threshold to hold
  within each group.
- **Model-based pruning.**
  [`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.html)
  refits a model after each removal, by VIF or condition number, with
  built-in engines for `lm`, `glm`,
  [lme4](https://github.com/lme4/lme4/) and
  [glmmTMB](https://github.com/glmmTMB/glmmTMB) and custom engines for
  any other modelling package.

In exact mode the result is the same on every run, and the largest
subset it returns keeps at least as many predictors as any greedy
removal at the same threshold. [Theory and
Formulation](https://gillescolling.com/corrselect/articles/theory.html)
defines the threshold graph and the algorithms.

## Installation

Install the released version from CRAN. Windows and macOS get a binary
package and need no build tools:

``` r
install.packages("corrselect")
```

The development version compiles C++ code. Install the build tools for
your operating system first:

| Operating system | Build tools |
|----|----|
| Windows | [Rtools](https://cran.r-project.org/bin/windows/Rtools/) matching your R version |
| macOS | Run `xcode-select --install` in Terminal |
| Debian / Ubuntu | Install `r-base-dev` |

Check that R can find the compiler:

``` r
pkgbuild::has_build_tools(debug = TRUE)
```

Then install corrselect from GitHub:

``` r
# install.packages("pak")
pak::pak("gcol33/corrselect")
```

corrselect needs no system libraries beyond the compiler. Its C++ code
links only to Rcpp, which is installed from CRAN with the package.

## Usage

1.  Prune a data frame to one subset with `corrPrune()`.
2.  List every maximal subset with `corrSelect()` and take one out as a
    data frame with `corrSubset()`.
3.  Prune the predictors of a regression model by VIF with
    `modelPrune()`.

The input is a data frame of candidate predictors, with numeric, factor
or ordered columns. Correlations are computed on the rows without
missing values, and the returned data frame keeps every row. The
threshold defaults to 0.7.

``` r
library(corrselect)

pruned <- corrPrune(mtcars)
names(pruned)
#> [1] "mpg"  "drat" "qsec" "gear" "carb"
```

Five of the eleven columns of `mtcars` remain, and no pair of them has a
correlation above 0.7. With eleven predictors the search is exact.

Several subsets usually satisfy the same threshold. `corrSelect()` lists
all maximal ones, and `corrSubset()` returns one of them, the top-ranked
by default:

``` r
res <- corrSelect(mtcars)
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
#>   [ 4] wt, qsec, am, carb                0.373  0.692     4
#>   [ 5] wt, qsec, gear, carb              0.388  0.656     4
#>   ... (10 more combinations)

best <- corrSubset(res, mtcars)
head(best, 3)
#>                mpg drat  qsec gear carb
#> Mazda RX4     21.0 3.90 16.46    4    4
#> Mazda RX4 Wag 21.0 3.90 17.02    4    4
#> Datsun 710    22.8 3.85 18.61    4    1
```

`modelPrune()` takes a model formula instead. It removes the predictor
with the highest VIF, refits, and repeats until no VIF exceeds 5:

``` r
fit <- modelPrune(mpg ~ cyl + disp + hp + wt, data = mtcars)
names(fit)
#> [1] "mpg" "cyl" "hp"  "wt"
```

`disp` is removed, and the response stays in the returned data frame.

## Keeping variables and holding the threshold across groups

`force_in` keeps predictors in the result whatever else is removed. `by`
computes the correlations within each level of a grouping column, and
the threshold has to hold in every group:

``` r
corrPrune(mtcars, threshold = 0.7, force_in = c("hp", "wt"))

cols <- c("x1", "x2", "x3", "x4", "x5", "site")
long <- longitudinal_example[, cols]
corrPrune(long, threshold = 0.6, by = "site")
```

`group_q` relaxes the grouped rule to a quantile of the per-group
correlations. [Advanced
Topics](https://gillescolling.com/corrselect/articles/advanced.html#grouped-pruning)
shows it on simulated data.

## Mixed-type data

`assocSelect()` enumerates maximal subsets of a data frame with numeric,
factor and ordered columns, with the measure chosen per pair:

``` r
set.seed(1)
df <- data.frame(
  height = rnorm(30, 170, 10),
  weight = rnorm(30, 70, 12),
  group  = factor(sample(c("A", "B"), 30, TRUE)),
  rating = ordered(sample(c("low", "med", "high"), 30, TRUE))
)
assocSelect(df, threshold = 0.6)
```

`corrPrune()` uses the same measures, so one call handles all-numeric
and mixed data. For two numeric columns, `measure` selects one of six
measures, three of which need a suggested package; the [`corrPrune()`
reference](https://gillescolling.com/corrselect/reference/corrPrune.html)
lists them.

## Pruning with other modelling packages

`modelPrune()` has built-in engines for `lm`, `glm`, lme4 and glmmTMB.
With lme4 and glmmTMB only the fixed effects are candidates for removal.
Any other package plugs in as a custom engine, a list of two functions:
one that fits the model and one that returns a score per fixed effect.
[Advanced
Topics](https://gillescolling.com/corrselect/articles/advanced.html#custom-engines-for-modelprune)
builds engines for [INLA](https://www.r-inla.org) and
[mgcv](https://cran.r-project.org/package=mgcv).

## corrPrune or modelPrune?

|  | `corrPrune()` | `modelPrune()` |
|----|----|----|
| Needs a model | No | Yes |
| Based on | Pairwise correlation or association | Model diagnostics (VIF, condition number) |
| Works without a response | Yes | No |
| Mixed models | No | Yes (lme4, glmmTMB) |
| Suited to | Exploratory analysis, many predictors | Regression workflows, VIF reduction |

Start with `corrPrune()` to reduce the number of predictors, then use
`modelPrune()` to remove the remaining collinearity within the model you
fit.

## Exact and greedy search

`corrPrune()` chooses the search mode from the number of predictors:
exact enumeration up to `max_exact_p` (100 by default) and the greedy
C++ search beyond that. Exact mode returns the largest subset that
satisfies the threshold. Greedy mode builds one subset by successive
removals and runs in time quadratic in the number of predictors, so it
handles wide data without that guarantee. `mode` sets either one:

``` r
corrPrune(mtcars, threshold = 0.7, mode = "exact")
corrPrune(mtcars, threshold = 0.7, mode = "greedy")
```

## Compared with caret::findCorrelation()

[caret](https://github.com/topepo/caret/)’s `findCorrelation()` removes
predictors one at a time: it takes the most correlated pair and drops
the member with the larger mean absolute correlation, until no pair
exceeds the cutoff. On `mtcars` at the same threshold, with caret 7.0.1:

``` r
drop <- caret::findCorrelation(cor(mtcars), cutoff = 0.7)
names(mtcars)[-drop]
#> [1] "qsec" "gear" "carb"

exact <- corrPrune(mtcars, threshold = 0.7, mode = "exact")
names(exact)
#> [1] "mpg"  "drat" "qsec" "gear" "carb"
```

[Comparison with
Alternatives](https://gillescolling.com/corrselect/articles/comparison.html)
runs both on a larger dataset and sets corrselect beside
[Boruta](https://gitlab.com/mbq/Boruta/),
[glmnet](https://glmnet.stanford.edu) and manual VIF removal.

## Documentation

Start with [Quick
Start](https://gillescolling.com/corrselect/articles/quickstart.html):
each function on a small example, with its main arguments.

- [Complete Workflows: Real-World
  Examples](https://gillescolling.com/corrselect/articles/workflows.html):
  ecological, survey, high-dimensional and mixed-model data pruned and
  modelled end to end.
- [Comparison with
  Alternatives](https://gillescolling.com/corrselect/articles/comparison.html):
  corrselect beside caret, Boruta, glmnet and manual VIF removal on the
  same data.
- [Advanced
  Topics](https://gillescolling.com/corrselect/articles/advanced.html):
  exact and greedy search with timings, tie-breaking, grouped pruning
  and custom engines.
- [Theory and
  Formulation](https://gillescolling.com/corrselect/articles/theory.html):
  the threshold graph, maximal cliques and the enumeration algorithms.
- [Full reference](https://gillescolling.com/corrselect/reference/):
  every function.

## Getting help

Questions and bug reports go to the [GitHub
issues](https://github.com/gcol33/corrselect/issues). A bug report is
easiest to act on with a small reproducible example, for instance made
with [reprex](https://reprex.tidyverse.org/).

## Contributing

Pull requests are welcome; see the [contributing
guide](https://gillescolling.com/corrselect/CONTRIBUTING.html).

## Support

> “Software is like sex: it’s better when it’s free.” — Linus Torvalds

I’m a PhD student who builds R packages in my free time because I
believe good tools should be free and open. I started these projects for
my own work and figured others might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to
say thanks. It helps with my coffee addiction.

[![Buy Me A
Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

corrselect is licensed under MIT.

## Citation

If you use corrselect, please cite the article in the Journal of Open
Source Software and the package version you used.

``` text
Colling G (2026). "corrselect: Fast and flexible predictor pruning for
data analysis and modeling." _Journal of Open Source Software_,
*11*(118), 9539. doi:10.21105/joss.09539
<https://doi.org/10.21105/joss.09539>.
<https://joss.theoj.org/papers/10.21105/joss.09539>.

Colling G (2026). _corrselect: Correlation-Based and Model-Based
Predictor Pruning_. doi:10.32614/CRAN.package.corrselect
<https://doi.org/10.32614/CRAN.package.corrselect>. R package version
3.3.0, <https://gillescolling.com/corrselect/>.
```

BibTeX:

``` bibtex
@Article{colling2026corrselectjoss,
  title = {corrselect: Fast and flexible predictor pruning for data analysis and modeling},
  author = {Gilles Colling},
  journal = {Journal of Open Source Software},
  year = {2026},
  volume = {11},
  number = {118},
  pages = {9539},
  doi = {10.21105/joss.09539},
  url = {https://joss.theoj.org/papers/10.21105/joss.09539},
}

@Manual{colling2026corrselect,
  title = {corrselect: Correlation-Based and Model-Based Predictor Pruning},
  author = {Gilles Colling},
  year = {2026},
  note = {R package version 3.3.0},
  doi = {10.32614/CRAN.package.corrselect},
  url = {https://gillescolling.com/corrselect/},
}
```

Download the citation as
[BibTeX](https://gillescolling.com/corrselect/corrselect.bib) or
[RIS](https://gillescolling.com/corrselect/corrselect.ris).
