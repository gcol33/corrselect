# Changelog

## corrselect 3.0.0

### Major Release: Predictor Pruning Toolkit

Version 3.0.0 represents a major expansion of corrselect from a
specialized subset enumeration tool into a comprehensive predictor
pruning toolkit. **Fully backward compatible** with 2.x - all existing
code continues to work.

### Bug Fixes

- **[`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)**:
  Fixed infinite loop when VIF computation encountered perfect
  multicollinearity
  - Added proper handling of `Inf` and `NA` VIF values in pruning loop
  - Clamped extreme R² values (\> 0.9999) to prevent division by
    near-zero
  - Added safety checks to prevent removing all variables
- **[`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)**:
  Fixed design matrix extraction for lme4 and glmmTMB engines
  - Now uses
    [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
    for all engines (more robust)
  - Eliminated “Could not find columns” warnings
- **Test suite**: All 261 tests pass with zero warnings (CRAN-compliant)

### Major Features

#### New Functions

- **[`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)**:
  High-level association-based predictor pruning
  - Model-free pruning using pairwise correlations or associations
  - Automatic measure selection (`measure = "auto"`)
  - Supports exact mode (small p), greedy mode (large p), or
    auto-selection
  - `force_in` parameter to protect important predictors
  - Returns single pruned data.frame with pairwise associations ≤
    threshold
- **[`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)**:
  Model-based predictor pruning using diagnostics
  - VIF-based iterative removal of multicollinear predictors
  - Supports multiple engines: `lm`, `glm`, `lme4`, `glmmTMB`
  - **Custom engine support**: Define your own modeling backends (INLA,
    mgcv, brms, etc.)
  - Prunes fixed effects only (preserves random effects in mixed models)
  - `force_in` parameter for protecting important variables
  - Returns pruned data.frame with final fitted model

#### New C++ Backend

- Fast deterministic greedy pruning algorithm
  - Polynomial-time complexity O(p² × k) vs exponential for exact search
  - Handles p \> 100 efficiently
  - Deterministic tie-breaking for reproducibility
  - Used by `corrPrune(mode = "greedy")` and `mode = "auto"`

### Enhancements

- Exact methods
  ([`corrSelect()`](https://gillescolling.com/corrselect/reference/corrSelect.md),
  [`assocSelect()`](https://gillescolling.com/corrselect/reference/assocSelect.md))
  now integrate seamlessly with
  [`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)
- Deterministic subset selection when multiple maximal sets exist
- Improved error messages for threshold feasibility checks
- Better handling of edge cases (single predictor, all correlated, etc.)
- **Custom engine interface** for
  [`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md):
  Users can define custom modeling backends with `fit` and `diagnostics`
  functions, enabling integration with any R modeling package

### Documentation

- **Five new comprehensive vignettes** (~60 minutes of content):
  - *Quick Start*: 5-minute introduction to corrPrune() and modelPrune()
  - *Complete Workflows*: Real-world examples across 4 domains (ecology,
    social science, genomics, clinical)
  - *Comparison with Alternatives*: When to choose corrselect vs caret,
    Boruta, glmnet
  - *Performance Benchmarks*: Timing comparisons, scalability tests, and
    optimization guidelines
  - *Advanced Topics*: Algorithms, custom engines (INLA, mgcv),
    performance optimization, troubleshooting
- **Four new example datasets** with full documentation (bioclim,
  survey, genes, longitudinal)
- Updated README with quickstart examples and custom engine support
- Full documentation for
  [`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)
  and
  [`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)
- Usage examples for all modeling engines

### Package Changes

- Added `lme4` and `glmmTMB` to Suggests (required for respective
  engines)
- Version bumped to 3.0.0 (major feature release)
- Updated package description to reflect expanded pruning functionality

### Notes

- **No breaking changes**: Version 3.0.0 is fully backward compatible
  with 2.0.1
- For large predictor sets (p \> 20), use `corrPrune(mode = "auto")` for
  best performance
- Mixed model engines require optional packages: install with
  `install.packages(c("lme4", "glmmTMB"))`

------------------------------------------------------------------------

## corrselect 2.0.1

CRAN release: 2025-09-08

### Bug Fixes

- `force_in` in
  [`MatSelect()`](https://gillescolling.com/corrselect/reference/MatSelect.md)
  now correctly accepts character column names.
- `els` now correctly lists all valid subsets when a single variable is
  forced in.
- [`corrSelect()`](https://gillescolling.com/corrselect/reference/corrSelect.md)
  now displays an appropriate warning if only one variable remains after
  dropping unsupported columns.
- Association matrix construction in
  [`assocSelect()`](https://gillescolling.com/corrselect/reference/assocSelect.md)
  now safely falls back to 0 for failed or meaningless associations
  (e.g. empty chi-squared tables due to sparse combinations or unused
  factor levels).

### Features Added

- [`assocSelect()`](https://gillescolling.com/corrselect/reference/assocSelect.md)
  now supports logical columns by automatically converting them to
  factors.
