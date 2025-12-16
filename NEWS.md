# corrselect 3.0.4

## Test Coverage Improvements

- Removed dead C++ code (`isValidAddition`, `isValidCombination`) from utils.cpp/utils.h
- Added edge case tests for ELS algorithm (force_in validation, threshold boundaries)
- Added edge case tests for association methods (Cramer's V sparse tables, eta edge cases)
- Added tests for corrPrune lexicographic tiebreaker and factor handling
- Added tests for modelPrune custom engine error handling and VIF edge cases
- Test coverage improved from 91.86% to 93.44%

---

# corrselect 3.0.3

## JOSS Review Response

This release addresses reviewer feedback from the JOSS submission.

### Documentation

- **paper.md**: Strengthened comparison with `caret::findCorrelation()` to emphasize the key difference (single solution vs. all maximal subsets)
- **paper.md**: Added explicit graph-theoretic context (maximal cliques / independent sets formulation)
- **paper.md**: Clarified that Bron-Kerbosch and ELS algorithms are implemented natively in C++, not as wrappers around igraph
- **paper.md**: Added note about NP-hard complexity and the recommendation to use exact mode only for p ≤ 100
- **paper.md**: Added code snippet demonstrating the "all subsets" output
- **paper.bib**: Added citations for igraph (Csardi & Nepusz, 2006) and FCBF (Yu & Liu, 2003)
- **README.md**: Added CRAN installation instructions (`install.packages("corrselect")`)
- **README.md**: Fixed mixed model example with `suppressWarnings()` to hide expected VIF computation warnings
- **quickstart vignette**: Fixed GitHub repository reference (GillesColling → gcol33)

### Testing

- Added edge case test: identity matrix (all off-diagonals = 0) returns single subset with all variables
- Added edge case test: perfect duplicates (r = 1.0) are correctly separated into different subsets
- Added threshold boundary test for correlation exactly at threshold

### Infrastructure

- Added GitHub Actions workflow for cross-platform R CMD check (Ubuntu, macOS, Windows)
- Added GitHub Actions workflow for test coverage reporting
- Updated `.gitignore` to exclude build artifacts (`*.Rcheck/`, `*.tar.gz`, `CRAN-SUBMISSION`)

---

# corrselect 3.0.2

## CRAN Compliance

- Single-quoted software names in DESCRIPTION ('lme4', 'glmmTMB') per CRAN policy

## Documentation

- Updated vignettes with improved examples and workflows

---

# corrselect 3.0.1

## Bug Fixes

- **`modelPrune()`**: Fixed infinite loop when VIF computation encountered perfect multicollinearity
  - Added proper handling of `Inf` and `NA` VIF values in pruning loop
  - Clamped extreme R² values (> 0.9999) to prevent division by near-zero
  - Added safety checks to prevent removing all variables
- **`modelPrune()`**: Fixed design matrix extraction for lme4 and glmmTMB engines
  - Now uses `stats::model.matrix()` for all engines (more robust)
  - Eliminated "Could not find columns" warnings
- **Test suite**: All 261 tests pass with zero warnings (CRAN-compliant)

---

# corrselect 3.0.0

## Major Release: Predictor Pruning Toolkit

Version 3.0.0 represents a major expansion of corrselect from a specialized subset enumeration tool into a comprehensive predictor pruning toolkit. **Fully backward compatible** with 2.x - all existing code continues to work.

## Major Features

### New Functions

- **`corrPrune()`**: High-level association-based predictor pruning
  - Model-free pruning using pairwise correlations or associations
  - Automatic measure selection (`measure = "auto"`)
  - Supports exact mode (small p), greedy mode (large p), or auto-selection
  - `force_in` parameter to protect important predictors
  - Returns single pruned data.frame with pairwise associations ≤ threshold

- **`modelPrune()`**: Model-based predictor pruning using diagnostics
  - VIF-based iterative removal of multicollinear predictors
  - Supports multiple engines: `lm`, `glm`, `lme4`, `glmmTMB`
  - **Custom engine support**: Define your own modeling backends (INLA, mgcv, brms, etc.)
  - Prunes fixed effects only (preserves random effects in mixed models)
  - `force_in` parameter for protecting important variables
  - Returns pruned data.frame with final fitted model

### New C++ Backend

- Fast deterministic greedy pruning algorithm
  - Polynomial-time complexity O(p² × k) vs exponential for exact search
  - Handles p > 100 efficiently
  - Deterministic tie-breaking for reproducibility
  - Used by `corrPrune(mode = "greedy")` and `mode = "auto"`

## Enhancements

- Exact methods (`corrSelect()`, `assocSelect()`) now integrate seamlessly with `corrPrune()`
- Deterministic subset selection when multiple maximal sets exist
- Improved error messages for threshold feasibility checks
- Better handling of edge cases (single predictor, all correlated, etc.)
- **Custom engine interface** for `modelPrune()`: Users can define custom modeling backends with `fit` and `diagnostics` functions, enabling integration with any R modeling package

## Documentation

- **Five new comprehensive vignettes** (~60 minutes of content):
  - *Quick Start*: 5-minute introduction to corrPrune() and modelPrune()
  - *Complete Workflows*: Real-world examples across 4 domains (ecology, social science, genomics, clinical)
  - *Comparison with Alternatives*: When to choose corrselect vs caret, Boruta, glmnet
  - *Performance Benchmarks*: Timing comparisons, scalability tests, and optimization guidelines
  - *Advanced Topics*: Algorithms, custom engines (INLA, mgcv), performance optimization, troubleshooting
- **Four new example datasets** with full documentation (bioclim, survey, genes, longitudinal)
- Updated README with quickstart examples and custom engine support
- Full documentation for `corrPrune()` and `modelPrune()`
- Usage examples for all modeling engines

## Package Changes

- Added `lme4` and `glmmTMB` to Suggests (required for respective engines)
- Version bumped to 3.0.0 (major feature release)
- Updated package description to reflect expanded pruning functionality

## Notes

- **No breaking changes**: Version 3.0.0 is fully backward compatible with 2.0.1
- For large predictor sets (p > 20), use `corrPrune(mode = "auto")` for best performance
- Mixed model engines require optional packages: install with `install.packages(c("lme4", "glmmTMB"))`

---

# corrselect 2.0.1

## Bug Fixes

- `force_in` in `MatSelect()` now correctly accepts character column names.
- `els` now correctly lists all valid subsets when a single variable is forced in.
- `corrSelect()` now displays an appropriate warning if only one variable remains after dropping unsupported columns.
- Association matrix construction in `assocSelect()` now safely falls back to 0 for failed or meaningless associations (e.g. empty chi-squared tables due to sparse combinations or unused factor levels).

## Features Added

- `assocSelect()` now supports logical columns by automatically converting them to factors.

---

# corrselect 2.0.0

## Major Release: Mixed-Type Association Selection

Version 2.0.0 introduces support for mixed-type data through the new `assocSelect()` function, enabling subset selection on datasets containing numeric, factor, and ordered variables.

## Major Features

- **`assocSelect()`**: New function for mixed-type data frame interface
  - Handles numeric, factor, and ordered variables
  - Automatic association measure selection based on variable pair types
  - Supports Pearson, Spearman, Kendall correlations
  - Computes Eta-squared for numeric-factor pairs
  - Computes Cramér's V for factor-factor pairs

## Enhancements

- Improved algorithm selection logic
- Better handling of edge cases in subset enumeration
- Enhanced documentation with examples for mixed-type workflows
