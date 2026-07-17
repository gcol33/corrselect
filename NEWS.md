# corrselect 3.2.3

## Bug Fixes

- **C++ backend**: `runELS()` was a single greedy expansion per seed vertex, not an implementation of Eppstein-Loffler-Strash, and could silently miss valid maximal subsets. Replaced with a genuine ELS implementation (degeneracy ordering + per-vertex bounded expansion), sharing a verified Bron-Kerbosch pivot core with the `"bron-kerbosch"` method. Verified against brute-force enumeration.
- **Greedy backend**: an undefined (NaN) association was silently treated as compatible (`NaN > threshold` is `false` in C++); NaN now always registers as a threshold violation.
- **MatSelect**: the symmetry check used exact floating-point equality, inconsistent with the R layer's `1e-8` tolerance, and could reject matrices the R layer had already accepted as symmetric. Added a minimum `ncol >= 2` guard, and `n_rows_used` no longer reports a fabricated row count for matrix input (now `NA`).
- **corrSelect**: numeric `force_in` indices were checked against the final (filtered) correlation matrix but never remapped from the original data frame's column positions, so a numeric index could silently force the wrong variable into every subset after non-numeric or constant columns were dropped.
- **corrPrune**: no longer errors on trivially satisfiable inputs (a single predictor, or a set where every pair exceeds the threshold) -- the pairwise constraint holds vacuously for one variable, so one is now retained instead of raising "No valid subsets found".
- **corrPrune / modelPrune / assocSelect**: undefined (NA) associations were handled inconsistently -- silently treated as zero association, silently passed through the greedy backend, or produced blank/uninformative errors. All undefined associations are now surfaced explicitly with a clear message identifying the affected variable pair(s).
- **corrPrune**: the `measure` argument had no effect on mixed-type data (numeric-numeric pairs always used Pearson regardless of the requested measure). It now customizes numeric-numeric pairs as documented, and the measure actually used per pair-type is reported via a new `assoc_methods_used` attribute.
- **modelPrune**: VIF and condition-number computation matched design-matrix columns to predictor names with a prefix-based regex, which could silently collide (e.g. `"x1"` matching the `"x10"` column). Columns are now resolved via the model's own `assign` bookkeeping.
- **modelPrune**: formulas with a transformed response (e.g. `log(mpg) ~ .`) crashed during formula parsing.
- **corrSubset**: `which = "best"` on a `CorrCombo` with no subsets raised an uninformative "subscript out of bounds" error instead of a clear message.
- Internal Rcpp exports (`runELS`, `runBronKerbosch`) now validate `force_in` bounds directly rather than relying solely on the R-level dispatcher.
- Added duplicate-column-name checks, `force_in`/`by` overlap detection, and a coverage warning when most groups are skipped during grouped aggregation in `corrPrune`.

## Test Coverage Improvements

- Added recovery-style and reference-verified tests for `corrPrune` and `modelPrune`: hand-computed grouped quantile aggregation, exact-value tie-break tests (lexicographic and greedy), a greedy-vs-exact identity check, VIF verified against `car::vif()`, condition-number verified against a manual SVD reference, and seed-repeated recovery tests against simulated ground truth.
- Fixed five `modelPrune` tests that silently passed a nonexistent `threshold` argument instead of `limit`.

---

# corrselect 3.2.2

## Maintenance

- Fixed `.Rbuildignore` to exclude non-standard hidden directories flagged by CRAN incoming checks.
- Removed temporary files from package source.

---

# corrselect 3.2.0

## Breaking Changes

- **CorrCombo class migrated from S4 to S7**: The `CorrCombo` result class now uses the modern S7 object system instead of S4. This brings cleaner construction (`CorrCombo(...)` instead of `new("CorrCombo", ...)`), built-in validation, and forward-looking OOP design.
- **`names` property renamed to `var_names`**: S7 reserves `names` as a property name. Code accessing `result@names` must be updated to `result@var_names`. All other `@` property access (`@subset_list`, `@avg_corr`, etc.) is unchanged.
- The `methods` package is no longer imported; `S7` is now a dependency.

---

# corrselect 3.1.0

## Bug Fixes

- **corrPrune**: Fixed numeric-numeric pair handling in mixed-type data (was incorrectly using Cramer's V instead of Pearson correlation)
- **corrPrune**: Fixed numeric-ordered pair handling (now properly converts ordered to numeric for Spearman correlation)

## Test Coverage Improvements

Coverage improved from 92% to 94%:

- Added tests for optional package measures (bicor, distance, maximal) with proper `skip_if_not_installed()` guards
- Added tests for lme4 and glmmTMB engines in modelPrune
- Added chi-squared edge case tests (sparse contingency tables, NA handling)
- Added VIF edge case tests (perfect collinearity, single predictor)
- Added lexicographic tie-breaking tests with synthetic correlation structures
- Added mixed-type data tests (numeric-ordered, ordered-ordered, factor-factor pairs)
- Added condition_number criterion tests
- findAllMaxSets.R now at 100% coverage
- corrPrune.R now at 97% coverage

---

# corrselect 3.0.7

## New Features

### corrPrune Enhancements

- **Grouped pruning**: New `by` parameter computes association matrices per group and aggregates using the `group_q` quantile (default: 0.5 = median). Useful when correlations vary across experimental conditions or subpopulations.
- **Additional measures for numeric data**:
  - `bicor`: Biweight midcorrelation (requires WGCNA package)
  - `distance`: Distance correlation (requires energy package)
  - `maximal`: Maximal information coefficient (requires minerva package)

### modelPrune Enhancements

- **Condition number criterion**: New `criterion = "condition_number"` option uses SVD-based condition indices for detecting multicollinearity. Higher values indicate greater collinearity.

## Tests

- Added comprehensive tests for grouped pruning functionality
- Added tests for condition_number criterion
- Added edge case tests for single-group and insufficient-rows scenarios

---

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
