# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

**corrselect** is an R package that performs exhaustive, model-agnostic variable subset selection based on pairwise correlation or association. It identifies all maximal subsets of variables whose pairwise correlations/associations remain below a user-defined threshold, helping reduce multicollinearity while maintaining interpretability.

## Development Commands

### Build and Install
```r
# Load package for development
devtools::load_all()

# Install package locally
devtools::install()

# If modifying C++ code, clean DLL first
devtools::clean_dll()
devtools::install()

# Regenerate documentation (after roxygen2 changes)
devtools::document()
```

### Testing
```r
# Run full test suite
devtools::test()

# Run complete package check (includes tests, examples, documentation)
devtools::check()

# Run specific test file during development
testthat::test_file("tests/testthat/test-corrSelect.R")

# Run test directory
testthat::test_dir("tests/testthat")
```

### Documentation
```r
# Build vignettes
devtools::build_vignettes()

# Build pkgdown site locally (output in docs/)
pkgdown::build_site()
```

## Architecture

### High-Level Design

The package provides five main user-facing functions. Three converge on a common exact-enumeration C++ backend; two (`corrPrune()`, `modelPrune()`) are higher-level predictor-pruning wrappers:

1. **corrSelect()** - Data frame interface for numeric correlation
2. **assocSelect()** - Data frame interface for mixed-type data (numeric, factor, ordered)
3. **MatSelect()** - Direct correlation/association matrix interface
4. **corrPrune()** - Association-based predictor pruning: returns a single pruned data frame (numeric or mixed-type), choosing between exact and greedy search (see Algorithm Selection Logic below)
5. **modelPrune()** - Model-based predictor pruning using VIF/condition-number criteria

`corrSelect()`/`assocSelect()`/`MatSelect()`:
- Preprocess input (handle missing data, validate types)
- Compute or validate a correlation/association matrix
- Call the C++ backend `findAllMaxSets()` to enumerate maximal subsets
- Return a `CorrCombo` S7 object with results

`corrPrune()` computes the same kind of association matrix (via the shared helpers in `R/assoc-metrics.R`, see below) but returns a single pruned `data.frame` rather than every maximal subset, using either exact search (routed through `MatSelect()`) or the greedy C++ backend (`src/method_greedy.cpp`).

### C++ Backend Architecture

**Entry point**: `src/corrselect_main.cpp::findAllMaxSets()`

The C++ layer implements three graph-theoretic algorithms for a graph where edges represent "sufficiently low correlation" (built by `buildCompatibilityMatrix()` in `src/clique_core.cpp`):

1. **Eppstein-Löffler-Strash (ELS)** (`src/method_els.cpp`)
   - Recommended when using `force_in` (variables that must appear in all subsets)
   - Exact enumeration of maximal independent sets: degeneracy ordering, then per-vertex pivoted Bron-Kerbosch over the induced subgraph
   - Shares its pivoted-search core with plain Bron-Kerbosch (see `clique_core.cpp` below)

2. **Bron-Kerbosch** (`src/method_bronkerbosch.cpp`)
   - Optional pivoting for performance (`use_pivot = TRUE`)
   - Default algorithm when no `force_in` specified
   - Exact enumeration; `tests/testthat/test-brute-force-ground-truth.R` verifies both this and ELS against an independent brute-force enumerator

3. **Greedy** (`src/method_greedy.cpp`)
   - Heuristic, not exhaustive: iteratively removes the "worst" variable (highest violation count, then highest max/avg association, then smallest index) until no pairwise violation remains
   - Returns a single pruned subset, not all maximal subsets
   - Used by `corrPrune(mode = "greedy")`, and automatically by `corrPrune(mode = "auto")` when the predictor count exceeds `max_exact_p`

**Shared clique-search core** (`src/clique_core.cpp`, `src/clique_core.h`):
- `buildCompatibilityMatrix()`: builds the boolean adjacency matrix (edge iff `abs(corMatrix(i,j)) <= threshold`)
- `bronKerboschPivot()`: the pivoted Bron-Kerbosch recursion shared by both `method_bronkerbosch.cpp` and `method_els.cpp` -- which named algorithm results depends on how the caller seeds R/P/X (see the header comment)

**Key types** (`src/corrselect_types.h`):
- `Combo` = `std::vector<int>` (variable indices)
- `ComboList` = `std::vector<Combo>` (collection of subsets)

**Utilities** (`src/utils.cpp`, `src/utils.h`):
- `validateCorMatrix()`/`validateForcedIndices()`: shared entry-point validation (square, unit diagonal, symmetric-or-upper-triangular, in-bounds force_in) used by all four Rcpp-exported backends (`findAllMaxSets`, `runELS`, `runBronKerbosch`, `greedyPruneBackend`)
- Correlation statistics (mean, min, max for subsets)

### R Layer Architecture

**Data frame preprocessing**:
- `corrSelect()`: Filters to numeric columns only, removes NA rows, computes correlation matrix using `cor_method` parameter
- `assocSelect()`: Handles mixed types by computing appropriate metrics for each pair type (Pearson, Spearman, Kendall, Eta-squared, Cramér's V)
- `corrPrune()`: Same association metrics as above (numeric-only or mixed-type), optionally computed per group (`by`) and aggregated by quantile (`group_q`); dispatches to exact or greedy search per `mode`

**Shared association-metric primitives** (`R/assoc-metrics.R`):
- `.pairwise_assoc_value()`: single-pair association (Pearson/Spearman/Kendall/bicor/distance/maximal/eta/Cramér's V), including NA/constant-column gating -- used by `assocSelect()` and `corrPrune()`'s mixed-type branch
- `.full_assoc_method_map()`: type-pair -> method lookup table
- `.mixed_type_assoc_matrix()`: pairwise mixed-type matrix builder, used by `assocSelect()` and `corrPrune()`
- `.numeric_assoc_matrix()`: vectorized numeric-only matrix builder (constant columns get association 0, not NA), used by `corrSelect()` and `corrPrune()`'s all-numeric branch
- Extracted specifically so a fix to NA/constant-column handling or a metric's definition applies to every caller at once (see #44)

**CorrCombo S7 class** (`R/CorrCombo.R`):
- Stores all discovered subsets with metadata
- Properties: `subset_list`, `avg_corr`, `min_corr`, `max_corr`, `threshold`, `forced_in`, `search_type`, `cor_method`, `n_rows_used`, `var_names`
- Custom `print()` method for user-friendly output
- `as.data.frame()` method for tidy data extraction

**Helper functions**:
- `corrSubset()` (`R/corrSubset.R`): Extracts specific subsets from original data, with option to keep non-numeric columns (`keepExtra = TRUE`)
- `findAllMaxSets()` (`R/findAllMaxSets.R`): R wrapper calling C++ via Rcpp

### Algorithm Selection Logic

Default method selection in `corrSelect()`, `assocSelect()`, and `MatSelect()`:
- If `force_in` provided → use ELS algorithm
- Otherwise → use Bron-Kerbosch algorithm

Users can override by explicitly setting `method = "els"` or `method = "bron-kerbosch"`.

`corrPrune()` has a separate, independent `mode` dispatch (exact vs. greedy, not ELS vs. Bron-Kerbosch):
- `mode = "auto"` (default): exact search if the predictor count is `<= max_exact_p` (default 100) and there are at least 2 predictors with `threshold > 0`; otherwise greedy
- `mode = "exact"`: always exact (routes through `MatSelect()`)
- `mode = "greedy"`: always the greedy C++ backend (`src/method_greedy.cpp`), which supports a single predictor and `threshold = 0`

### Association Metrics for Mixed Data

`assocSelect()` automatically selects metrics based on variable pair types (the same table, minus the configurable numeric-ordered/ordered-ordered methods, is used by `corrPrune()`'s mixed-type branch -- see `.full_assoc_method_map()` above):

| Type 1 | Type 2 | Default Metric |
|--------|--------|----------------|
| numeric | numeric | Pearson (configurable via `method_num_num`) |
| numeric | ordered | Spearman (configurable via `method_num_ord`) |
| numeric | factor | Eta-squared (fixed) |
| ordered | ordered | Spearman (configurable via `method_ord_ord`) |
| ordered | factor | Cramér's V (fixed) |
| factor | factor | Cramér's V (fixed) |

External correlation methods require additional packages:
- `bicor`: WGCNA package
- `distance`: energy package
- `maximal`: minerva package

## File Organization

```
R/                           # R source files
├── corrSelect.R            # Numeric data frame interface
├── assocSelect.R           # Mixed-type data frame interface
├── MatSelect.R             # Matrix interface
├── corrPrune.R             # Association-based predictor pruning (exact/greedy)
├── modelPrune.R            # Model-based predictor pruning (VIF/condition number)
├── assoc-metrics.R         # Shared association-metric primitives (see above)
├── CorrCombo.R             # S7 class definition and methods
├── corrSubset.R            # Subset extraction helper
├── findAllMaxSets.R        # R wrapper for C++ entry point
├── corrselect-package.R    # Package-level documentation
├── cor_example.R           # Example correlation matrix / data generator
├── data.R                  # Documentation for bundled example datasets
└── zzz.R                   # Package load hooks

src/                         # C++ source files (Rcpp)
├── corrselect_main.cpp     # Main entry point and algorithm dispatch
├── corrselect_types.h      # Type definitions (Combo, ComboList)
├── clique_core.{cpp,h}     # Shared pivoted Bron-Kerbosch core (used by both method_els and method_bronkerbosch)
├── method_els.{cpp,h}      # Eppstein-Löffler-Strash implementation
├── method_bronkerbosch.{cpp,h}  # Bron-Kerbosch implementation
├── method_greedy.{cpp,h}   # Greedy pruning backend (used by corrPrune())
├── utils.{cpp,h}           # Shared entry-point validation and correlation stats
└── RcppExports.cpp         # Generated Rcpp bindings

tests/testthat/             # Unit tests
├── test-corrSelect.R
├── test-assocSelect.R
├── test-corrPrune.R
├── test-modelPrune.R
├── test-CorrCombo.R
├── test-corrMatSelect-els.R
├── test-corrMatSelect-bron-kerbosch.R
├── test-corrSubset.R
├── test-findAllMaxSets.R
├── test-cpp-validation-consistency.R
├── test-reference-associations.R  # Reference-verified association-metric checks
└── test-brute-force-ground-truth.R  # Brute-force maximality/exhaustiveness/cross-algorithm checks

vignettes/                   # Long-form documentation
man/                         # Generated documentation (roxygen2)
docs/                        # Generated pkgdown website
claude/                      # Claude Code working files (git-ignored)
                             # Store generated .md files and notes here
```

## Important Conventions

### Index Conversion
- **R layer**: 1-based indexing (standard R convention)
- **C++ layer**: 0-based indexing (standard C++ convention)
- Conversion happens in `corrselect_main.cpp`: subtract 1 when entering C++, add 1 when returning to R

### Force-In Variables
The `force_in` parameter can be:
- Character vector of variable names (R layer converts to indices)
- Numeric vector of column indices (user must provide 1-based)

These variables appear in every returned subset. They are converted to 0-based indices before being passed to C++.

### Result Ordering
Results are sorted by:
1. Size (descending) - larger subsets first
2. Average absolute correlation (ascending) - lower correlation preferred

This happens in C++ before returning to R.

### Missing Data Handling
Both `corrSelect()` and `assocSelect()` remove rows with any NA values before computing the correlation matrix. A warning is issued if rows are dropped. The number of rows actually used is stored in the `CorrCombo` object's `n_rows_used` property.

## Testing Guidelines

- Keep tests fast and reproducible
- Always use `set.seed()` for random data
- Include edge cases (empty data, single variable, all correlated, all uncorrelated)
- Test both algorithms (ELS and Bron-Kerbosch)
- Test force_in functionality
- Test mixed-type data scenarios in assocSelect tests

## Documentation

All exported functions use roxygen2 documentation with:
- `@param` descriptions
- `@return` type and structure
- `@details` for algorithm specifics
- `@examples` that are executable
- `@seealso` cross-references

Vignettes provide:
- Usage examples with real-world scenarios
- Performance comparisons between algorithms
- Guidance on threshold selection
- Mixed-type data workflows

### CRITICAL: DOI and URL Handling

**NEVER remove or unlink DOIs/URLs without explicit user approval.**

When CRAN check reports broken DOIs or URLs:
1. **ALWAYS** verify the correct DOI/URL first
2. **ASK the user** before making any changes
3. If a DOI is incorrect, search for the correct one and propose an update
4. Only unlink/remove with explicit user permission

Example: If a DOI like `10.1016/S0167-5060(06)80001-3` returns 404, search for the correct DOI (e.g., `10.1016/S0167-5060(13)71063-X`) and ask user to verify before updating.
