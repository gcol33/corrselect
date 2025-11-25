# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Overview

**corrselect** is an R package that performs exhaustive, model-agnostic
variable subset selection based on pairwise correlation or association.
It identifies all maximal subsets of variables whose pairwise
correlations/associations remain below a user-defined threshold, helping
reduce multicollinearity while maintaining interpretability.

## Development Commands

### Build and Install

``` r

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

``` r

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

``` r

# Build vignettes
devtools::build_vignettes()

# Build pkgdown site locally (output in docs/)
pkgdown::build_site()
```

## Architecture

### High-Level Design

The package provides three main user-facing functions that all converge
on a common C++ backend:

1.  **corrSelect()** - Data frame interface for numeric correlation
2.  **assocSelect()** - Data frame interface for mixed-type data
    (numeric, factor, ordered)
3.  **MatSelect()** - Direct correlation/association matrix interface

All three functions: - Preprocess input (handle missing data, validate
types) - Compute or validate a correlation/association matrix - Call the
C++ backend `findAllMaxSets()` to enumerate maximal subsets - Return a
`CorrCombo` S4 object with results

### C++ Backend Architecture

**Entry point**: `src/corrselect_main.cpp::findAllMaxSets()`

The C++ layer implements two exact graph-theoretic algorithms for
enumerating all maximal cliques in a graph where edges represent
“sufficiently low correlation”:

1.  **Eppstein-Löffler-Strash (ELS)** (`src/method_els.cpp`)
    - Recommended when using `force_in` (variables that must appear in
      all subsets)
    - Exact enumeration of maximal independent sets
2.  **Bron-Kerbosch** (`src/method_bronkerbosch.cpp`)
    - Optional pivoting for performance (`use_pivot = TRUE`)
    - Default algorithm when no `force_in` specified

**Key types** (`src/corrselect_types.h`): - `Combo` = `std::vector<int>`
(variable indices) - `ComboList` = `std::vector<Combo>` (collection of
subsets)

**Utilities** (`src/utils.cpp`, `src/utils.h`): - Matrix validation -
Correlation statistics (mean, min, max for subsets)

### R Layer Architecture

**Data frame preprocessing**: -
[`corrSelect()`](https://gillescolling.com/corrselect/reference/corrSelect.md):
Filters to numeric columns only, removes NA rows, computes correlation
matrix using `cor_method` parameter -
[`assocSelect()`](https://gillescolling.com/corrselect/reference/assocSelect.md):
Handles mixed types by computing appropriate metrics for each pair type
(Pearson, Spearman, Kendall, Eta-squared, Cramér’s V)

**CorrCombo S4 class** (`R/CorrCombo.R`): - Stores all discovered
subsets with metadata - Slots: `subset_list`, `avg_corr`, `min_corr`,
`max_corr`, `threshold`, `forced_in`, `search_type`, `cor_method`,
`n_rows_used`, `names` - Custom `show()` method for user-friendly
output - [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
method for tidy data extraction

**Helper functions**: -
[`corrSubset()`](https://gillescolling.com/corrselect/reference/corrSubset.md)
(`R/corrSubset.R`): Extracts specific subsets from original data, with
option to keep non-numeric columns (`keepExtra = TRUE`) -
`findAllMaxSets()` (`R/findAllMaxSets.R`): R wrapper calling C++ via
Rcpp

### Algorithm Selection Logic

Default method selection in both
[`corrSelect()`](https://gillescolling.com/corrselect/reference/corrSelect.md)
and
[`assocSelect()`](https://gillescolling.com/corrselect/reference/assocSelect.md): -
If `force_in` provided → use ELS algorithm - Otherwise → use
Bron-Kerbosch algorithm

Users can override by explicitly setting `method = "els"` or
`method = "bron-kerbosch"`.

### Association Metrics for Mixed Data

[`assocSelect()`](https://gillescolling.com/corrselect/reference/assocSelect.md)
automatically selects metrics based on variable pair types:

| Type 1  | Type 2  | Default Metric                               |
|---------|---------|----------------------------------------------|
| numeric | numeric | Pearson (configurable via `method_num_num`)  |
| numeric | ordered | Spearman (configurable via `method_num_ord`) |
| numeric | factor  | Eta-squared (fixed)                          |
| ordered | ordered | Spearman (configurable via `method_ord_ord`) |
| ordered | factor  | Cramér’s V (fixed)                           |
| factor  | factor  | Cramér’s V (fixed)                           |

External correlation methods require additional packages: - `bicor`:
WGCNA package - `distance`: energy package - `maximal`: minerva package

## File Organization

    R/                           # R source files
    ├── corrSelect.R            # Numeric data frame interface
    ├── assocSelect.R           # Mixed-type data frame interface
    ├── MatSelect.R             # Matrix interface
    ├── CorrCombo.R             # S4 class definition and methods
    ├── corrSubset.R            # Subset extraction helper
    └── findAllMaxSets.R        # R wrapper for C++ entry point

    src/                         # C++ source files (Rcpp)
    ├── corrselect_main.cpp     # Main entry point and algorithm dispatch
    ├── corrselect_types.h      # Type definitions (Combo, ComboList)
    ├── method_els.{cpp,h}      # Eppstein-Löffler-Strash implementation
    ├── method_bronkerbosch.{cpp,h}  # Bron-Kerbosch implementation
    ├── utils.{cpp,h}           # Matrix validation and correlation stats
    └── RcppExports.cpp         # Generated Rcpp bindings

    tests/testthat/             # Unit tests
    ├── test-corrSelect.R
    ├── test-assocSelect.R
    ├── test-CorrCombo.R
    ├── test-corrMatSelect-els.R
    ├── test-corrMatSelect-bron-kerbosch.R
    └── test-corrSubset.R

    vignettes/                   # Long-form documentation
    man/                         # Generated documentation (roxygen2)
    docs/                        # Generated pkgdown website
    claude/                      # Claude Code working files (git-ignored)
                                 # Store generated .md files and notes here

## Important Conventions

### Index Conversion

- **R layer**: 1-based indexing (standard R convention)
- **C++ layer**: 0-based indexing (standard C++ convention)
- Conversion happens in `corrselect_main.cpp`: subtract 1 when entering
  C++, add 1 when returning to R

### Force-In Variables

The `force_in` parameter can be: - Character vector of variable names (R
layer converts to indices) - Numeric vector of column indices (user must
provide 1-based)

These variables appear in every returned subset. They are converted to
0-based indices before being passed to C++.

### Result Ordering

Results are sorted by: 1. Size (descending) - larger subsets first 2.
Average absolute correlation (ascending) - lower correlation preferred

This happens in C++ before returning to R.

### Missing Data Handling

Both
[`corrSelect()`](https://gillescolling.com/corrselect/reference/corrSelect.md)
and
[`assocSelect()`](https://gillescolling.com/corrselect/reference/assocSelect.md)
remove rows with any NA values before computing the correlation matrix.
A warning is issued if rows are dropped. The number of rows actually
used is stored in the `CorrCombo` object’s `n_rows_used` slot.

## Testing Guidelines

- Keep tests fast and reproducible
- Always use [`set.seed()`](https://rdrr.io/r/base/Random.html) for
  random data
- Include edge cases (empty data, single variable, all correlated, all
  uncorrelated)
- Test both algorithms (ELS and Bron-Kerbosch)
- Test force_in functionality
- Test mixed-type data scenarios in assocSelect tests

## Documentation

All exported functions use roxygen2 documentation with: - `@param`
descriptions - `@return` type and structure - `@details` for algorithm
specifics - `@examples` that are executable - `@seealso`
cross-references

Vignettes provide: - Usage examples with real-world scenarios -
Performance comparisons between algorithms - Guidance on threshold
selection - Mixed-type data workflows

### CRITICAL: DOI and URL Handling

**NEVER remove or unlink DOIs/URLs without explicit user approval.**

When CRAN check reports broken DOIs or URLs: 1. **ALWAYS** verify the
correct DOI/URL first 2. **ASK the user** before making any changes 3.
If a DOI is incorrect, search for the correct one and propose an update
4. Only unlink/remove with explicit user permission

Example: If a DOI like `10.1016/S0167-5060(06)80001-3` returns 404,
search for the correct DOI (e.g., `10.1016/S0167-5060(13)71063-X`) and
ask user to verify before updating.
