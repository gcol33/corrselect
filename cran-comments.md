## Test environments

* local: Windows 11 x64 (build 26200), R 4.5.1
* GitHub Actions (via R CMD check):
  - windows-latest (R-release)
  - macOS-latest (R-release)
  - ubuntu-latest (R-devel, R-release, R-oldrel)

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

- All 261 tests pass successfully
- All examples run without errors
- All vignettes build successfully with accessibility improvements (alt-text for all figures)

## Package updates

This is a major release (3.0.0) that is **fully backward compatible** with version 2.x.

### Major additions:
- New `corrPrune()` function for high-level association-based pruning
- New `modelPrune()` function for VIF-based model pruning
- New C++ greedy algorithm for high-dimensional data (p > 100)
- Custom engine support for integrating any modeling framework

### Bug fixes:
- Fixed infinite loop in `modelPrune()` with perfect multicollinearity
- Fixed design matrix extraction for lme4 and glmmTMB engines
- Improved Unicode handling in documentation (LaTeX compatibility)

### Documentation improvements:
- Enhanced vignettes with detailed explanations and workflows
- Added alt-text to all figures for accessibility
- Comprehensive examples across ecological, survey, genomic, and mixed model contexts

## Downstream dependencies

There are currently no downstream dependencies for this package.
