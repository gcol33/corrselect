## Test environments
- Local Windows 11, R 4.5.1
- win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Notes on this release

This is a major version update (2.0.1 → 3.0.0) that:

1. Adds two new high-level functions: `corrPrune()` and `modelPrune()`
2. Fixes critical bugs in `modelPrune()`:
   - Infinite loop when VIF computation encountered perfect multicollinearity
   - Design matrix extraction issues with lme4/glmmTMB engines
3. All 261 tests pass with zero warnings (3.8 seconds)
4. Fully backward compatible with version 2.0.1

## Downstream dependencies
There are currently no downstream dependencies for this package.
