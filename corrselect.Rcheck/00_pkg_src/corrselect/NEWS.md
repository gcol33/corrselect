# corrselect 2.0.1

## Bug Fixes

- `force_in` in `MatSelect()` now correctly accepts character column names.
- `els` now correctly lists all valid subsets when a single variable is forced in.
- `corrSelect()` now displays an appropriate warning if only one variable remains after dropping unsupported columns.
- Association matrix construction in `assocSelect()` now safely falls back to 0 for failed or meaningless associations (e.g. empty chi-squared tables due to sparse combinations or unused factor levels).

## Features Added

- `assocSelect()` now supports logical columns by automatically converting them to factors.
