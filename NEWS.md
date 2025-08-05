# corrselect 1.0.0

## Initial Release

- Added `corrSelect()` for selecting variable subsets below a user-specified correlation threshold.
- Supports two exact algorithms: Eppstein–Löffler–Strash (ELS) and Bron–Kerbosch (BK).
- Provides data-frame and matrix interfaces (`corrSelect()` and `MatSelect()`).
- Supports multiple correlation measures (`pearson`, `spearman`, `kendall`, `bicor`, `distance`, `maximal`).
- Implements `force_in` argument to require specific variables in every subset.
- Handles missing values by removing incomplete rows.
- Returns `CorrCombo` S4 object summarizing all maximal valid subsets with correlation statistics.
- Includes helper functions for extracting subsets (`corrSubset()`) and data frame conversion (`as.data.frame()`).

