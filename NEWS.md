# corrselect 2.0.0

## Major Changes

- **New Function: `assocSelect()`**  
  Enables variable subset selection for **mixed data frames** (numeric, ordered, and factor variables) using appropriate association measures (e.g., Cramér's V, Spearman, Eta).  
  Automatically chooses the correct method for each variable type pair.

- **S4 Class Improvements**  
  The `CorrCombo` object now includes:
  - `assoc_methods` field for mixed data input
  - `cor_method = "mixed"` when used with `assocSelect()`
  - Improved `show()` method with aligned output and better readability

## Minor Improvements

- Better error messages for unsupported column types or invalid `force_in` entries
- Cleaner `show()` display with 2-per-row method alignment for `assoc_methods`
