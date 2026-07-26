#' Association-Based Predictor Pruning
#'
#' `corrPrune()` performs model-free variable subset selection by iteratively
#' removing predictors until all pairwise associations fall below a specified
#' threshold. It returns a single pruned data frame with predictors that satisfy
#' the association constraint.
#'
#' @param data A data.frame containing candidate predictors.
#' @param threshold Numeric scalar. Maximum allowed pairwise association
#'   (default: 0.7). Must be in `[0, 1]` -- every supported association
#'   measure is bounded in `[0, 1]` (in absolute value), so this range is
#'   enforced the same way regardless of `mode` (`threshold = 0` is valid
#'   only in `mode = "greedy"`; see Mode Selection below).
#' @param measure Character string specifying the numeric-numeric association
#'   measure to use. One of `"auto"` (default, Pearson), `"pearson"`,
#'   `"spearman"`, `"kendall"`, `"bicor"`, `"distance"`, or `"maximal"`. This
#'   only customizes numeric-numeric pairs; every other pair-type combination
#'   is fixed and not affected by `measure`: eta-squared for
#'   numeric-categorical pairs, Cramer's V for categorical-categorical pairs,
#'   and Spearman for numeric-ordered and ordered-ordered pairs. The measure
#'   actually used for each pair-type combination is reported in the
#'   `assoc_methods_used` attribute of the result.
#' @param mode Character string specifying the search algorithm. Options:
#'   - `"auto"` (default): uses exact search if number of predictors <= `max_exact_p`
#'     and there are at least 2 predictors with `threshold > 0`, otherwise uses
#'     greedy search (exact search requires both, since it routes through
#'     \code{\link{MatSelect}()})
#'   - `"exact"`: exhaustive search for maximal subsets (may be slow for large p);
#'     requires at least 2 predictors and `threshold > 0`
#'   - `"greedy"`: fast approximate search using iterative removal; supports a
#'     single predictor and `threshold = 0`
#' @param force_in Character vector of variable names that must be retained in
#'   the final subset. Default: NULL.
#' @param by Character vector naming one or more grouping variables. If provided,
#'   associations are computed separately within each group, then aggregated
#'   using the quantile specified by `group_q`. Default: NULL (no grouping).
#' @param group_q Numeric scalar in (0, 1]. Quantile used to aggregate
#'   associations across groups when `by` is provided. Default: 1 (maximum,
#'   ensuring threshold holds in all groups). Use 0.9 for 90th percentile, etc.
#' @param max_exact_p Integer. Maximum number of predictors for which exact
#'   mode is used when `mode = "auto"`. Default: 100.
#' @param ... Additional arguments (reserved for future use).
#'
#' @return A data.frame containing the pruned subset of predictors, with the
#'   selected columns unchanged from `data` (same types and values --
#'   character/logical/integer columns are converted internally only for
#'   association computation, never in the returned data). The result
#'   has the following attributes:
#'   \describe{
#'     \item{selected_vars}{Character vector of retained variable names}
#'     \item{removed_vars}{Character vector of removed variable names}
#'     \item{mode}{Character string indicating which mode was used ("exact" or "greedy")}
#'     \item{measure}{Character string indicating which measure was used for numeric-numeric pairs}
#'     \item{assoc_methods_used}{Named list mapping each pair-type combination (e.g. "numeric_numeric", "numeric_factor") to the association method actually used}
#'     \item{threshold}{The threshold value used}
#'     \item{n_rows_used}{Number of complete-case rows used to compute associations (see Details); the returned data itself is not row-filtered}
#'   }
#'
#' @details
#' `corrPrune()` identifies a subset of predictors whose pairwise associations
#' are all below `threshold`. The function works in several stages:
#'
#' 1. **Variable type detection**: Identifies numeric vs. categorical predictors
#' 2. **Constant-column removal**: Predictors that are constant across every
#'    complete-case row are excluded with a warning, since their association
#'    with anything is undefined and they would otherwise ride into the
#'    result without contributing any information
#' 3. **Association measurement**: Computes appropriate pairwise associations
#' 4. **Grouping (optional)**: If `by` is specified, computes associations within
#'    each group and aggregates using the specified quantile
#' 5. **Feasibility check**: Verifies that `force_in` variables satisfy the
#'    threshold constraint (a `force_in` variable excluded for being constant
#'    also errors here)
#' 6. **Subset selection**: Uses either exact or greedy search to find a valid subset
#'
#' **Grouped Pruning**: When `by` is provided, the function ensures the selected
#' predictors satisfy the threshold constraint across groups. For example, with
#' `group_q = 1` (default), the returned predictors will have pairwise associations
#' below `threshold` in *all* groups. With `group_q = 0.9`, they will satisfy
#' the constraint in at least 90% of groups.
#'
#' **Mode Selection**: Exact mode guarantees finding all maximal subsets and
#' returns the largest one. Greedy mode is faster but approximate, using an
#' iterative removal strategy based on association scores.
#'
#' **Tie-Breaking**: When multiple subsets or variables are equally good,
#' deterministic tie-breaking is applied:
#' \itemize{
#'   \item \strong{Exact mode}: Selects by (1) largest subset size, (2) lowest
#'     average correlation, (3) alphabetically first variable names. Column
#'     order does not affect the result.
#'   \item \strong{Greedy mode}: Removes the variable with (1) most constraint
#'     violations, (2) highest max association, (3) highest average association,
#'     (4) lowest column index. Column order can influence the result when
#'     earlier criteria are tied.
#' }
#' To see all maximal subsets instead of a single selection, use
#' \code{\link{corrSelect}()}.
#'
#' @seealso
#' \code{\link{corrSelect}} for exhaustive subset enumeration,
#' \code{\link{assocSelect}} for mixed-type data subset enumeration,
#' \code{\link{modelPrune}} for model-based predictor pruning.
#'
#' @examples
#' # Basic numeric data pruning
#' data(mtcars)
#' pruned <- corrPrune(mtcars, threshold = 0.7)
#' names(pruned)
#'
#' # Force certain variables to be included
#' pruned <- corrPrune(mtcars, threshold = 0.7, force_in = "mpg")
#'
#' # Use greedy mode for faster computation
#' pruned <- corrPrune(mtcars, threshold = 0.7, mode = "greedy")
#'
#' @importFrom stats complete.cases quantile
#' @export
corrPrune <- function(
  data,
  threshold   = 0.7,
  measure     = "auto",
  mode        = "auto",
  force_in    = NULL,
  by          = NULL,
  group_q     = 1,
  max_exact_p = 100,
  ...
) {

  force_in <- .validate_corrPrune_args(
    data, threshold, measure, mode, force_in, by, group_q, max_exact_p
  )

  # Store original data for final subsetting
  data_orig <- data

  detected <- .detect_corrPrune_types(data, by)
  data  <- detected$data
  types <- detected$types

  constant_dropped <- .drop_constant_corrPrune_columns(data, types, force_in)
  data  <- constant_dropped$data
  types <- constant_dropped$types

  measure_used <- .resolve_corrPrune_measure(measure)

  assoc_result <- if (is.null(by)) {
    .compute_ungrouped_assoc_matrix(data, measure_used, types)
  } else {
    .compute_grouped_assoc_matrix(data, data_orig, by, types, measure_used, group_q)
  }
  A_eff <- assoc_result$mat
  assoc_methods_used <- assoc_result$assoc_methods_used
  n_rows_used <- assoc_result$n_rows_used

  .reject_undefined_associations(A_eff)

  .check_corrPrune_force_in_feasibility(A_eff, force_in, threshold)

  p <- ncol(data)
  mode_used <- .resolve_corrPrune_mode(mode, p, max_exact_p, threshold)

  selected_vars <- if (mode_used == "exact") {
    .run_corrPrune_exact(A_eff, threshold, force_in)
  } else {
    .run_corrPrune_greedy(A_eff, threshold, force_in)
  }

  # Return the pruned data with selected variables, subsetting from the
  # caller's original untouched columns (data_orig) rather than the
  # internally-converted `data` (character/logical -> factor, integer ->
  # numeric), so corrPrune() only ever removes columns -- it never silently
  # changes the type of a column it keeps.
  data_pruned <- data_orig[, selected_vars, drop = FALSE]

  # Compute removed variables
  all_vars <- colnames(data)
  removed_vars <- setdiff(all_vars, selected_vars)

  # Add attributes for metadata
  attr(data_pruned, "selected_vars") <- selected_vars
  attr(data_pruned, "removed_vars") <- removed_vars
  attr(data_pruned, "mode") <- mode_used
  attr(data_pruned, "measure") <- measure_used
  attr(data_pruned, "assoc_methods_used") <- assoc_methods_used
  attr(data_pruned, "threshold") <- threshold
  attr(data_pruned, "n_vars_original") <- ncol(data)
  attr(data_pruned, "n_vars_selected") <- length(selected_vars)
  attr(data_pruned, "n_rows_used") <- n_rows_used

  return(data_pruned)
}

# ===========================================================================
# Helper functions
# ===========================================================================

#' Validate corrPrune()'s arguments and return the normalized (deduplicated)
#' `force_in`, its only argument that needs normalizing beyond validation.
#' @noRd
.validate_corrPrune_args <- function(data, threshold, measure, mode, force_in, by, group_q, max_exact_p) {
  # Check data is data.frame-like
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }

  # Check for zero columns
  if (ncol(data) == 0L) {
    stop("'data' must contain at least one column")
  }

  # Check threshold
  if (!is.numeric(threshold) || length(threshold) != 1L) {
    stop("'threshold' must be a single numeric value")
  }
  if (is.na(threshold) || threshold < 0 || threshold > 1) {
    stop("'threshold' must be in the range [0, 1] and non-missing")
  }

  # Check measure
  if (!is.character(measure) || length(measure) != 1L) {
    stop("'measure' must be a single character string")
  }

  # Check mode
  if (!is.character(mode) || length(mode) != 1L) {
    stop("'mode' must be a single character string")
  }
  if (!mode %in% c("auto", "exact", "greedy")) {
    stop("'mode' must be one of: 'auto', 'exact', 'greedy'")
  }

  # Check for duplicate column names up front: downstream name-based matching
  # (force_in, by) would otherwise silently resolve to only the first match.
  if (anyDuplicated(names(data))) {
    stop("'data' has duplicate column names: ",
         paste(unique(names(data)[duplicated(names(data))]), collapse = ", "))
  }

  # Check force_in
  if (!is.null(force_in)) {
    if (!is.character(force_in)) {
      stop("'force_in' must be a character vector of variable names")
    }
    force_in <- unique(force_in)
    missing_vars <- setdiff(force_in, names(data))
    if (length(missing_vars) > 0L) {
      stop(sprintf(
        "'force_in' variable(s) not found in data: %s",
        paste(missing_vars, collapse = ", ")
      ))
    }
  }

  # Check by (grouping variable)
  if (!is.null(by)) {
    if (!is.character(by)) {
      stop("'by' must be a character vector of variable names")
    }
    missing_by <- setdiff(by, names(data))
    if (length(missing_by) > 0L) {
      stop(sprintf(
        "'by' variable(s) not found in data: %s",
        paste(missing_by, collapse = ", ")
      ))
    }
    if (!is.null(force_in)) {
      overlap <- intersect(force_in, by)
      if (length(overlap) > 0L) {
        stop(sprintf(
          "'force_in' cannot include grouping variable(s) named in 'by': %s",
          paste(overlap, collapse = ", ")
        ))
      }
    }
  }

  # Check group_q
  if (!is.numeric(group_q) || length(group_q) != 1L) {
    stop("'group_q' must be a single numeric value")
  }
  if (is.na(group_q) || group_q <= 0 || group_q > 1) {
    stop("'group_q' must be in the interval (0, 1]")
  }

  # Check max_exact_p
  if (!is.numeric(max_exact_p) || length(max_exact_p) != 1L) {
    stop("'max_exact_p' must be a single numeric value")
  }
  if (is.na(max_exact_p) || max_exact_p < 1) {
    stop("'max_exact_p' must be >= 1")
  }

  force_in
}

#' Detect and classify corrPrune()'s predictor types: drops any grouping
#' columns named in `by` from the predictor set, auto-converts remaining
#' columns to the canonical types the shared association machinery expects
#' (shared with assocSelect() via .auto_convert_types()), and rejects any
#' column whose resulting type is not numeric/ordered/factor. Returns a list
#' with the (possibly column-reduced, type-converted) data and a named
#' character vector of per-column types.
#' @noRd
.detect_corrPrune_types <- function(data, by) {
  predictor_cols <- setdiff(names(data), by)
  if (length(predictor_cols) == 0L) {
    stop("'by' names every column in 'data'; no predictor columns remain to prune.")
  }
  data <- data[, predictor_cols, drop = FALSE]

  # Auto-convert and classify variable types (shared with assocSelect())
  data <- .auto_convert_types(data)

  types <- vapply(data, function(x) class(x)[1], character(1))
  valid_types <- c("numeric", "ordered", "factor")
  bad <- names(data)[!types %in% valid_types]
  if (length(bad)) {
    stop(sprintf(
      "Unsupported column types in: %s",
      paste(bad, collapse = ", ")
    ))
  }

  list(data = data, types = types)
}

#' Drops columns of `data` that are constant across every complete-case row
#' (before any group split). A column constant across every complete-case row
#' can never violate the threshold, so it would otherwise ride into the final
#' subset without contributing any information. Detected on the whole-data
#' complete cases, ignoring `by`, since a globally-constant column is
#' necessarily constant within every group too -- this is distinct from a
#' column constant only *within one group*, which
#' .numeric_assoc_matrix()/.mixed_type_assoc_matrix()'s own zero-out logic
#' still handles further downstream. Skipped when there are fewer than 2
#' complete rows so this doesn't mask the "too few rows" error
#' .compute_single_assoc_matrix() raises later. Errors if a `force_in`
#' variable is among the dropped columns.
#' @noRd
.drop_constant_corrPrune_columns <- function(data, types, force_in) {
  complete_data <- data[complete.cases(data), , drop = FALSE]
  if (nrow(complete_data) < 2L) {
    return(list(data = data, types = types))
  }

  kept <- names(.drop_constant_columns(complete_data))
  dropped_const <- setdiff(names(data), kept)
  if (length(dropped_const) == 0L) {
    return(list(data = data, types = types))
  }

  bad_force_in <- intersect(force_in, dropped_const)
  if (length(bad_force_in) > 0L) {
    stop(sprintf(
      "'force_in' variable(s) were excluded for being constant: %s",
      paste(bad_force_in, collapse = ", ")
    ))
  }

  data  <- data[, kept, drop = FALSE]
  types <- types[kept]
  if (ncol(data) == 0L) {
    stop("No predictors remain after excluding constant columns.")
  }

  list(data = data, types = types)
}

#' Resolve corrPrune()'s numeric-numeric association measure. `measure`
#' customizes the numeric-numeric sub-measure only; other pair-type
#' combinations always use eta-squared (numeric-categorical) or Cramer's V
#' (categorical-categorical), mirroring assocSelect()'s fixed dispatch table.
#' This applies whether or not the data is all-numeric, so a mixed-type call
#' can still request e.g. measure = "kendall" for its numeric-numeric pairs.
#' @noRd
.resolve_corrPrune_measure <- function(measure) {
  numeric_measure_choices <- c("pearson", "spearman", "kendall", "bicor", "distance", "maximal")
  if (measure == "auto") {
    return("pearson")
  }
  if (measure %in% numeric_measure_choices) {
    return(measure)
  }
  stop(sprintf(
    "'measure' must be one of: %s. It customizes numeric-numeric associations only; other pair types always use eta-squared or Cramer's V.",
    paste(c("auto", numeric_measure_choices), collapse = ", ")
  ))
}

#' Association matrix for corrPrune()'s ungrouped path: computes the matrix
#' once over all rows via .compute_single_assoc_matrix() and reports the
#' complete-case row count used. Shared with .compute_grouped_assoc_matrix()'s
#' single-group fallback, so both paths derive `n_rows_used` the same way.
#' @noRd
.compute_ungrouped_assoc_matrix <- function(data, measure_used, types) {
  computed <- .compute_single_assoc_matrix(data, measure_used, types)
  list(
    mat = computed$mat,
    assoc_methods_used = computed$assoc_methods_used,
    n_rows_used = nrow(data[complete.cases(data), ])
  )
}

#' Grouped, quantile-aggregated association matrix for corrPrune()'s `by`
#' path: splits `data` by the interaction of the `by` columns (read from
#' `data_orig` since `by` columns are excluded from the predictor set),
#' computes an association matrix within each group with enough complete
#' rows, verifies no pair is left genuinely undefined in a group that was
#' actually checked, then aggregates across groups per-cell using the
#' `group_q` quantile. Falls back to .compute_ungrouped_assoc_matrix() when
#' fewer than two groups are present. Returns a list with the aggregated
#' matrix ($mat), the per-pair-type methods used ($assoc_methods_used, which
#' is structural and therefore identical across groups), and the number of
#' rows from groups that actually contributed ($n_rows_used).
#' @noRd
.compute_grouped_assoc_matrix <- function(data, data_orig, by, types, measure_used, group_q) {
  group_var <- interaction(data_orig[, by, drop = FALSE], drop = TRUE)

  # Rows with a missing grouping value produce NA here (interaction() has
  # no "NA" level of its own) and would otherwise be silently excluded from
  # every group with no warning at all -- unlike every other NA-driven row
  # drop in this package.
  n_na_by <- sum(is.na(group_var))
  if (n_na_by > 0) {
    warning(sprintf(
      "%d row%s with missing values in the grouping variable(s) ('%s') were excluded from every group.",
      n_na_by, if (n_na_by == 1) "" else "s", paste(by, collapse = "', '")
    ))
  }

  group_levels <- levels(group_var)
  n_groups <- length(group_levels)

  if (n_groups < 2) {
    warning("Only one group found; proceeding without grouping.")
    return(.compute_ungrouped_assoc_matrix(data, measure_used, types))
  }

  # Compute association matrix for each group
  p <- ncol(data)
  assoc_arrays <- array(NA_real_, dim = c(p, p, n_groups))
  dimnames(assoc_arrays) <- list(names(data), names(data), group_levels)

  rows_per_group <- integer(n_groups)
  assoc_methods_used <- list()
  for (g in seq_along(group_levels)) {
    grp_idx <- which(group_var == group_levels[g])
    grp_data <- data[grp_idx, , drop = FALSE]

    # Skip groups with insufficient data
    grp_complete <- grp_data[complete.cases(grp_data), , drop = FALSE]
    rows_per_group[g] <- nrow(grp_complete)

    if (nrow(grp_complete) < 2) {
      warning(sprintf("Group '%s' has fewer than 2 complete rows; skipping.", group_levels[g]))
      next
    }

    # Compute association matrix for this group. Only the informative
    # "Removed N row(s)..." warning from .compute_single_assoc_matrix()
    # itself is allowed through; anything else (e.g. a stats::cor()
    # zero-variance warning on a small group) is muffled, same as before.
    # assoc_methods_used is structural (types and measure_used are the
    # same across every group), so re-assigning it on each iteration is
    # harmless -- every computed group returns the same value.
    withCallingHandlers({
      computed_g <- .compute_single_assoc_matrix(grp_data, measure_used, types)
      assoc_arrays[, , g] <- computed_g$mat
      assoc_methods_used <- computed_g$assoc_methods_used
    }, warning = function(w) {
      if (!grepl("^Removed \\d+ row", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    })
  }

  # A group counts as "computed" only if it had enough complete rows to
  # actually be run through .compute_single_assoc_matrix() above (as
  # opposed to a skipped group, already warned about individually).
  group_computed <- rows_per_group >= 2

  # A cell is genuinely undefined if some *computed* group still
  # produced NA for that specific pair -- e.g. a factor level that
  # happens to be unused within that one group, or a degenerate
  # contingency table -- as distinct from a skipped group's NA, which is
  # a deliberate, already-warned exclusion. Silently dropping the former
  # from the group_q quantile (as the aggregation below does for the
  # latter) would let group_q = 1's "holds in every group" guarantee
  # pass without that group's association ever actually being checked.
  undefined_cells <- apply(assoc_arrays, c(1, 2), function(vals) {
    any(is.na(vals) & group_computed)
  })
  diag(undefined_cells) <- FALSE
  if (any(undefined_cells)) {
    bad_idx <- which(undefined_cells & upper.tri(undefined_cells), arr.ind = TRUE)
    bad_pairs <- sprintf("'%s' and '%s'", names(data)[bad_idx[, 1]], names(data)[bad_idx[, 2]])
    stop(sprintf(
      "Association is undefined for %s in at least one group that had enough data to be included (e.g. an unused factor level, or a degenerate contingency table within that group). Excluding it from the group_q aggregate would silently skip verifying that group; consider excluding the offending variable, choosing a coarser grouping, or filtering the degenerate group explicitly.",
      paste(bad_pairs, collapse = ", ")
    ))
  }

  # Aggregate across groups using group_q quantile. Only computed groups
  # contribute; skipped groups (rows_per_group < 2, already warned above)
  # are excluded here as a deliberate, already-communicated omission.
  A_eff <- apply(assoc_arrays, c(1, 2), function(vals) {
    vals <- vals[group_computed & !is.na(vals)]
    if (length(vals) == 0) return(NA_real_)
    quantile(vals, probs = group_q, na.rm = TRUE)
  })
  colnames(A_eff) <- rownames(A_eff) <- names(data)
  diag(A_eff) <- 1

  # Only rows from groups that actually contributed to A_eff count
  # towards n_rows_used; a skipped group's rows never fed the
  # association matrix at all.
  n_rows_used <- sum(rows_per_group[group_computed])

  n_contributing <- sum(group_computed)
  if (n_contributing < n_groups) {
    warning(sprintf(
      "Only %d of %d groups had enough complete rows to contribute to the group_q aggregate; the rest were skipped.",
      n_contributing, n_groups
    ))
  }

  list(mat = A_eff, assoc_methods_used = assoc_methods_used, n_rows_used = n_rows_used)
}

#' Stop if corrPrune()'s effective association matrix contains any undefined
#' (NA) cell. An NA in `A_eff` means the true association for that pair is
#' unknown (e.g. Cramer's V undefined for a degenerate contingency table),
#' not that the pair is known to be compatible. Surfacing this explicitly
#' keeps exact and greedy modes consistent: without this check, exact mode
#' would hit a generic "mat must not contain NA" error deep inside
#' MatSelect(), and greedy mode would silently pass NaN through the C++
#' backend as if it were a non-violation.
#' @noRd
.reject_undefined_associations <- function(A_eff) {
  if (!anyNA(A_eff)) return(invisible(NULL))

  na_idx <- which(is.na(A_eff) & upper.tri(A_eff), arr.ind = TRUE)
  bad_pairs <- sprintf("'%s' and '%s'",
                       colnames(A_eff)[na_idx[, 1]], colnames(A_eff)[na_idx[, 2]])
  stop(sprintf(
    "Association matrix contains undefined (NA) values for: %s. This may be caused by sparse combinations or unused factor levels.",
    paste(bad_pairs, collapse = ", ")
  ))
}

#' Check that corrPrune()'s `force_in` variables satisfy the threshold
#' constraint among themselves, erroring out with a concrete offending pair
#' if not.
#' @noRd
.check_corrPrune_force_in_feasibility <- function(A_eff, force_in, threshold) {
  if (is.null(force_in)) return(invisible(NULL))

  # Get indices of force_in variables
  force_in_idx <- match(force_in, colnames(A_eff))
  if (length(force_in_idx) <= 1) return(invisible(NULL))

  # Extract submatrix for force_in variables
  M <- A_eff[force_in_idx, force_in_idx]

  # Check upper triangle (excluding diagonal). .reject_undefined_associations()
  # already stops on any NA anywhere in A_eff before this is called, so M can
  # never contain NA here -- only the magnitude check applies.
  Mtri <- M[upper.tri(M)]
  violations <- which(abs(Mtri) > threshold, arr.ind = FALSE)
  if (length(violations) == 0) return(invisible(NULL))

  # Find which pairs violate
  upper_tri_idx <- which(upper.tri(M), arr.ind = TRUE)
  bad_pairs <- upper_tri_idx[violations, , drop = FALSE]
  var1 <- force_in[bad_pairs[1, 1]]
  var2 <- force_in[bad_pairs[1, 2]]
  bad_val <- abs(M[bad_pairs[1, 1], bad_pairs[1, 2]])

  stop(sprintf(
    "Variables in 'force_in' violate the threshold constraint. Example: '%s' and '%s' have association %.3f > %.3f",
    var1, var2, bad_val, threshold
  ))
}

#' Resolve corrPrune()'s exact-vs-greedy mode. Exact mode routes through
#' MatSelect(), which requires >= 2 columns and threshold > 0 -- stricter
#' than corrPrune()'s own contract (>= 1 column, threshold >= 0; both cap at
#' 1). `mode = "auto"` degrades to greedy for inputs exact mode cannot
#' service, rather than erroring on documented-valid corrPrune() input.
#' @noRd
.resolve_corrPrune_mode <- function(mode, p, max_exact_p, threshold) {
  if (mode != "auto") return(mode)
  if (p <= max_exact_p && p >= 2 && threshold > 0) "exact" else "greedy"
}

#' Run corrPrune()'s exact-mode backend: enumerate all maximal subsets via
#' MatSelect() and select one via .select_exact_subset()'s deterministic
#' tie-break. MatSelect() requires >= 2 columns and threshold > 0, stricter
#' than corrPrune()'s own contract on the lower bound (both cap at 1) --
#' surfaces a corrPrune-specific error for an explicit mode = "exact" request
#' on input it cannot service, rather than letting MatSelect()'s internal
#' message leak through.
#' @noRd
.run_corrPrune_exact <- function(A_eff, threshold, force_in) {
  p <- ncol(A_eff)
  if (p < 2) {
    stop("mode = 'exact' requires at least two variables in 'data'. ",
         "Use mode = 'greedy' (or the default mode = 'auto') for single-variable input.")
  }
  if (threshold <= 0) {
    stop("mode = 'exact' requires 'threshold' > 0. ",
         "Use mode = 'greedy' (or the default mode = 'auto') for threshold = 0.")
  }

  combo_result <- MatSelect(
    mat = A_eff,
    threshold = threshold,
    method = NULL,  # Let MatSelect choose based on force_in
    force_in = force_in
  )

  .select_exact_subset(combo_result)
}

#' Deterministic tie-break among corrPrune()'s exact-mode maximal subsets:
#' (1) largest subset size, (2) if tied, smallest average correlation, (3) if
#' still tied, lexicographically first. MatSelect() always returns at least
#' one maximal subset for a matrix with >= 2 columns -- including size-1
#' subsets when no pair of variables is mutually compatible under the
#' threshold -- so `combo_result@subset_list` is never empty here.
#' @noRd
.select_exact_subset <- function(combo_result) {
  subset_sizes <- vapply(combo_result@subset_list, length, integer(1))
  max_size <- max(subset_sizes)
  largest_subsets_idx <- which(subset_sizes == max_size)

  if (length(largest_subsets_idx) == 1) {
    selected_idx <- largest_subsets_idx[1]
  } else {
    # Multiple subsets of same max size: break tie by avg correlation
    avg_corrs <- combo_result@avg_corr[largest_subsets_idx]
    min_avg <- min(avg_corrs)
    best_avg_idx <- which(avg_corrs == min_avg)
    candidates_idx <- largest_subsets_idx[best_avg_idx]

    if (length(candidates_idx) == 1) {
      selected_idx <- candidates_idx[1]
    } else {
      # Still tied: use lexicographic order
      candidates_subsets <- combo_result@subset_list[candidates_idx]
      # Sort each subset and concatenate for comparison
      sorted_strings <- vapply(candidates_subsets, function(s) {
        paste(sort(s), collapse = ",")
      }, character(1))
      lex_order <- order(sorted_strings)
      selected_idx <- candidates_idx[lex_order[1]]
    }
  }

  combo_result@subset_list[[selected_idx]]
}

#' Run corrPrune()'s greedy-mode backend: the fast C++ greedy pruner, given
#' 0-based `force_in` indices as it expects.
#' @noRd
.run_corrPrune_greedy <- function(A_eff, threshold, force_in) {
  force_in_cpp <- if (!is.null(force_in)) {
    match(force_in, colnames(A_eff)) - 1L
  } else {
    NULL
  }

  keep_idx <- greedyPruneBackend(
    assoc_matrix = A_eff,
    threshold = threshold,
    force_in = force_in_cpp
  )

  colnames(A_eff)[keep_idx]
}
