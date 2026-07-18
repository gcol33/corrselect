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
#' 2. **Association measurement**: Computes appropriate pairwise associations
#' 3. **Grouping (optional)**: If `by` is specified, computes associations within
#'    each group and aggregates using the specified quantile
#' 4. **Feasibility check**: Verifies that `force_in` variables satisfy the
#'    threshold constraint
#' 5. **Subset selection**: Uses either exact or greedy search to find a valid subset
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

  # ===========================================================================
  # Step 1 — Validate inputs
  # ===========================================================================

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

  # ===========================================================================
  # Step 2 — Variable type detection
  # ===========================================================================

  # Store original data for final subsetting
  data_orig <- data

  # Remove grouping variables from predictor set if present
  predictor_cols <- setdiff(names(data), by)
  if (length(predictor_cols) == 0L) {
    stop("'by' names every column in 'data'; no predictor columns remain to prune.")
  }
  data <- data[, predictor_cols, drop = FALSE]

  # Auto-convert and classify variable types (following assocSelect pattern)
  data[] <- lapply(data, function(col) {
    if (is.character(col)) {
      factor(col)
    } else if (is.logical(col)) {
      factor(col)
    } else if (is.factor(col)) {
      droplevels(col)
    } else if (is.integer(col)) {
      as.numeric(col)
    } else {
      col
    }
  })

  # Classify each column
  types <- vapply(data, function(x) class(x)[1], character(1))
  valid_types <- c("numeric", "ordered", "factor")
  bad <- names(data)[!types %in% valid_types]
  if (length(bad)) {
    stop(sprintf(
      "Unsupported column types in: %s",
      paste(bad, collapse = ", ")
    ))
  }

  # ===========================================================================
  # Step 3 — Resolve association measure
  # ===========================================================================

  # `measure` customizes the numeric-numeric sub-measure only; other pair-type
  # combinations always use eta-squared (numeric-categorical) or Cramer's V
  # (categorical-categorical), mirroring assocSelect()'s fixed dispatch table.
  # This applies whether or not the data is all-numeric, so a mixed-type call
  # can still request e.g. measure = "kendall" for its numeric-numeric pairs.
  numeric_measure_choices <- c("pearson", "spearman", "kendall", "bicor", "distance", "maximal")
  if (measure == "auto") {
    measure_used <- "pearson"
  } else if (measure %in% numeric_measure_choices) {
    measure_used <- measure
  } else {
    stop(sprintf(
      "'measure' must be one of: %s. It customizes numeric-numeric associations only; other pair types always use eta-squared or Cramer's V.",
      paste(c("auto", numeric_measure_choices), collapse = ", ")
    ))
  }

  # Real per-pair-type association-method metadata (mirrors assocSelect()'s
  # `assoc_methods_used` attribute). Determined purely by the static variable
  # types and the resolved numeric-numeric measure, so it is valid whether or
  # not `by`-grouping is used. Reuses .full_assoc_method_map() -- the same
  # type-pair -> method table .compute_single_assoc_matrix() dispatches
  # through below -- rather than re-deriving an independent copy that could
  # drift from it (see #59).
  assoc_methods_used <- list()
  if (length(types) >= 2) {
    if (all(types == "numeric")) {
      assoc_methods_used <- list(numeric_numeric = measure_used)
    } else {
      full_assoc_methods <- .full_assoc_method_map(measure_used)
      for (.i in seq_len(length(types) - 1)) {
        for (.j in (.i + 1):length(types)) {
          .key <- paste(types[.i], types[.j], sep = "_")
          if (is.null(assoc_methods_used[[.key]])) {
            assoc_methods_used[[.key]] <- full_assoc_methods[[.key]]
          }
        }
      }
    }
  }

  # ===========================================================================
  # Step 4 — Compute effective association matrix
  # ===========================================================================

  # Helper function to compute association matrix for a single data frame
  .compute_single_assoc_matrix <- function(df_input, meas, var_types) {
    # Handle missing values (shared by both branches below): listwise
    # deletion up front, so every pair-type computation sees the same
    # complete-case data rather than each pair applying its own ad hoc NA
    # policy.
    dropped <- sum(!complete.cases(df_input))
    if (dropped > 0) {
      df_clean <- df_input[complete.cases(df_input), ]
      if (dropped == nrow(df_input)) {
        stop("All rows contain missing values")
      }
      warning(sprintf(
        "Removed %d row%s with missing values when computing associations (rows are not removed from the returned data).",
        dropped, if (dropped == 1) "" else "s"
      ))
    } else {
      df_clean <- df_input
    }

    # A single remaining row makes sd()/cor() degenerate (NA, not 0/FALSE),
    # which would otherwise surface here as an opaque "missing value where
    # TRUE/FALSE needed" from .numeric_assoc_matrix()'s constant-column check
    # rather than a clear, corrPrune-specific message (matching
    # corrSelect()/assocSelect()'s equivalent guard).
    if (nrow(df_clean) < 2) {
      stop("Fewer than two complete-case rows remain after removing missing values: ",
           "cannot compute associations.")
    }

    # If all numeric, use the shared vectorized correlation-based builder
    # (constant columns get association 0, not NA -- see .numeric_assoc_matrix()).
    if (all(var_types == "numeric")) {
      if (!meas %in% c("pearson", "spearman", "kendall", "bicor", "distance", "maximal")) {
        stop(sprintf("Measure '%s' is not supported. Use one of: pearson, spearman, kendall, bicor, distance, maximal", meas))
      }
      .numeric_assoc_matrix(df_clean, meas)
    } else {
      # Mixed-type: shared pairwise builder (also used by assocSelect()).
      # corrPrune() only exposes a configurable measure for numeric-numeric
      # pairs; every other pair-type combination uses the fixed dispatch
      # documented in ?corrPrune (spearman / eta / cramersv).
      full_assoc_methods <- .full_assoc_method_map(meas)
      .mixed_type_assoc_matrix(df_clean, var_types, full_assoc_methods)$mat
    }
  }

  # Compute effective association matrix
  if (is.null(by)) {
    # Case A: No grouping
    A_eff <- .compute_single_assoc_matrix(data, measure_used, types)
    n_rows_used <- nrow(data[complete.cases(data), ])
  } else {
    # Case B: Grouped association
    # Split data by grouping variable(s)
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
      A_eff <- .compute_single_assoc_matrix(data, measure_used, types)
      n_rows_used <- nrow(data[complete.cases(data), ])
    } else {
      # Compute association matrix for each group
      p <- ncol(data)
      assoc_arrays <- array(NA_real_, dim = c(p, p, n_groups))
      dimnames(assoc_arrays) <- list(names(data), names(data), group_levels)

      rows_per_group <- integer(n_groups)
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
        withCallingHandlers({
          assoc_arrays[, , g] <- .compute_single_assoc_matrix(grp_data, measure_used, types)
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
    }
  }

  # ===========================================================================
  # Step 4b — Reject genuinely undefined associations
  # ===========================================================================
  # An NA in A_eff means the true association for that pair is unknown (e.g.
  # Cramer's V undefined for a degenerate contingency table), not that the
  # pair is known to be compatible. Surfacing this explicitly here keeps exact
  # and greedy modes consistent: without this check, exact mode would hit a
  # generic "mat must not contain NA" error deep inside MatSelect(), and
  # greedy mode would silently pass NaN through the C++ backend as if it were
  # a non-violation.
  if (anyNA(A_eff)) {
    na_idx <- which(is.na(A_eff) & upper.tri(A_eff), arr.ind = TRUE)
    bad_pairs <- sprintf("'%s' and '%s'",
                         colnames(A_eff)[na_idx[, 1]], colnames(A_eff)[na_idx[, 2]])
    stop(sprintf(
      "Association matrix contains undefined (NA) values for: %s. This may be caused by sparse combinations or unused factor levels.",
      paste(bad_pairs, collapse = ", ")
    ))
  }

  # ===========================================================================
  # Step 5 — Feasibility check for force_in
  # ===========================================================================

  if (!is.null(force_in)) {
    # Get indices of force_in variables
    force_in_idx <- match(force_in, colnames(A_eff))

    if (length(force_in_idx) > 1) {
      # Extract submatrix for force_in variables
      M <- A_eff[force_in_idx, force_in_idx]

      # Check upper triangle (excluding diagonal). Step 4b above already
      # stop()s on any NA anywhere in A_eff, so M can never contain NA here --
      # only the magnitude check applies.
      Mtri <- M[upper.tri(M)]
      violations <- which(abs(Mtri) > threshold, arr.ind = FALSE)

      if (length(violations) > 0) {
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
    }
  }

  # ===========================================================================
  # Step 6 — Mode resolution
  # ===========================================================================

  p <- ncol(data)
  if (mode == "auto") {
    # Exact mode routes through MatSelect(), which requires >= 2 columns
    # and threshold > 0 -- stricter than corrPrune()'s own contract (>= 1
    # column, threshold >= 0; both cap at 1). "auto" degrades to greedy for
    # inputs exact mode cannot service, rather than erroring on
    # documented-valid corrPrune() input.
    mode_used <- if (p <= max_exact_p && p >= 2 && threshold > 0) "exact" else "greedy"
  } else {
    mode_used <- mode
  }

  # ===========================================================================
  # Step 7 — Exact or Greedy backend
  # ===========================================================================

  if (mode_used == "exact") {
    # Step 7A — Exact mode: use MatSelect to get all maximal subsets.
    # MatSelect() requires >= 2 columns and threshold > 0, stricter than
    # corrPrune()'s own contract on the lower bound (both cap at 1) --
    # surface a corrPrune-specific error for an explicit mode = "exact"
    # request on input it cannot service, rather than letting MatSelect()'s
    # internal message leak through.
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

    # Choose one subset using deterministic tie-breaking:
    # 1. Largest subset size
    # 2. If tied: smallest average correlation
    # 3. If tied: lexicographically first
    #
    # MatSelect() always returns at least one maximal subset for a matrix
    # with >= 2 columns -- including size-1 subsets when no pair of
    # variables is mutually compatible under the threshold -- so
    # combo_result@subset_list is never empty here.

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

    selected_vars <- combo_result@subset_list[[selected_idx]]

  } else {
    # Step 7B — Greedy mode: use fast C++ greedy backend

    # Convert force_in to 0-based indices for C++
    force_in_cpp <- if (!is.null(force_in)) {
      match(force_in, colnames(A_eff)) - 1L
    } else {
      NULL
    }

    # Call C++ greedy backend
    keep_idx <- greedyPruneBackend(
      assoc_matrix = A_eff,
      threshold = threshold,
      force_in = force_in_cpp
    )

    # Convert 1-based indices back to variable names
    selected_vars <- colnames(A_eff)[keep_idx]
  }

  # ===========================================================================
  # Step 8 — Final output
  # ===========================================================================

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
