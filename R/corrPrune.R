#' Association-Based Predictor Pruning
#'
#' `corrPrune()` performs model-free variable subset selection by iteratively
#' removing predictors until all pairwise associations fall below a specified
#' threshold. It returns a single pruned data frame with predictors that satisfy
#' the association constraint.
#'
#' @param data A data.frame containing candidate predictors.
#' @param threshold Numeric scalar. Maximum allowed pairwise association
#'   (default: 0.7). Must be non-negative.
#' @param measure Character string specifying the association measure to use.
#'   Options: `"auto"` (default), `"pearson"`, `"spearman"`, `"kendall"`,
#'   `"cramersv"`, `"eta"`, etc. When `"auto"`, Pearson correlation is used
#'   for all-numeric data, and appropriate measures are selected for mixed-type
#'   data.
#' @param mode Character string specifying the search algorithm. Options:
#'   - `"auto"` (default): uses exact search if number of predictors <= `max_exact_p`,
#'     otherwise uses greedy search
#'   - `"exact"`: exhaustive search for maximal subsets (may be slow for large p)
#'   - `"greedy"`: fast approximate search using iterative removal
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
#' @return A data.frame containing the pruned subset of predictors. The result
#'   has the following attributes:
#'   \describe{
#'     \item{selected_vars}{Character vector of retained variable names}
#'     \item{removed_vars}{Character vector of removed variable names}
#'     \item{mode}{Character string indicating which mode was used ("exact" or "greedy")}
#'     \item{measure}{Character string indicating which association measure was used}
#'     \item{threshold}{The threshold value used}
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
#' returns the largest one (with deterministic tie-breaking). Greedy mode is
#' faster but approximate, using a deterministic removal strategy based on
#' association scores.
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
#' @importFrom stats cor complete.cases
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
  if (is.na(threshold) || threshold < 0) {
    stop("'threshold' must be non-negative and non-missing")
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

  # Check force_in
  if (!is.null(force_in)) {
    if (!is.character(force_in)) {
      stop("'force_in' must be a character vector of variable names")
    }
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

  # Check if all numeric (for measure auto-resolution)
  all_numeric <- all(types == "numeric")

  # ===========================================================================
  # Step 3 — Resolve association measure
  # ===========================================================================

  # Auto-resolve measure
  if (measure == "auto") {
    measure_used <- if (all_numeric) "pearson" else "cramersv"
  } else {
    measure_used <- measure
  }

  # ===========================================================================
  # Step 4 — Compute effective association matrix
  # ===========================================================================

  # Helper function to compute association matrix for a single data frame
  .compute_single_assoc_matrix <- function(df_input, meas, var_types) {
    p <- ncol(df_input)
    mat <- diag(1, p)
    colnames(mat) <- rownames(mat) <- names(df_input)

    # If all numeric, use correlation-based approach
    if (all(var_types == "numeric")) {
      # Handle missing values
      dropped <- sum(!complete.cases(df_input))
      if (dropped > 0) {
        df_clean <- df_input[complete.cases(df_input), ]
        if (dropped == nrow(df_input)) {
          stop("All rows contain missing values")
        }
        warning(sprintf(
          "Removed %d row%s with missing values.",
          dropped, if (dropped == 1) "" else "s"
        ))
      } else {
        df_clean <- df_input
      }

      # Compute correlation matrix based on measure
      if (meas %in% c("pearson", "spearman", "kendall")) {
        mat <- abs(cor(df_clean, method = meas))
      } else {
        stop(sprintf("Measure '%s' not yet implemented for numeric data", meas))
      }
    } else {
      # Mixed-type: compute pairwise associations using appropriate measures

      # Compute pairwise associations
      for (i in seq_len(p - 1)) {
        for (j in (i + 1):p) {
          xi <- df_input[[i]]
          xj <- df_input[[j]]
          ti <- var_types[i]
          tj <- var_types[j]

          # Determine association measure based on types
          if (ti == "numeric" && tj == "numeric") {
            # Use the specified numeric measure
            assoc_val <- abs(cor(xi, xj, method = meas, use = "complete.obs"))
          } else if ((ti == "numeric" && tj == "ordered") || (ti == "ordered" && tj == "numeric")) {
            # Numeric-Ordered: use Spearman
            assoc_val <- abs(cor(xi, xj, method = "spearman", use = "complete.obs"))
          } else if ((ti == "numeric" && tj == "factor") || (ti == "factor" && tj == "numeric")) {
            # Numeric-Factor: use eta-squared
            cat_var <- if (ti == "factor") xi else xj
            num_var <- if (ti == "numeric") xi else xj
            if (length(unique(cat_var)) < 2) {
              assoc_val <- 0
            } else {
              ss_tot <- sum((num_var - mean(num_var, na.rm = TRUE))^2, na.rm = TRUE)
              if (ss_tot == 0) {
                assoc_val <- 0
              } else {
                ss_bet <- sum(tapply(num_var, cat_var, function(z) {
                  length(z) * (mean(z, na.rm = TRUE) - mean(num_var, na.rm = TRUE))^2
                }), na.rm = TRUE)
                assoc_val <- sqrt(ss_bet / ss_tot)
              }
            }
          } else if (ti == "ordered" && tj == "ordered") {
            # Ordered-Ordered: use Spearman
            assoc_val <- abs(cor(as.numeric(xi), as.numeric(xj), method = "spearman", use = "complete.obs"))
          } else {
            # Factor-Factor or Factor-Ordered: use Cramer's V
            tbl <- table(xi, xj)
            if (min(dim(tbl)) < 2 || any(rowSums(tbl) == 0) || any(colSums(tbl) == 0)) {
              assoc_val <- NA_real_
            } else {
              suppressWarnings({
                chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE)$statistic)
              })
              if (length(chi2) == 0 || is.na(chi2)) {
                assoc_val <- NA_real_
              } else {
                assoc_val <- sqrt(chi2 / (sum(tbl) * (min(dim(tbl)) - 1)))
              }
            }
          }

          mat[i, j] <- mat[j, i] <- assoc_val
        }
      }
    }

    mat
  }

  # Compute effective association matrix
  if (is.null(by)) {
    # Case A: No grouping
    A_eff <- .compute_single_assoc_matrix(data, measure_used, types)
    n_rows_used <- nrow(data[complete.cases(data), ])
  } else {
    # Case B: Grouped association
    stop("Grouped pruning (by/group_q) not yet implemented")
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

      # Check upper triangle (excluding diagonal)
      violations <- which(M[upper.tri(M)] > threshold, arr.ind = FALSE)

      if (length(violations) > 0) {
        # Find which pairs violate
        upper_tri_idx <- which(upper.tri(M), arr.ind = TRUE)
        bad_pairs <- upper_tri_idx[violations, , drop = FALSE]
        var1 <- force_in[bad_pairs[1, 1]]
        var2 <- force_in[bad_pairs[1, 2]]
        bad_val <- M[bad_pairs[1, 1], bad_pairs[1, 2]]

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
    mode_used <- if (p <= max_exact_p) "exact" else "greedy"
  } else {
    mode_used <- mode
  }

  # ===========================================================================
  # Step 7 — Exact or Greedy backend
  # ===========================================================================

  if (mode_used == "exact") {
    # Step 7A — Exact mode: use MatSelect to get all maximal subsets
    combo_result <- MatSelect(
      mat = A_eff,
      threshold = threshold,
      method = NULL,  # Let MatSelect choose based on force_in
      force_in = force_in
    )

    # Extract all subsets that satisfy the constraint
    if (length(combo_result@subset_list) == 0) {
      stop("No valid subsets found that satisfy the threshold constraint")
    }

    # Choose one subset using deterministic tie-breaking:
    # 1. Largest subset size
    # 2. If tied: smallest average correlation
    # 3. If tied: lexicographically first

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

  # Return the pruned data with selected variables
  data_pruned <- data[, selected_vars, drop = FALSE]

  # Compute removed variables
  all_vars <- colnames(data)
  removed_vars <- setdiff(all_vars, selected_vars)

  # Add attributes for metadata
  attr(data_pruned, "selected_vars") <- selected_vars
  attr(data_pruned, "removed_vars") <- removed_vars
  attr(data_pruned, "mode") <- mode_used
  attr(data_pruned, "measure") <- measure_used
  attr(data_pruned, "threshold") <- threshold
  attr(data_pruned, "n_vars_original") <- ncol(data)
  attr(data_pruned, "n_vars_selected") <- length(selected_vars)

  return(data_pruned)
}
