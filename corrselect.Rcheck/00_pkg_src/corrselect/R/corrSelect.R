#' Select Variable Subsets with Low Correlation (Data Frame Interface)
#'
#' Identifies combinations of numeric variables in a data frame such that all pairwise absolute correlations
#' fall below a specified threshold. This function is a wrapper around \code{\link{MatSelect}()}
#' and accepts data frames, tibbles, or data tables with automatic preprocessing.
#'
#' Only numeric columns are used for correlation analysis. Non‐numeric columns (factors, characters,
#' logicals, etc.) are ignored, and their names and types are printed to inform the user. These can be
#' optionally reattached later using \code{\link{corrSubset}()} with \code{keepExtra = TRUE}.
#'
#' Rows with missing values are removed before computing correlations. A warning is issued if any rows are dropped.
#'
#' @param df A data frame. Only numeric columns are used.
#' @param threshold A numeric value in (0, 1). Maximum allowed absolute correlation. Defaults to 0.7.
#' @param method Character. Selection algorithm to use. One of \code{"els"} or
#'        \code{"bron-kerbosch"}. If not specified, the function chooses automatically:
#'        \code{"els"} when \code{force_in} is provided, otherwise \code{"bron-kerbosch"}.
#' @param force_in Optional character vector or numeric indices of columns to force into all subsets.
#' @param cor_method Character string indicating which correlation method to use.
#'        One of \code{"pearson"} (default), \code{"spearman"}, \code{"kendall"},
#'        \code{"bicor"}, \code{"distance"}, or \code{"maximal"}.
#' @param ... Additional arguments passed to \code{\link{MatSelect}()}, e.g., \code{use_pivot}.
#'
#' @return An object of class \code{\link{CorrCombo}}, containing selected subsets and correlation statistics.
#'
#' @details
#' The \code{cor_method} controls how the correlation matrix is computed:
#' \itemize{
#'   \item \code{"pearson"}: Standard linear correlation.
#'   \item \code{"spearman"}: Rank-based monotonic correlation.
#'   \item \code{"kendall"}: Kendall's tau.
#'   \item \code{"bicor"}: Biweight midcorrelation (WGCNA::bicor).
#'   \item \code{"distance"}: Distance correlation (energy::dcor).
#'   \item \code{"maximal"}: Maximal information coefficient (minerva::mine).
#' }
#'
#' For \code{"bicor"}, \code{"distance"}, and \code{"maximal"}, the corresponding
#' package must be installed.
#'
#' @seealso [assocSelect()], [MatSelect()], [corrSubset()]
#'
#' @examples
#' set.seed(42)
#' n <- 100
#'
#' # Create 20 variables: 5 blocks of correlated variables + some noise
#' block1 <- matrix(rnorm(n * 4), ncol = 4)
#' block2 <- matrix(rnorm(n), ncol = 1)
#' block2 <- matrix(rep(block2, 4), ncol = 4) + matrix(rnorm(n * 4, sd = 0.1), ncol = 4)
#' block3 <- matrix(rnorm(n * 4), ncol = 4)
#' block4 <- matrix(rnorm(n * 4), ncol = 4)
#' block5 <- matrix(rnorm(n * 4), ncol = 4)
#'
#' df <- as.data.frame(cbind(block1, block2, block3, block4, block5))
#' colnames(df) <- paste0("V", 1:20)
#'
#' # Add a non-numeric column to be ignored
#' df$label <- factor(sample(c("A", "B"), n, replace = TRUE))
#'
#' # Basic usage
#' corrSelect(df, threshold = 0.8)
#'
#' # Try Bron–Kerbosch with pivoting
#' corrSelect(df, threshold = 0.6, method = "bron-kerbosch", use_pivot = TRUE)
#'
#' # Force in a specific variable and use Spearman correlation
#' corrSelect(df, threshold = 0.6, force_in = "V10", cor_method = "spearman")
#'
#' @importFrom stats complete.cases cor sd
#' @importFrom methods is
#' @export
corrSelect <- function(df,
                       threshold = 0.7,
                       method = NULL,
                       force_in = NULL,
                       cor_method = c("pearson", "spearman", "kendall", "bicor", "distance", "maximal"),
                       ...) {

  # Normalize cor_method
  cor_method <- match.arg(cor_method)

  df <- as.data.frame(df)
  if (!is.data.frame(df)) stop("`df` must be a data frame.")
  if (ncol(df) < 2) stop("`df` must have at least two columns.")

  # Identify numeric columns
  numeric_cols <- vapply(df, is.numeric, logical(1))
  df_num       <- df[, numeric_cols, drop = FALSE]
  used_names   <- names(df_num)

  # Validate character force_in *before* subsetting
  if (!is.null(force_in) && is.character(force_in)) {
    if (!all(force_in %in% names(df))) {
      missing <- setdiff(force_in, names(df))
      stop("The following `force_in` names are not in the data frame: ",
           paste(missing, collapse = ", "))
    }
  }

  # Remove rows with NA
  n_before <- nrow(df_num)
  df_num   <- df_num[complete.cases(df_num), , drop = FALSE]
  n_after  <- nrow(df_num)
  n_dropped <- if (length(n_before) == 1 && length(n_after) == 1) n_before - n_after else 0

  if (n_dropped > 0) {
    warning(sprintf("Removed %d row%s with missing values before correlation analysis.",
                    n_dropped, if (n_dropped == 1) "" else "s"))
  }

  .reportSkippedVariables(df, used_names)

  if (ncol(df_num) < 2) stop("Less than two numeric columns remain after preprocessing.")

  # Drop constant variables
  is_const <- vapply(df_num, function(x) sd(x) == 0, logical(1))
  if (any(is_const)) {
    const_vars <- names(df_num)[is_const]
    warning("The following numeric columns were constant and excluded: ",
            paste(const_vars, collapse = ", "))
    df_num     <- df_num[, !is_const, drop = FALSE]
    used_names <- used_names[!is_const]
  }
  if (ncol(df_num) < 2) stop("Less than two numeric columns remain after excluding constants.")

  # Build correlation/association matrix
  mat <- switch(
    cor_method,
    pearson = cor(df_num, use = "everything", method = "pearson"),
    spearman = cor(df_num, use = "everything", method = "spearman"),
    kendall = cor(df_num, use = "everything", method = "kendall"),
    bicor = {
      if (!requireNamespace("WGCNA", quietly = TRUE)) stop("Install the 'WGCNA' package for bicor.")
      suppressWarnings(WGCNA::bicor(df_num))
    },
    distance = {
      if (!requireNamespace("energy", quietly = TRUE)) stop("Install the 'energy' package for distance correlation.")
      n <- ncol(df_num)
      mat <- matrix(NA_real_, n, n)
      for (i in seq_len(n)) {
        for (j in seq(i, n)) {
          d <- energy::dcor(df_num[[i]], df_num[[j]])
          mat[i, j] <- d
          mat[j, i] <- d
        }
      }
      colnames(mat) <- rownames(mat) <- colnames(df_num)
      mat
    },
    maximal = {
      if (!requireNamespace("minerva", quietly = TRUE)) stop("Install the 'minerva' package for maximal correlation (MIC).")
      mic <- minerva::mine(df_num)
      mat <- mic$MIC
      colnames(mat) <- rownames(mat) <- colnames(df_num)
      mat
    },
    stop("Unsupported correlation method.")
  )

  if (anyNA(mat) || any(!is.finite(mat))) {
    stop("Correlation matrix contains NA or infinite values. Check your data.")
  }

  # Handle force_in as names or indices
  if (!is.null(force_in)) {
    if (is.character(force_in)) {
      if (!all(force_in %in% colnames(mat))) {
        missing <- setdiff(force_in, colnames(mat))
        stop("The following `force_in` columns were excluded from correlation: ",
             paste(missing, collapse = ", "))
      }
      force_in <- match(force_in, colnames(mat))
    }

    if (!is.numeric(force_in) || any(force_in < 1) || any(force_in > ncol(mat))) {
      stop("`force_in` must be valid 1-based column indices or names.")
    }
  } else {
    force_in <- integer(0)
  }

  # Conditionally set default method
  if (is.null(method)) {
    method <- if (length(force_in) > 0) "els" else "bron-kerbosch"
  } else {
    method <- match.arg(method, choices = c("bron-kerbosch", "els"))
  }

  # Run backend selection
  result <- MatSelect(
    mat       = mat,
    threshold = threshold,
    method    = method,
    force_in  = force_in,
    ...
  )

  if (!is(result, "CorrCombo")) {
    stop("MatSelect() must return an object of class 'CorrCombo'.")
  }

  result@n_rows_used <- as.integer(n_after)
  result@cor_method  <- cor_method

  result
}


.reportSkippedVariables <- function(df, used) {
  skipped <- setdiff(names(df), used)
  if (length(skipped) == 0) return(invisible())
  reasons <- vapply(skipped, function(v) {
    x <- df[[v]]
    if (is.factor(x)) {
      if (is.ordered(x)) "ordered factor (excluded)" else "unordered factor (excluded)"
    } else if (is.character(x)) {
      "character (excluded)"
    } else {
      paste0(class(x)[1], " (excluded)")
    }
  }, character(1))
  message(
    "The following variables were excluded from the correlation analysis:\n",
    paste0("  - ", skipped, ": ", reasons, collapse = "\n")
  )
}
