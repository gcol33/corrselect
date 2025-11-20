#' @useDynLib corrselect, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL
#' Select Variable Subsets with Low Correlation or Association (Matrix Interface)
#'
#' Identifies all maximal subsets of variables from a symmetric matrix (typically a correlation matrix)
#' such that all pairwise absolute values stay below a specified threshold. Implements exact algorithms
#' such as Eppstein–Löffler–Strash (ELS) and Bron–Kerbosch (with or without pivoting).
#'
#' @param mat A numeric, symmetric matrix with 1s on the diagonal (e.g. correlation matrix).
#'        Column names (if present) are used to label output variables.
#' @param threshold A numeric scalar in (0, 1). Maximum allowed absolute pairwise value.
#'        Defaults to \code{0.7}.
#' @param method Character. Selection algorithm to use. One of \code{"els"} or
#'        \code{"bron-kerbosch"}. If not specified, the function chooses automatically:
#'        \code{"els"} when \code{force_in} is provided, otherwise \code{"bron-kerbosch"}.
#' @param force_in Optional integer vector of 1-based column indices to force into every subset.
#' @param ... Additional arguments passed to the backend, e.g., \code{use_pivot} (logical)
#'        for enabling pivoting in Bron–Kerbosch (ignored by ELS).
#'
#' @return An object of class \code{\link{CorrCombo}}, containing all valid subsets and their
#' correlation statistics.
#'
#' @examples
#' set.seed(42)
#' mat <- matrix(rnorm(100), ncol = 10)
#' colnames(mat) <- paste0("V", 1:10)
#' cmat <- cor(mat)
#'
#' # Default method (Bron-Kerbosch)
#' res1 <- MatSelect(cmat, threshold = 0.5)
#'
#' # Bron–Kerbosch without pivot
#' res2 <- MatSelect(cmat, threshold = 0.5, method = "bron-kerbosch", use_pivot = FALSE)
#'
#' # Bron–Kerbosch with pivoting
#' res3 <- MatSelect(cmat, threshold = 0.5, method = "bron-kerbosch", use_pivot = TRUE)
#'
#' # Force variable 1 into every subset (with warning if too correlated)
#' res4 <- MatSelect(cmat, threshold = 0.5, force_in = 1)
#'
#' @importFrom methods new
#' @export
MatSelect <- function(mat,
                      threshold = 0.7,
                      method = NULL,
                      force_in = NULL,
                      ...) {

  # Force-in conversion (names or indices)
  if (!is.null(force_in)) {
    if (is.character(force_in)) {
      if (is.null(colnames(mat))) {
        stop("`mat` has no column names: cannot use character `force_in`.")
      }
      missing_names <- setdiff(force_in, colnames(mat))
      if (length(missing_names)) {
        stop("`force_in` names not found in matrix: ",
             paste(missing_names, collapse = ", "))
      }
      force_in <- match(force_in, colnames(mat))
    }
    
    # Now: must be valid 1-based indices
    if (!is.numeric(force_in) || anyNA(force_in) ||
        any(force_in < 1) || any(force_in > ncol(mat))) {
      stop("`force_in` must be valid 1-based column indices or names.")
    }
  } else {
    force_in <- integer(0)
  }
  

  # Conditionally select default method
  if (is.null(method)) {
    method <- if (length(force_in) > 0) "els" else "bron-kerbosch"
  } else {
    method <- match.arg(method, choices = c("bron-kerbosch", "els"))
  }

  ## ---- Input validation ----
  if (!is.matrix(mat) || !is.numeric(mat)) {
    stop("`mat` must be a numeric matrix.")
  }
  if (nrow(mat) != ncol(mat)) {
    stop("`mat` must be square.")
  }
  if (anyNA(mat)) {
    stop("`mat` must not contain NA.")
  }
  if (!all(abs(diag(mat) - 1) < 1e-8)) {
    stop("Diagonal entries of `mat` must be 1.")
  }
  if (!all(abs(mat - t(mat)) < 1e-8)) {
    stop("`mat` must be symmetric.")
  }
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold)) {
    stop("`threshold` must be a single numeric value.")
  }
  if (threshold <= 0 || threshold > 1) {
    stop("`threshold` must be in the range (0, 1].")
  }
  n <- ncol(mat)

  ## ---- force_in validation ----
  if (!is.null(force_in)) {
    if (!is.numeric(force_in) ||
        any(force_in < 1) ||
        any(force_in > n)) {
      stop("`force_in` must be valid 1-based column indices.")
    }
  }

  ## ---- prepare names ----
  varnames   <- colnames(mat)
  if (is.null(varnames)) varnames <- paste0("V", seq_len(n))
  force_names <- if (!is.null(force_in)) varnames[force_in] else character()

  ## ---- warn if forced_in vars are too correlated internally ----
  if (length(force_names) > 1) {
    submat <- abs(mat[force_in, force_in, drop = FALSE])
    cors   <- submat[upper.tri(submat)]
    if (any(cors > threshold, na.rm = TRUE)) {
      warning(
        "Variables in `force_in` are mutually correlated beyond the threshold. ",
        "They will still be forced into all subsets."
      )
    }
  }

  ## ---- backend options ----
  dots      <- list(...)
  use_pivot <- TRUE
  if ("use_pivot" %in% names(dots)) {
    tmp <- as.logical(dots$use_pivot)
    if (length(tmp) > 0 && !is.na(tmp[1])) use_pivot <- tmp[1]
  }

  ## ---- dispatch to C/C++ backend ----
  raw_out <- findAllMaxSets(
    corMatrix = mat,
    threshold = threshold,
    method    = method,
    force_in  = force_in,
    use_pivot = use_pivot
  )

  ## ---- extract combos & avg_corr ----
  if (is.list(raw_out) &&
      length(raw_out) > 0 &&
      is.list(raw_out[[1]]) &&
      all(c("combo", "avg_corr") %in% names(raw_out[[1]]))) {
    combos <- lapply(raw_out, `[[`, "combo")
    avg    <- vapply(raw_out, `[[`, numeric(1), "avg_corr")
  } else if (is.list(raw_out) && all(vapply(raw_out, is.integer, logical(1)))) {
    combos <- raw_out
    avg    <- rep(NA_real_, length(combos))
  } else {
    combos <- list()
    avg    <- numeric()
  }

  ## ---- drop singletons ----
  keep <- vapply(combos, length, integer(1)) > 1L
  combos <- combos[keep]
  avg    <- avg[keep]

  ## ---- empty-result early return ----
  if (length(combos) == 0L) {
    return(new("CorrCombo",
               subset_list = list(),
               avg_corr    = numeric(),
               min_corr    = numeric(),
               max_corr    = numeric(),
               names       = varnames,
               threshold   = threshold,
               forced_in   = force_names,
               search_type = method,
               n_rows_used = as.integer(n)))
  }

  ## ---- map indices to names ----
  named_sets <- lapply(combos, function(idx) varnames[idx])

  ## ---- compute min/max correlations ----
  get_minmax <- function(idx) {
    sub <- abs(mat[idx, idx])
    cors <- sub[lower.tri(sub)]
    c(min(cors, na.rm = TRUE), max(cors, na.rm = TRUE))
  }
  mm <- t(vapply(combos, get_minmax, numeric(2)))

  ## ---- build CorrCombo object ----
  result <- new("CorrCombo",
                subset_list = named_sets,
                avg_corr    = avg,
                min_corr    = mm[,1],
                max_corr    = mm[,2],
                names       = varnames,
                threshold   = threshold,
                forced_in   = force_names,
                search_type = method,
                n_rows_used = as.integer(n))

  if (method == "bron-kerbosch") {
    attr(result, "use_pivot") <- use_pivot
  }

  result
}
