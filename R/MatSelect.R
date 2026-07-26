#' @useDynLib corrselect, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

# Numeric tolerance for the diagonal-is-1 and symmetry checks on `mat` below.
.MATSELECT_TOL <- 1e-8

# Thresholds for the combinatorial-blowup heuristic warning below: only warn
# when there are more than this many variables *and* more than this share of
# pairs are compatibility-graph edges (i.e. at or below `threshold`).
.MATSELECT_BLOWUP_MIN_VARS <- 100L
.MATSELECT_BLOWUP_MAX_DENSITY <- 0.3

#' Select Variable Subsets with Low Correlation or Association (Matrix Interface)
#'
#' Identifies all maximal subsets of variables from a symmetric matrix (typically a correlation matrix)
#' such that all pairwise absolute values stay below a specified threshold. Implements exact algorithms
#' such as Eppstein–Löffler–Strash (ELS) and Bron–Kerbosch (with or without pivoting).
#'
#' @param mat A numeric, symmetric matrix with 1s on the diagonal (e.g. correlation matrix).
#'        Column names (if present) are used to label output variables.
#' @param threshold A numeric scalar in (0, 1]. Maximum allowed absolute pairwise value.
#'        Defaults to \code{0.7}.
#' @param method Character. Selection algorithm to use. One of \code{"els"} or
#'        \code{"bron-kerbosch"}. If not specified, the function chooses automatically:
#'        \code{"els"} when \code{force_in} is provided, otherwise \code{"bron-kerbosch"}.
#' @param force_in Optional integer vector of 1-based column indices to force into every subset.
#'        If the forced variables are themselves mutually correlated beyond \code{threshold},
#'        \code{MatSelect()} warns (naming the offending pair) but still forces them into every
#'        returned subset -- unlike \code{\link{corrPrune}()}, which treats this condition as
#'        infeasible and errors instead.
#' @param ... Additional arguments passed to the backend. The only supported
#'        argument is \code{use_pivot} (logical), for enabling pivoting in
#'        Bron–Kerbosch (ignored by ELS); any other named argument is an error.
#'
#' @return An object of class \code{\link{CorrCombo}}, containing all valid subsets and their
#' correlation statistics. If every variable is pairwise correlated above \code{threshold},
#' the only valid maximal subsets are single variables; these are returned with
#' \code{min_corr}/\code{max_corr} set to \code{NA} (there is no pair to summarize).
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
#' # Force variables 1 and 2 into every subset (warns if they are mutually
#' # correlated beyond the threshold; both are still forced in regardless)
#' res4 <- MatSelect(cmat, threshold = 0.5, force_in = c(1, 2))
#'
#' @export
MatSelect <- function(mat,
                      threshold = 0.7,
                      method = NULL,
                      force_in = NULL,
                      ...) {

  ## ---- Input validation ----
  # Runs before force_in resolution below: force_in's own checks assume `mat`
  # is already a valid matrix (colnames()/ncol() on a malformed `mat` would
  # otherwise surface as a misleading force_in-flavored error instead of the
  # real problem with `mat` itself).
  if (!is.matrix(mat) || !is.numeric(mat)) {
    stop("`mat` must be a numeric matrix.")
  }
  if (nrow(mat) != ncol(mat)) {
    stop("`mat` must be square.")
  }
  if (ncol(mat) < 2) {
    stop("`mat` must have at least two columns.")
  }
  if (anyDuplicated(colnames(mat))) {
    stop("`mat` has duplicate column names: ",
         paste(unique(colnames(mat)[duplicated(colnames(mat))]), collapse = ", "))
  }
  if (anyNA(mat)) {
    stop("`mat` must not contain NA.")
  }
  if (!all(abs(diag(mat) - 1) < .MATSELECT_TOL)) {
    stop("Diagonal entries of `mat` must be 1.")
  }
  if (!all(abs(mat - t(mat)) < .MATSELECT_TOL)) {
    stop("`mat` must be symmetric.")
  }
  .validate_threshold(threshold)
  n <- ncol(mat)

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

    # Now: must be valid 1-based indices. Whole-number-ness is checked
    # explicitly -- as.integer() truncates rather than rounds, so a
    # non-integer index (e.g. from floating-point drift) would otherwise
    # silently resolve to a different, valid column instead of erroring.
    if (!is.numeric(force_in) || anyNA(force_in) ||
        any(force_in != as.integer(force_in)) ||
        any(force_in < 1) || any(force_in > ncol(mat))) {
      stop("`force_in` must be valid 1-based column indices or names.")
    }

    force_in <- unique(force_in)
  } else {
    force_in <- integer(0)
  }

  # Conditionally select default method
  if (is.null(method)) {
    method <- if (length(force_in) > 0) "els" else "bron-kerbosch"
  } else {
    method <- match.arg(method, choices = c("bron-kerbosch", "els"))
  }

  ## ---- prepare names ----
  varnames   <- colnames(mat)
  if (is.null(varnames)) varnames <- paste0("V", seq_len(n))
  force_names <- if (!is.null(force_in)) varnames[force_in] else character()

  ## ---- forced_in vars that are too correlated internally ----
  # MatSelect() honors an explicit force_in request even when it is
  # internally incompatible with `threshold` -- unlike corrPrune(), which
  # treats the same condition as infeasible and stop()s (see #98). This
  # divergence is intentional: MatSelect() is the low-level enumeration
  # primitive and force_in is a direct instruction, while corrPrune()
  # promises a single subset that satisfies threshold. The warning naming
  # the specific offending pair is issued by the shared C++ validation path
  # (warnIfForcedMutuallyIncompatible() in src/utils.cpp, called from
  # runELS()/runBronKerbosch() below), so a caller who bypasses MatSelect()
  # and calls those Rcpp exports directly still gets it (#111).

  ## ---- warn about possible combinatorial blowup ----
  # Exhaustive maximal-subset enumeration is worst-case exponential in the
  # number of variables. Only warn when that's actually plausible: a large
  # variable count *and* a permissive-enough threshold that a large share of
  # pairs are compatible (a strict threshold keeps the compatibility graph
  # sparse, where blowup isn't a practical risk).
  if (n > .MATSELECT_BLOWUP_MIN_VARS) {
    compat_density <- mean(abs(mat[upper.tri(mat)]) <= threshold)
    if (compat_density > .MATSELECT_BLOWUP_MAX_DENSITY) {
      warning(sprintf(
        "%d variables with %.0f%% of pairs at or below the threshold: exhaustive maximal-subset enumeration can be exponential in the worst case. Consider corrPrune(mode = \"greedy\") for large variable counts.",
        n, 100 * compat_density
      ))
    }
  }

  ## ---- backend options ----
  dots      <- list(...)
  dot_names <- names(dots)
  if (is.null(dot_names)) dot_names <- rep.int("", length(dots))
  unrecognized_dots <- setdiff(dot_names, "use_pivot")
  if (length(unrecognized_dots) > 0) {
    stop(sprintf(
      "Unrecognized argument(s) in `...`: %s. The only supported `...` argument is `use_pivot`.",
      paste(unrecognized_dots, collapse = ", ")
    ))
  }
  use_pivot <- TRUE
  if ("use_pivot" %in% names(dots)) {
    tmp <- suppressWarnings(as.logical(dots$use_pivot))
    if (length(tmp) != 1 || is.na(tmp)) {
      stop("`use_pivot` must be a single TRUE/FALSE value.")
    }
    use_pivot <- tmp
    if (method == "els") {
      warning("`use_pivot` has no effect when method = \"els\" (ELS always uses pivoting internally); the supplied value is ignored.")
    }
  }

  ## ---- dispatch to C/C++ backend ----
  raw_out <- .findAllMaxSetsR(
    corMatrix = mat,
    threshold = threshold,
    method    = method,
    force_in  = force_in,
    use_pivot = use_pivot
  )

  ## ---- extract combos & avg_corr ----
  # findAllMaxSets() (src/corrselect_main.cpp) always returns either an empty
  # list or a list of list(combo=, avg_corr=) elements -- there is no other
  # shape to guard against. An unexpected shape here means the C++ contract
  # changed without this extraction being updated to match.
  if (length(raw_out) == 0L) {
    combos <- list()
    avg    <- numeric()
  } else if (is.list(raw_out) &&
             is.list(raw_out[[1]]) &&
             all(c("combo", "avg_corr") %in% names(raw_out[[1]]))) {
    combos <- lapply(raw_out, `[[`, "combo")
    avg    <- vapply(raw_out, `[[`, numeric(1), "avg_corr")
  } else {
    stop("Internal error: unexpected return shape from findAllMaxSets(). ",
         "Expected a list of list(combo=, avg_corr=) elements.")
  }

  ## ---- empty-result early return ----
  if (length(combos) == 0L) {
    return(CorrCombo(
               subset_list = list(),
               avg_corr    = numeric(),
               min_corr    = numeric(),
               max_corr    = numeric(),
               var_names   = varnames,
               threshold   = threshold,
               forced_in   = force_names,
               search_type = method,
               n_rows_used = NA_integer_))
  }

  ## ---- map indices to names ----
  named_sets <- lapply(combos, function(idx) varnames[idx])

  ## ---- compute min/max correlations ----
  get_minmax <- function(idx) {
    # A size-1 subset has no pairwise correlation to report.
    if (length(idx) < 2L) {
      return(c(NA_real_, NA_real_))
    }
    sub <- abs(mat[idx, idx])
    cors <- sub[lower.tri(sub)]
    c(min(cors, na.rm = TRUE), max(cors, na.rm = TRUE))
  }
  mm <- t(vapply(combos, get_minmax, numeric(2)))

  ## ---- build CorrCombo object ----
  result <- CorrCombo(
                subset_list = named_sets,
                avg_corr    = avg,
                min_corr    = mm[,1],
                max_corr    = mm[,2],
                var_names   = varnames,
                threshold   = threshold,
                forced_in   = force_names,
                search_type = method,
                n_rows_used = NA_integer_)

  if (method == "bron-kerbosch") {
    attr(result, "use_pivot") <- use_pivot
  }

  result
}
