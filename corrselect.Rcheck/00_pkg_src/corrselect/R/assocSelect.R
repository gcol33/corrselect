#' Select Variable Subsets with Low Association (Mixed-Type Data Frame Interface)
#'
#' Identifies combinations of variables of any common data type (numeric,
#' ordered factors, or unordered) factors—whose *pair-wise association* does not
#' exceed a user-supplied threshold.
#' The routine wraps \code{\link{MatSelect}()} and handles all pre-processing
#' (type conversion, missing-row removal, constant-column checks) for typical
#' data-frame/tibble/data-table inputs.
#'
#' A single call can therefore screen a data set that mixes continuous and
#' categorical features and return every subset whose internal associations are
#' “sufficiently low” under the metric(s) you choose.
#'
#' Rows containing \code{NA} are dropped with a warning; constant columns are
#' treated as having zero association with every other variable.
#'
#' @param df A data frame (or tibble / data.table). May contain any mix of:
#'   \itemize{
#'     \item numeric / integer (treated as numeric)
#'     \item ordered factors
#'     \item unordered factors (character vectors are coerced to factors)
#'   }
#' @param threshold Numeric in \eqn{(0,1)}. Maximum allowed pair-wise
#'   \emph{absolute} association. Default \code{0.7}.
#' @param method Character; the subset-search algorithm. One of
#'   \code{"els"} or \code{"bron-kerbosch"}.  If \code{NULL} (default) the
#'   function selects automatically: ELS when \code{force_in} is supplied,
#'   otherwise Bron–Kerbosch.
#' @param force_in Optional character vector or column indices specifying
#'   variables that must appear in every returned subset.
#' @param method_num_num Association measure for numeric–numeric pairs.
#'   One of \code{"pearson"} (default), \code{"spearman"}, \code{"kendall"},
#'   \code{"bicor"}, \code{"distance"}, or \code{"maximal"}.
#' @param method_num_ord Association measure for numeric–ordered pairs.
#'   One of \code{"spearman"} (default) or \code{"kendall"}.
#' @param method_ord_ord Association measure for ordered–ordered pairs.
#'   One of \code{"spearman"} (default) or \code{"kendall"}.
#' @param ...  Additional arguments passed unchanged to \code{\link{MatSelect}()}
#'   (e.g., \code{use_pivot = TRUE} for Bron–Kerbosch).
#'
#' @return A \code{\link{CorrCombo}} S4 object containing:
#'   \itemize{
#'     \item all valid subsets,
#'     \item their summary association statistics,
#'     \item metadata (algorithm used, rows kept, forced-in variables, etc.).
#'   }
#'   The object’s \code{show()} method prints the association metrics that were
#'   \emph{actually used} for this data set.
#'
#' @details
#' The default association measure for each variable-type combination is:
#'
#' \describe{
#'   \item{numeric – numeric}{\code{method_num_num} (default \code{"pearson"})}
#'   \item{numeric – ordered}{\code{method_num_ord}}
#'   \item{numeric – unordered}{\code{"eta"} (ANOVA \eqn{\eta^{2}})}
#'   \item{ordered – ordered}{\code{method_ord_ord}}
#'   \item{ordered – unordered}{\code{"cramersv"}}
#'   \item{unordered – unordered}{\code{"cramersv"}}
#' }
#'
#' All association measures are rescaled to \eqn{[0,1]} before thresholding.
#' External packages are required for
#' \code{"bicor"} (\pkg{WGCNA}),
#' \code{"distance"} (\pkg{energy}),
#' and \code{"maximal"} (\pkg{minerva}); an informative error is thrown if they
#' are missing.
#'
#'@seealso [corrSelect()], [MatSelect()], [corrSubset()]
#'
#' @examples
#' df <- data.frame(
#'   height = rnorm(15, 170, 10),
#'   weight = rnorm(15, 70, 12),
#'   group  = factor(rep(LETTERS[1:3], each = 5)),
#'   score  = ordered(sample(c("low","med","high"), 15, TRUE))
#' )
#'
#' ## keep every subset whose internal associations <= 0.6
#' assocSelect(df, threshold = 0.6)
#'
#' ## use Kendall for all rank-based comparisons and force 'height' to appear
#' assocSelect(df,
#'             threshold       = 0.5,
#'             method_num_num  = "kendall",
#'             method_num_ord  = "kendall",
#'             method_ord_ord  = "kendall",
#'             force_in        = "height")
#'
#' @importFrom stats cor complete.cases chisq.test
#' @importFrom methods is
#' @export
assocSelect <- function(df,
                        threshold = 0.7,
                        method = NULL,
                        force_in = NULL,
                        method_num_num = c("pearson", "spearman", "kendall",
                                           "bicor", "distance", "maximal"),
                        method_num_ord = c("spearman", "kendall"),
                        method_ord_ord = c("spearman", "kendall"),
                        ...) {

  ## ---------- preprocessing ----------
  df <- as.data.frame(df)
  # Auto-convert and drop unused levels
  df[] <- lapply(df, function(col) {
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



  if (ncol(df) < 2) stop("`df` needs at least two columns.")

  valid_types <- c("numeric", "ordered", "factor")
  types <- vapply(df, function(x) class(x)[1], character(1))
  bad   <- names(df)[!types %in% valid_types]
  if (length(bad))
    stop("Unsupported column types in: ", paste(bad, collapse = ", "))

  dropped <- sum(!complete.cases(df))
  if (dropped) {
    df <- df[complete.cases(df), ]
    warning(sprintf("Removed %d row%s with missing values.",
                    dropped, if (dropped == 1) "" else "s"))
  }

  ## ---------- finalise methods ----------
  method_num_num <- match.arg(method_num_num)
  method_num_ord <- match.arg(method_num_ord)
  method_ord_ord <- match.arg(method_ord_ord)

  full_assoc_methods <- list(
    numeric_numeric  = method_num_num,
    numeric_ordered  = method_num_ord,
    ordered_numeric  = method_num_ord,
    numeric_factor   = "eta",
    factor_numeric   = "eta",
    ordered_ordered  = method_ord_ord,
    ordered_factor   = "cramersv",
    factor_ordered   = "cramersv",
    factor_factor    = "cramersv"
  )

  assoc_methods_used <- list()

  ## ---------- primitive dispatcher ----------
  get_assoc <- function(x, y, meth, tx, ty) {
    if (length(unique(x)) < 2 || length(unique(y)) < 2) return(0)

    switch(meth,
           pearson  = abs(cor(x, y, method = "pearson")),
           spearman = abs(cor(rank(x), rank(y), method = "spearman")),
           kendall  = abs(cor(rank(x), rank(y), method = "kendall")),
           bicor = {
             requireNamespace("WGCNA")
             abs(WGCNA::bicor(cbind(x, y))[1, 2])
           },
           distance = {
             requireNamespace("energy")
             abs(energy::dcor(x, y))
           },
           maximal = {
             requireNamespace("minerva")
             minerva::mine(x, y)$MIC
           },
           cramersv = {
             tbl <- table(x, y)
             if (min(dim(tbl)) < 2 || any(tbl == 0)) return(NA_real_)
             suppressWarnings({
               chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE)$statistic)
             })
             if (length(chi2) == 0 || is.na(chi2)) return(NA_real_)
             sqrt(chi2 / (sum(tbl) * (min(dim(tbl)) - 1)))
           },
           eta = {
             cat <- if (tx == "factor") x else y
             num <- if (tx == "numeric") x else y
             if (length(unique(cat)) < 2) return(0)
             ss_tot <- sum((num - mean(num))^2)
             if (ss_tot == 0) return(0)
             ss_bet <- sum(tapply(num, cat,
                                  function(z) length(z) * (mean(z) - mean(num))^2))
             sqrt(ss_bet / ss_tot)
           },
           stop("Unsupported association method: ", meth)
    )
  }

  ## ---------- build matrix ----------
  p   <- ncol(df)
  mat <- diag(1, p)
  colnames(mat) <- rownames(mat) <- names(df)

  for (i in seq_len(p - 1L)) {
    for (j in (i + 1L):p) {
      tx <- types[i]
      ty <- types[j]

      if (is.na(tx) || is.na(ty)) {
        stop("Unexpected NA in column types. Possibly unsupported variable types.")
      }

      key <- paste(tx, ty, sep = "_")
      meth <- full_assoc_methods[[key]]

      if (is.null(meth)) {
        stop("No rule for variable-type pair: ", key)
      }

      if (is.null(assoc_methods_used[[key]])) {
        assoc_methods_used[[key]] <- meth
      }

      a <- get_assoc(df[[i]], df[[j]], meth, tx, ty)
      if (is.na(a)) {
        a <- 0  # fallback to 0 if association could not be computed
      }
      a <- max(0, min(1, a))
      mat[i, j] <- mat[j, i] <- a
    }
  }

  ## ---------- resolve force_in ----------
  if (!is.null(force_in)) {
    if (is.character(force_in)) {
      if (!all(force_in %in% names(df)))
        stop("Some entries in `force_in` do not match column names.")
      force_in <- match(force_in, names(df))
    }
  }

  ## ---------- subset selection ----------
  if (anyNA(mat)) {
    stop("Association matrix contains NA values. This may be caused by sparse combinations or unused factor levels.")
  }
  res <- MatSelect(mat, threshold, method, force_in, ...)
  res@n_rows_used <- nrow(df)
  res@cor_method  <- "mixed"

  # Save methods used as attributes (not slots)
  attr(res, "assoc_methods_used") <- assoc_methods_used
  attr(res, "assoc_methods_all")  <- full_assoc_methods

  res
}
