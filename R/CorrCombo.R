#' @title CorrCombo S4 class
#'
#' @description
#' Holds the result of \code{\link{corrSelect}} or \code{\link{MatSelect}}: a list of valid variable combinations
#' and their correlation statistics.
#'
#' This class stores all subsets of variables that meet the specified correlation constraint,
#' along with metadata such as the algorithm used, correlation method(s), variables forced into every subset,
#' and summary statistics for each combination.
#'
#' @name CorrCombo
#' @docType class
#' @aliases CorrCombo-class
#'
#' @slot subset_list   A list of character vectors. Each vector is a valid subset (variable names).
#' @slot avg_corr      A numeric vector. Average absolute correlation within each subset.
#' @slot min_corr      A numeric vector. Minimum pairwise absolute correlation in each subset.
#' @slot max_corr      A numeric vector. Maximum pairwise absolute correlation within each subset.
#' @slot names         Character vector of all variable names used for decoding.
#' @slot threshold     Numeric scalar. The correlation threshold used during selection.
#' @slot forced_in     Character vector. Variable names that were forced into each subset.
#' @slot search_type   Character string. One of \code{"els"} or \code{"bron-kerbosch"}.
#' @slot cor_method    Character string. Either a single method (e.g. "pearson") or "mixed" if multiple methods used.
#' @slot n_rows_used   Integer. Number of rows used for computing the correlation matrix (after removing missing values).
#'
#' @seealso \code{\link{corrSelect}}, \code{\link{MatSelect}}, \code{\link{corrSubset}}
#'
#' @examples
#' show(new("CorrCombo",
#'   subset_list = list(c("A", "B"), c("A", "C")),
#'   avg_corr = c(0.2, 0.3),
#'   min_corr = c(0.1, 0.2),
#'   max_corr = c(0.3, 0.4),
#'   names = c("A", "B", "C"),
#'   threshold = 0.5,
#'   forced_in = character(),
#'   search_type = "els",
#'   cor_method = "mixed",
#'   n_rows_used = as.integer(5)
#' ))
#'
#' @importFrom methods new
#' @export
setClass(
  "CorrCombo",
  slots = list(
    subset_list   = "list",
    avg_corr      = "numeric",
    min_corr      = "numeric",
    max_corr      = "numeric",
    names         = "character",
    threshold     = "numeric",
    forced_in     = "character",
    search_type   = "character",
    cor_method    = "character",
    n_rows_used   = "integer"
  ),
  validity = function(object) {
    n <- length(object@subset_list)
    check_lengths <- function(slot_value) {
      len <- length(slot_value)
      len == 0 || len == n
    }
    if (!check_lengths(object@avg_corr)) return("avg_corr must match subset_list length or be empty.")
    if (!check_lengths(object@min_corr)) return("min_corr must match subset_list length or be empty.")
    if (!check_lengths(object@max_corr)) return("max_corr must match subset_list length or be empty.")
    TRUE
  }
)

#' @rdname CorrCombo
#' @aliases show,CorrCombo-method
#' @importMethodsFrom methods show
#' @exportMethod show
#' @param object A \code{CorrCombo} object to be printed.
#' @title CorrCombo Class
#'
#' @description
#' An S4 class that stores the result of correlation-based subset selection.
#'
#' @slot subset_list A list of character vectors, each representing a subset of variable names.
#' @slot avg_corr Numeric vector: average correlation of each subset.
#' @slot min_corr Numeric vector: minimum correlation of each subset.
#' @slot max_corr Numeric vector: maximum correlation of each subset.
#' @slot names Character vector of variable names in the original matrix.
#' @slot threshold Numeric threshold used for correlation filtering.
#' @slot forced_in Character vector of variables that were forced into all subsets.
#' @slot search_type Character: the search algorithm used (e.g., "els", "bron-kerbosch").
#' @slot cor_method Character: the correlation method used.
#' @slot n_rows_used Integer: number of rows used to compute correlations.
#'
#' @export
setClass(
  "CorrCombo",
  slots = list(
    subset_list   = "list",
    avg_corr      = "numeric",
    min_corr      = "numeric",
    max_corr      = "numeric",
    names         = "character",
    threshold     = "numeric",
    forced_in     = "character",
    search_type   = "character",
    cor_method    = "character",
    n_rows_used   = "integer"
  ),
  validity = function(object) {
    n <- length(object@subset_list)
    check_lengths <- function(slot_value) {
      len <- length(slot_value)
      len == 0 || len == n
    }
    if (!check_lengths(object@avg_corr)) return("avg_corr must match subset_list length or be empty.")
    if (!check_lengths(object@min_corr)) return("min_corr must match subset_list length or be empty.")
    if (!check_lengths(object@max_corr)) return("max_corr must match subset_list length or be empty.")
    TRUE
  }
)

#' @rdname CorrCombo
#' @aliases show,CorrCombo-method
#' @importMethodsFrom methods show
#' @exportMethod show
#' @param object A \code{CorrCombo} object to be printed.
setMethod("show", "CorrCombo", function(object) {
  n <- length(object@subset_list)
  cat("CorrCombo object\n")
  cat("-----------------\n")
  cat(sprintf("  Method:      %s\n", object@search_type))
  cat(sprintf("  Correlation: %s\n", object@cor_method))

  assoc_methods <- attr(object, "assoc_methods_used")
  if (!is.null(assoc_methods) && length(assoc_methods)) {
    entries <- sprintf("%s = %s", names(assoc_methods), unlist(assoc_methods))
    full_str <- paste(entries, collapse = ", ")
    prefix <- "  AssocMethod: "
    wrap_w <- getOption("width", 80) - nchar(prefix)
    wrapped <- strwrap(full_str, width = wrap_w, exdent = 0)
    cat(prefix, wrapped[1], "\n", sep = "")
    if (length(wrapped) > 1) {
      indent <- strrep(" ", nchar(prefix))
      for (line in wrapped[-1]) cat(indent, line, "\n", sep = "")
    }
  }

  cat(sprintf("  Threshold:   %.3f\n", object@threshold))
  cat(sprintf("  Subsets:     %d valid combinations\n", n))
  cat(sprintf("  Data Rows:   %d used in correlation\n", object@n_rows_used))

  if (object@search_type == "bron-kerbosch") {
    use_pivot <- attr(object, "use_pivot")
    if (!is.null(use_pivot)) {
      cat(sprintf("  Pivot:       %s\n", use_pivot))
    }
  }

  if (length(object@forced_in)) {
    cat(sprintf("  Forced-in:   %s\n", paste(object@forced_in, collapse = ", ")))
  }

  if (n == 0) {
    cat("\n  No valid subsets found.\n")
    return(invisible(NULL))
  }

  cat("\nTop combinations:\n")
  cat("  No.  Variables                          Avg    Max    Size\n")
  cat("  ------------------------------------------------------------\n")

  show_n <- min(5, n)
  for (i in seq_len(show_n)) {
    vars <- object@subset_list[[i]]
    avg  <- object@avg_corr[i]
    maxc <- object@max_corr[i]
    size <- length(vars)

    max_vars_display <- 6
    var_str <- if (length(vars) > max_vars_display) {
      paste(c(vars[1:max_vars_display], "..."), collapse = ", ")
    } else {
      paste(vars, collapse = ", ")
    }
    if (nchar(var_str) > 32) {
      var_str <- paste0(substr(var_str, 1, 29), "...")
    }

    cat(sprintf("  [%2d] %-33s %5.3f  %5.3f   %3d\n",
                i, var_str, avg, maxc, size))
  }

  if (n > show_n) {
    cat(sprintf("  ... (%d more combinations)\n", n - show_n))
  }

  invisible(NULL)
})


#' @title Coerce CorrCombo to a Data Frame
#'
#' @description Converts a \code{CorrCombo} object into a data frame of variable combinations.
#'
#' @name as.data.frame.CorrCombo
#' @method as.data.frame CorrCombo
#' @export
#'
#' @param x A \code{CorrCombo} object.
#' @param row.names Optional row names for the output data frame.
#' @param optional Logical. Passed to \code{data.frame()}.
#' @param ... Additional arguments passed to \code{data.frame()}.
#'
#' @return A data frame where each row corresponds to a subset of variables. Columns are named
#'   \code{VarName01}, \code{VarName02}, ..., up to the size of the largest subset. Subsets shorter than the
#'   maximum length are padded with \code{NA}.
#'
#' @seealso \code{\link{CorrCombo}}
#'
#' @examples
#' set.seed(1)
#' mat <- matrix(rnorm(100), ncol = 10)
#' colnames(mat) <- paste0("V", 1:10)
#' res <- corrSelect(cor(mat), threshold = 0.5)
#' as.data.frame(res)
as.data.frame.CorrCombo <- function(x, row.names = NULL, optional = FALSE, ...) {
  n <- length(x@subset_list)
  if (n == 0) {
    mat0 <- matrix(nrow = 0, ncol = 0)
    return(as.data.frame(mat0,
                         row.names        = row.names,
                         optional         = optional,
                         stringsAsFactors = FALSE,
                         ...))
  }

  max_len <- max(lengths(x@subset_list))
  mat <- t(vapply(
    x@subset_list,
    function(s) c(s, rep(NA_character_, max_len - length(s))),
    FUN.VALUE = character(max_len)
  ))

  df <- as.data.frame(mat,
                      optional = optional,
                      stringsAsFactors = FALSE,
                      ...)

  colnames(df) <- sprintf("VarName%02d", seq_len(ncol(df)))

  if (is.null(row.names)) {
    rownames(df) <- sprintf("Subset%02d [avg=%.3f]", seq_len(n), x@avg_corr)
  } else {
    rownames(df) <- row.names
  }

  df
}
