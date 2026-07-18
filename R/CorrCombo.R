#' @title CorrCombo Class
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
#'
#' @details
#' Properties:
#' \describe{
#'   \item{subset_list}{A list of character vectors. Each vector is a valid subset (variable names).}
#'   \item{avg_corr}{A numeric vector. Average absolute correlation within each subset.}
#'   \item{min_corr}{A numeric vector. Minimum pairwise absolute correlation in each subset.}
#'   \item{max_corr}{A numeric vector. Maximum pairwise absolute correlation within each subset.}
#'   \item{var_names}{Character vector of all variable names used for decoding.}
#'   \item{threshold}{Numeric scalar. The correlation threshold used during selection.}
#'   \item{forced_in}{Character vector. Variable names that were forced into each subset.}
#'   \item{search_type}{Character string. One of \code{"els"} or \code{"bron-kerbosch"}.}
#'   \item{cor_method}{Character string. Either a single method (e.g. "pearson") or "mixed" if multiple methods used.}
#'   \item{n_rows_used}{Integer. Number of rows used for computing the correlation matrix (after removing missing values). \code{NA} when constructed from a matrix directly (e.g. via \code{\link{MatSelect}}), since a matrix input has no associated row count.}
#' }
#'
#' @param subset_list A list of character vectors. Each vector is a valid subset (variable names).
#' @param avg_corr A numeric vector. Average absolute correlation within each subset.
#' @param min_corr A numeric vector. Minimum pairwise absolute correlation in each subset.
#' @param max_corr A numeric vector. Maximum pairwise absolute correlation within each subset.
#' @param var_names Character vector of all variable names used for decoding.
#' @param threshold Numeric scalar. The correlation threshold used during selection.
#' @param forced_in Character vector. Variable names forced into each subset. Defaults to \code{character()}.
#' @param search_type Character string. One of \code{"els"} or \code{"bron-kerbosch"}.
#' @param cor_method Character string. Correlation method or \code{"mixed"}. Defaults to \code{character()}.
#' @param n_rows_used Integer. Number of rows used for computing the correlation matrix. \code{NA} for matrix input.
#'
#' @seealso \code{\link{corrSelect}}, \code{\link{MatSelect}}, \code{\link{corrSubset}}
#'
#' @examples
#' print(CorrCombo(
#'   subset_list = list(c("A", "B"), c("A", "C")),
#'   avg_corr = c(0.2, 0.3),
#'   min_corr = c(0.1, 0.2),
#'   max_corr = c(0.3, 0.4),
#'   var_names = c("A", "B", "C"),
#'   threshold = 0.5,
#'   forced_in = character(),
#'   search_type = "els",
#'   cor_method = "mixed",
#'   n_rows_used = 5L
#' ))
#'
#' @importFrom S7 new_class new_property class_list class_double class_character class_integer
#' @export
CorrCombo <- new_class("CorrCombo",
  package = NULL,
  properties = list(
    subset_list = class_list,
    avg_corr    = class_double,
    min_corr    = class_double,
    max_corr    = class_double,
    var_names   = class_character,
    threshold   = class_double,
    forced_in   = new_property(class_character, default = character()),
    search_type = class_character,
    cor_method  = new_property(class_character, default = character()),
    n_rows_used = class_integer
  ),
  validator = function(self) {
    n <- length(self@subset_list)
    check_lengths <- function(slot_value) {
      len <- length(slot_value)
      len == 0 || len == n
    }
    if (!check_lengths(self@avg_corr)) return("avg_corr must match subset_list length or be empty.")
    if (!check_lengths(self@min_corr)) return("min_corr must match subset_list length or be empty.")
    if (!check_lengths(self@max_corr)) return("max_corr must match subset_list length or be empty.")
    NULL
  }
)


#' @rdname CorrCombo
#' @param x A \code{CorrCombo} object to be printed.
#' @param ... Additional arguments (ignored).
#' @export
print.CorrCombo <- function(x, ...) {
  n <- length(x@subset_list)
  cat("CorrCombo object\n")
  cat("-----------------\n")
  cat(sprintf("  Method:      %s\n", x@search_type))
  cat(sprintf("  Correlation: %s\n", x@cor_method))

  assoc_methods <- attr(x, "assoc_methods_used")
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

  cat(sprintf("  Threshold:   %.3f\n", x@threshold))
  cat(sprintf("  Subsets:     %d maximal subsets\n", n))
  if (is.na(x@n_rows_used)) {
    cat("  Data Rows:   not applicable (matrix input)\n")
  } else {
    cat(sprintf("  Data Rows:   %d used in correlation\n", x@n_rows_used))
  }

  if (x@search_type == "bron-kerbosch") {
    use_pivot <- attr(x, "use_pivot")
    if (!is.null(use_pivot)) {
      cat(sprintf("  Pivot:       %s\n", use_pivot))
    }
  }

  if (length(x@forced_in)) {
    cat(sprintf("  Forced-in:   %s\n", paste(x@forced_in, collapse = ", ")))
  }

  if (n == 0) {
    cat("\n  No valid subsets found.\n")
    return(invisible(x))
  }

  cat("\nTop combinations:\n")
  cat("  No.  Variables                          Avg    Max    Size\n")
  cat("  ------------------------------------------------------------\n")

  show_n <- min(5, n)
  for (i in seq_len(show_n)) {
    vars <- x@subset_list[[i]]
    avg  <- x@avg_corr[i]
    maxc <- x@max_corr[i]
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

  invisible(x)
}


#' @rdname CorrCombo
#' @param object A \code{CorrCombo} object to summarize.
#' @export
summary.CorrCombo <- function(object, ...) {
  x <- object
  n <- length(x@subset_list)
  sizes <- lengths(x@subset_list)

  result <- list(
    n_subsets      = n,
    search_type    = x@search_type,
    cor_method     = x@cor_method,
    threshold      = x@threshold,
    n_rows_used    = x@n_rows_used,
    forced_in      = x@forced_in,
    size_range     = if (n) range(sizes) else c(NA_integer_, NA_integer_),
    size_median    = if (n) stats::median(sizes) else NA_real_,
    avg_corr_range = if (n) range(x@avg_corr) else c(NA_real_, NA_real_),
    n_max_size     = if (n) sum(sizes == max(sizes)) else 0L
  )
  class(result) <- "summary.CorrCombo"
  result
}

#' @rdname CorrCombo
#' @param x A \code{summary.CorrCombo} object to be printed.
#' @export
print.summary.CorrCombo <- function(x, ...) {
  cat("CorrCombo summary\n")
  cat("-----------------\n")
  cat(sprintf("  Method:      %s\n", x$search_type))
  cat(sprintf("  Correlation: %s\n", x$cor_method))
  cat(sprintf("  Threshold:   %.3f\n", x$threshold))
  cat(sprintf("  Subsets:     %d maximal subsets\n", x$n_subsets))

  if (x$n_subsets > 0) {
    cat(sprintf(
      "  Subset size: min %d, median %.1f, max %d (%d subset%s at max size)\n",
      x$size_range[1], x$size_median, x$size_range[2],
      x$n_max_size, if (x$n_max_size == 1) "" else "s"
    ))
    cat(sprintf(
      "  Avg corr:    range [%.3f, %.3f] across subsets\n",
      x$avg_corr_range[1], x$avg_corr_range[2]
    ))
  }

  if (is.na(x$n_rows_used)) {
    cat("  Data Rows:   not applicable (matrix input)\n")
  } else {
    cat(sprintf("  Data Rows:   %d used in correlation\n", x$n_rows_used))
  }

  if (length(x$forced_in)) {
    cat(sprintf("  Forced-in:   %s\n", paste(x$forced_in, collapse = ", ")))
  }

  invisible(x)
}


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
#' res <- MatSelect(cor(mat), threshold = 0.5)
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
