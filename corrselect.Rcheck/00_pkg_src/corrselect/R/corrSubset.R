#' Extract Variable Subsets from a CorrCombo Object
#'
#' Extracts one or more variable subsets from a \code{\link{CorrCombo}} object as data frames.
#' Typically used after \code{\link{corrSelect}} or \code{\link{MatSelect}} to obtain filtered
#' versions of the original dataset containing only low‐correlation variable combinations.
#'
#' @param res A \code{\link{CorrCombo}} object returned by \code{corrSelect} or \code{MatSelect}.
#' @param df A data frame or matrix. Must contain all variables listed in \code{res@names}.
#'   Columns not in \code{res@names} are ignored unless \code{keepExtra = TRUE}.
#' @param which Subsets to extract. One of:
#'   \itemize{
#'     \item \code{"best"} (default) or \code{1}: the top‐ranked subset.
#'     \item A single integer (e.g. \code{2}): the nth ranked subset.
#'     \item A vector of integers (e.g. \code{1:3}): multiple subsets.
#'     \item \code{"all"}: all available subsets.
#'   }
#'   Subsets are ranked by decreasing size, then increasing average correlation.
#' @param keepExtra Logical. If \code{TRUE}, columns in \code{df} not in \code{res@names}
#'   (e.g., factors, characters) are retained. Defaults to \code{FALSE}.
#'
#' @return A data frame if a single subset is extracted, or a list of data frames if multiple
#'   subsets are extracted. Each data frame contains the selected variables (and optionally extras).
#'
#' @note A warning is issued if any rows contain missing values in the selected variables.
#'
#' @examples
#' # Simulate input data
#' set.seed(123)
#' df <- as.data.frame(matrix(rnorm(100), nrow = 10))
#' colnames(df) <- paste0("V", 1:10)
#'
#' # Compute correlation matrix
#' cmat <- cor(df)
#'
#' # Select subsets using corrSelect
#' res <- corrSelect(cmat, threshold = 0.5)
#'
#' # Extract the best subset (default)
#' corrSubset(res, df)
#'
#' # Extract the second-best subset
#' corrSubset(res, df, which = 2)
#'
#' # Extract the first three subsets
#' corrSubset(res, df, which = 1:3)
#'
#' # Extract all subsets
#' corrSubset(res, df, which = "all")
#'
#' # Extract best subset and retain additional numeric column
#' df$CopyV1 <- df$V1
#' corrSubset(res, df, which = 1, keepExtra = TRUE)
#'
#' @seealso \code{\link{corrSelect}}, \code{\link{MatSelect}}, \code{\link{CorrCombo}}
#' @export
corrSubset <- function(res, df, which = "best", keepExtra = FALSE) {
  # Validate inputs
  if (!inherits(res, "CorrCombo")) {
    stop("`res` must be a CorrCombo object.")
  }
  if (!is.data.frame(df) && !is.matrix(df)) {
    stop("`df` must be a data frame or matrix.")
  }
  missing_vars <- setdiff(res@names, colnames(df))
  if (length(missing_vars)) {
    stop("The following variables are missing in `df`: ",
         paste(missing_vars, collapse = ", "))
  }

  # Determine which subsets to extract
  subset_list <- res@subset_list
  if (identical(which, "all")) {
    indices <- seq_along(subset_list)
  } else if (is.character(which) && identical(which, "best")) {
    indices <- 1L
  } else if (is.numeric(which)) {
    indices <- as.integer(which)
    if (any(indices < 1 | indices > length(subset_list))) {
      stop("`which` indices are out of bounds.")
    }
  } else {
    stop("`which` must be \"best\", \"all\", or a valid integer index/vector.")
  }

  # Determine extra columns
  extra_cols <- if (keepExtra) setdiff(colnames(df), res@names) else character()

  # Extract data for each subset
  result_list <- lapply(indices, function(i) {
    vars <- subset_list[[i]]
    df[, c(vars, extra_cols), drop = FALSE]
  })

  # Warn if missing values in selected variables
  na_counts <- vapply(result_list, function(subdf) {
    vars <- intersect(colnames(subdf), res@names)
    sum(!complete.cases(subdf[, vars, drop = FALSE]))
  }, integer(1))
  n_rows <- vapply(result_list, nrow, integer(1))
  if (any(na_counts > 0)) {
    warning(
      "Some subsets contain rows with missing values:\n",
      paste(sprintf("Subset %d: %d of %d rows",
                    indices, na_counts, n_rows),
            collapse = "\n")
    )
  }

  # Return single data frame or list
  if (length(result_list) == 1) {
    result_list[[1]]
  } else {
    names(result_list) <- paste0("Subset", indices)
    result_list
  }
}
