#' Internal dispatcher to C++ — do not export
#' @noRd
findAllMaxSets <- function(
    corMatrix,
    threshold,
    method = c("els", "bron-kerbosch"),
    force_in = NULL,     # 1-based indices here
    use_pivot = TRUE
) {
  method <- match.arg(method)
  # translate R’s hyphenated name into the C++‐expected underscore form
  method <- if (method == "bron-kerbosch") "bron_kerbosch" else method

  # Convert 1-based force_in -> 0-based for C++
  if (!is.null(force_in)) {
    if (!is.numeric(force_in) ||
        any(force_in < 1) ||
        any(force_in > ncol(corMatrix))) {
      stop("`force_in` must be valid 1-based column indices.")
    }
    fi <- as.integer(force_in - 1L)
  } else {
    fi <- integer(0)
  }

  .Call(
    "_corrselect_findAllMaxSets",
    corMatrix,   # NumericMatrix
    threshold,   # double
    method,      # string ("els" or "bron_kerbosch")
    fi,          # IntegerVector (0-based)
    use_pivot    # logical
  )
}
