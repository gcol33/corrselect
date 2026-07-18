#' Internal dispatcher to the Rcpp-generated findAllMaxSets() export — do not export
#'
#' Translates R-facing conventions (hyphenated method name, 1-based force_in)
#' into what the C++ backend expects, then calls the generated wrapper in
#' RcppExports.R rather than hardcoding a second `.Call()` to the same symbol.
#' @noRd
.findAllMaxSetsR <- function(
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

  findAllMaxSets(
    corMatrix = corMatrix,
    threshold = threshold,
    method    = method,      # string ("els" or "bron_kerbosch")
    force_in  = fi,          # IntegerVector (0-based)
    use_pivot = use_pivot
  )
}
