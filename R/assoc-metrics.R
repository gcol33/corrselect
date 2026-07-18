## Shared association-metric primitives used by corrSelect(), assocSelect(),
## and corrPrune(). Kept in one place so a fix to NA/constant-column handling
## or a metric's definition applies to every caller at once.

# Association value for a single pair of variables, used by assocSelect()
# and corrPrune()'s mixed-type branch. `type_x`/`type_y` (one of "numeric",
# "ordered", "factor") are only consulted by the "eta" method, to identify
# which side is the categorical variable.
.pairwise_assoc_value <- function(x, y, method, type_x, type_y) {
  # length <= 1 means there is not enough data to estimate an association at
  # all (e.g. every row but one was dropped for missing values) -- undefined,
  # not known to be 0.
  if (length(x) <= 1 || length(y) <= 1) return(NA_real_)

  # A fully-observed constant column (either variable, any type) carries no
  # information about anything, so its association with any other variable
  # is well-defined as 0. Restricted to columns with no NA at all: a column
  # that is constant only among its non-missing values is an NA-handling
  # question, not this constant-column question.
  if ((!anyNA(x) && length(unique(x)) == 1) ||
      (!anyNA(y) && length(unique(y)) == 1)) {
    return(0)
  }

  switch(method,
    # rank()/as.numeric() coercion: spearman/kendall are also used for
    # numeric-ordered and ordered-ordered pairs (see .full_assoc_method_map()),
    # and stats::cor() rejects factor input directly. Rank-based methods are
    # invariant under any monotonic transform, so ranking first doesn't change
    # the result for already-numeric input.
    pearson  = abs(stats::cor(as.numeric(x), as.numeric(y), method = "pearson", use = "complete.obs")),
    spearman = abs(stats::cor(rank(x), rank(y), method = "spearman", use = "complete.obs")),
    kendall  = abs(stats::cor(rank(x), rank(y), method = "kendall", use = "complete.obs")),
    bicor = {
      if (!requireNamespace("WGCNA", quietly = TRUE)) stop("Install the 'WGCNA' package for bicor.")
      abs(WGCNA::bicor(cbind(x, y))[1, 2])
    },
    distance = {
      if (!requireNamespace("energy", quietly = TRUE)) stop("Install the 'energy' package for distance correlation.")
      abs(energy::dcor(x, y))
    },
    maximal = {
      if (!requireNamespace("minerva", quietly = TRUE)) stop("Install the 'minerva' package for maximal information coefficient.")
      minerva::mine(x, y)$MIC
    },
    cramersv = {
      tbl <- table(x, y)
      if (min(dim(tbl)) < 2 || any(rowSums(tbl) == 0) || any(colSums(tbl) == 0)) return(NA_real_)
      chi2 <- suppressWarnings(stats::chisq.test(tbl, correct = FALSE)$statistic)
      if (length(chi2) == 0 || is.na(chi2)) return(NA_real_)
      sqrt(chi2 / (sum(tbl) * (min(dim(tbl)) - 1)))
    },
    eta = {
      cat_var <- if (type_x == "factor") x else y
      num_var <- if (type_x == "numeric") x else y
      ss_tot <- sum((num_var - mean(num_var))^2)
      sqrt(sum(tapply(num_var, cat_var,
                       function(z) length(z) * (mean(z) - mean(num_var))^2)) / ss_tot)
    },
    stop("Unsupported association method: ", method)
  )
}

# Type-pair -> method lookup table shared by assocSelect() and corrPrune().
# `method_num_ord`/`method_ord_ord` default to "spearman" to match
# corrPrune()'s fixed (non-configurable) mixed-type dispatch; assocSelect()
# passes its own user-configured values.
.full_assoc_method_map <- function(method_num_num,
                                    method_num_ord = "spearman",
                                    method_ord_ord = "spearman") {
  list(
    numeric_numeric = method_num_num,
    numeric_ordered = method_num_ord,
    ordered_numeric = method_num_ord,
    numeric_factor  = "eta",
    factor_numeric  = "eta",
    ordered_ordered = method_ord_ord,
    ordered_factor  = "cramersv",
    factor_ordered  = "cramersv",
    factor_factor   = "cramersv"
  )
}

# Pairwise mixed-type association matrix for a complete-case (NA-free) data
# frame `df` whose per-column types are given in `types` (one of "numeric",
# "ordered", "factor" per column, as returned by
# vapply(df, function(x) class(x)[1], character(1))). `full_assoc_methods`
# is a type-pair -> method map as built by .full_assoc_method_map().
# Returns a list with the association matrix and the methods actually used.
.mixed_type_assoc_matrix <- function(df, types, full_assoc_methods) {
  p   <- ncol(df)
  mat <- diag(1, p)
  colnames(mat) <- rownames(mat) <- names(df)
  assoc_methods_used <- list()

  for (i in seq_len(p - 1L)) {
    for (j in (i + 1L):p) {
      tx <- types[i]
      ty <- types[j]

      if (is.na(tx) || is.na(ty)) {
        stop("Unexpected NA in column types. Possibly unsupported variable types.")
      }

      key  <- paste(tx, ty, sep = "_")
      meth <- full_assoc_methods[[key]]
      if (is.null(meth)) stop("No rule for variable-type pair: ", key)
      if (is.null(assoc_methods_used[[key]])) assoc_methods_used[[key]] <- meth

      # An NA here means the association is genuinely undefined (e.g. a
      # sparse contingency table for Cramer's V), not that the variables are
      # known to be independent -- left as NA so callers can surface it
      # explicitly instead of silently treating it as 0 (compatible).
      a <- .pairwise_assoc_value(df[[i]], df[[j]], meth, tx, ty)
      if (!is.na(a)) a <- max(0, min(1, a))
      mat[i, j] <- mat[j, i] <- a
    }
  }

  list(mat = mat, assoc_methods_used = assoc_methods_used)
}

# Vectorized numeric-only association matrix, shared by corrSelect() and
# corrPrune()'s all-numeric branch. `df_num` must already be complete-case
# (NA-free). Constant columns get association 0 with every other variable
# rather than the NA/NaN that the underlying correlation functions would
# otherwise produce, matching the constant-column contract in
# .pairwise_assoc_value() above.
.numeric_assoc_matrix <- function(df_num, method) {
  p <- ncol(df_num)
  is_const <- vapply(df_num, function(x) stats::sd(x) == 0, logical(1))

  mat <- switch(
    method,
    # A constant column makes the underlying correlation NA/NaN for that
    # column, which is expected and handled below (is_const zero-out) rather
    # than a condition the caller needs to see a warning about.
    pearson  = suppressWarnings(stats::cor(df_num, use = "everything", method = "pearson")),
    spearman = suppressWarnings(stats::cor(df_num, use = "everything", method = "spearman")),
    kendall  = suppressWarnings(stats::cor(df_num, use = "everything", method = "kendall")),
    bicor = {
      if (!requireNamespace("WGCNA", quietly = TRUE)) stop("Install the 'WGCNA' package for bicor.")
      suppressWarnings(WGCNA::bicor(df_num))
    },
    distance = {
      if (!requireNamespace("energy", quietly = TRUE)) stop("Install the 'energy' package for distance correlation.")
      m <- diag(1, p)
      for (i in seq_len(p - 1)) {
        for (j in (i + 1):p) {
          m[i, j] <- m[j, i] <- energy::dcor(df_num[[i]], df_num[[j]])
        }
      }
      colnames(m) <- rownames(m) <- colnames(df_num)
      m
    },
    maximal = {
      if (!requireNamespace("minerva", quietly = TRUE)) stop("Install the 'minerva' package for maximal information coefficient.")
      m <- diag(1, p)
      for (i in seq_len(p - 1)) {
        for (j in (i + 1):p) {
          m[i, j] <- m[j, i] <- minerva::mine(df_num[[i]], df_num[[j]])$MIC
        }
      }
      colnames(m) <- rownames(m) <- colnames(df_num)
      m
    },
    stop("Unsupported association method: ", method)
  )

  if (any(is_const)) {
    mat[is_const, ] <- 0
    mat[, is_const] <- 0
    diag(mat) <- 1
  }

  mat
}
