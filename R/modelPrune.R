#' Model-Based Predictor Pruning
#'
#' `modelPrune()` performs iterative removal of fixed-effect predictors based on
#' model diagnostics (e.g., VIF) until all remaining predictors satisfy a
#' specified threshold. It supports linear models, generalized linear models,
#' and mixed models.
#'
#' @param formula A model formula specifying the response and predictors.
#'   May include random effects for mixed models (e.g., `y ~ x1 + x2 + (1|group)`).
#' @param data A data.frame containing the variables in the formula.
#' @param engine Either a character string for built-in engines, or a list defining a custom engine.
#'
#'   **Built-in engines** (character string):
#'   - `"lm"` (default): Linear models via `stats::lm()`
#'   - `"glm"`: Generalized linear models via `stats::glm()` (requires `family` argument)
#'   - `"lme4"`: Mixed models via `lme4::lmer()` or `lme4::glmer()` (requires lme4 package)
#'   - `"glmmTMB"`: Generalized linear mixed models via `glmmTMB::glmmTMB()` (requires glmmTMB package)
#'
#'   **Custom engine** (named list with required components):
#'   - `fit`: function(formula, data, ...) that returns a fitted model object
#'   - `diagnostics`: function(model, fixed_effects) that returns a named numeric vector
#'     of diagnostic scores (one per fixed effect, higher values = worse)
#'   - `name` (optional): character string used in error messages (default: "custom")
#' @param criterion Character string specifying the diagnostic criterion for pruning.
#'   For built-in engines, supported values are:
#'   - `"vif"` (default): Variance Inflation Factor. Measures how much the variance
#'     of a coefficient is inflated due to collinearity. Values > 5-10 indicate
#'     problematic multicollinearity.
#'   - `"condition_number"`: Condition indices based on singular value decomposition
#'     of the design matrix. Higher values indicate greater collinearity.
#'   For custom engines, this parameter is ignored (diagnostics are computed by the
#'   engine's `diagnostics` function).
#' @param limit Numeric scalar. Maximum allowed value for the criterion.
#'   Predictors with diagnostic values exceeding this limit are iteratively removed.
#'   Default: 5 (common VIF threshold).
#' @param force_in Character vector of predictor names that must be retained
#'   in the final model. These variables will not be removed during pruning.
#'   Default: NULL.
#' @param max_steps Integer. Maximum number of pruning iterations. If NULL (default),
#'   pruning continues until all diagnostics are below the limit or no more
#'   removable predictors remain.
#' @param ... Additional arguments passed to the modeling function (e.g., `family`
#'   for glm/glmer, control parameters for lme4/glmmTMB).
#'
#' @return A data.frame containing only the retained predictors (and response).
#'   The result has the following attributes:
#'   \describe{
#'     \item{selected_vars}{Character vector of retained predictor names}
#'     \item{removed_vars}{Character vector of removed predictor names (in order of removal)}
#'     \item{engine}{Character string indicating which engine was used (for custom engines, this is the engine's `name` field)}
#'     \item{criterion}{Character string indicating which criterion was used}
#'     \item{limit}{The threshold value used}
#'     \item{final_model}{The final fitted model object (optional)}
#'   }
#'
#' @details
#' `modelPrune()` works by:
#' 1. Parsing the formula to identify fixed-effect predictors
#' 2. Fitting the initial model
#' 3. Computing diagnostics for each fixed-effect predictor
#' 4. Checking feasibility of `force_in` constraints
#' 5. Iteratively removing the predictor with the worst diagnostic value
#'    (excluding `force_in` variables) until all diagnostics <= `limit`
#' 6. Returning the pruned data frame
#'
#' **Random Effects**: For mixed models (lme4, glmmTMB), only fixed-effect
#' predictors are considered for pruning. Random-effect structure is preserved
#' exactly as specified in the original formula.
#'
#' **VIF Computation**: Variance Inflation Factors are computed from the
#' fixed-effects design matrix. For categorical predictors, VIF represents
#' the inflation for the entire factor (not individual dummy variables).
#'
#' **Condition Number Computation**: Condition indices are computed via
#' singular value decomposition of the fixed-effects design matrix, which is
#' centered and scaled (`scale(X, center = TRUE, scale = TRUE)`) before the
#' decomposition; this is a common convention for collinearity screening but
#' means the diagnostic does not reflect intercept-related collinearity the
#' way an uncentered decomposition would. For a categorical predictor with
#' multiple dummy columns, the predictor's condition index is approximated as
#' the maximum condition index across its associated columns, rather than a
#' proper joint decomposition restricted to that factor's subspace.
#'
#' **Determinism**: The algorithm is deterministic. Ties in diagnostic values
#' are broken by removing the predictor that appears last in the formula.
#'
#' **Force-in Constraints**: If variables in `force_in` violate the diagnostic
#' threshold, the function will error. This ensures that the constraint is
#' feasible before pruning begins.
#'
#' @seealso
#' \code{\link{corrPrune}} for association-based predictor pruning,
#' \code{\link{corrSelect}} for exhaustive subset enumeration.
#'
#' @examples
#' # Linear model with VIF-based pruning
#' data(mtcars)
#' pruned <- modelPrune(mpg ~ ., data = mtcars, engine = "lm", limit = 5)
#' names(pruned)
#'
#' # Force certain predictors to remain
#' pruned <- modelPrune(mpg ~ ., data = mtcars, force_in = "drat", limit = 20)
#'
#' # GLM example (requires family argument). Uses a small predictor set
#' # that avoids quasi-complete separation on this data (unlike `am ~ .`,
#' # which regresses a 32-row binary response on all 10 mtcars predictors).
#' pruned <- modelPrune(vs ~ mpg + wt + hp, data = mtcars, engine = "glm",
#'                      family = binomial(), limit = 5)
#'
#' \dontrun{
#' # Custom engine example (INLA)
#' inla_engine <- list(
#'   name = "inla",
#'   fit = function(formula, data, ...) {
#'     inla::inla(formula = formula, data = data,
#'                family = list(...)$family %||% "gaussian",
#'                control.compute = list(config = TRUE))
#'   },
#'   diagnostics = function(model, fixed_effects) {
#'     scores <- model$summary.fixed[, "sd"]
#'     names(scores) <- rownames(model$summary.fixed)
#'     scores[fixed_effects]
#'   }
#' )
#'
#' pruned <- modelPrune(y ~ x1 + x2 + x3, data = df,
#'                      engine = inla_engine, limit = 0.5)
#' }
#'
#' @importFrom stats as.formula lm model.matrix setNames terms
#' @export
modelPrune <- function(
  formula,
  data,
  engine     = "lm",
  criterion  = "vif",
  limit      = 5,
  force_in   = NULL,
  max_steps  = NULL,
  ...
) {

  # ===========================================================================
  # Input validation
  # ===========================================================================

  # Check formula
  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula object")
  }

  # Check data
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }

  # Check engine (character string for built-in, or list for custom)
  is_custom_engine <- FALSE

  if (is.character(engine)) {
    # Built-in engine
    if (length(engine) != 1L) {
      stop("Built-in 'engine' must be a single character string")
    }
    valid_engines <- c("lm", "glm", "lme4", "glmmTMB")
    if (!engine %in% valid_engines) {
      stop(sprintf(
        "Built-in 'engine' must be one of: %s\nFor custom engines, provide a list with 'fit' and 'diagnostics' components.",
        paste(valid_engines, collapse = ", ")
      ))
    }
  } else if (is.list(engine)) {
    # Custom engine
    is_custom_engine <- TRUE

    # Validate required fields
    required_fields <- c("fit", "diagnostics")
    missing <- setdiff(required_fields, names(engine))
    if (length(missing) > 0) {
      stop(sprintf(
        "Custom engine missing required fields: %s\nRequired: 'fit' and 'diagnostics'",
        paste(missing, collapse = ", ")
      ))
    }

    # Validate field types
    if (!is.function(engine$fit)) {
      stop("Custom engine field 'fit' must be a function with signature: function(formula, data, ...)")
    }
    if (!is.function(engine$diagnostics)) {
      stop("Custom engine field 'diagnostics' must be a function with signature: function(model, fixed_effects)")
    }

    # Optional name field
    if (is.null(engine$name)) {
      engine$name <- "custom"
    } else if (!is.character(engine$name) || length(engine$name) != 1L) {
      stop("Custom engine field 'name' must be a single character string")
    }
  } else {
    stop("'engine' must be either:\n  - A character string ('lm', 'glm', 'lme4', 'glmmTMB')\n  - A list with 'fit' and 'diagnostics' functions")
  }

  # Check criterion (only for built-in engines)
  valid_criteria <- c("vif", "condition_number")
  if (!is_custom_engine) {
    if (!is.character(criterion) || length(criterion) != 1L) {
      stop("'criterion' must be a single character string")
    }
    if (!criterion %in% valid_criteria) {
      stop(sprintf("For built-in engines, criterion must be one of: %s",
                   paste(valid_criteria, collapse = ", ")))
    }
  } else {
    # For custom engines, criterion is ignored (diagnostics come from engine)
    if (!missing(criterion) && criterion != "vif") {
      message("Note: 'criterion' parameter is ignored for custom engines (diagnostics computed by engine$diagnostics)")
    }
  }

  # Check limit
  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit)) {
    if (length(limit) == 1L && is.na(limit)) {
      stop("'limit' must be positive and non-missing")
    }
    stop("'limit' must be a single numeric value")
  }
  if (limit <= 0) {
    stop("'limit' must be positive and non-missing")
  }

  # Check force_in
  if (!is.null(force_in)) {
    if (!is.character(force_in)) {
      stop("'force_in' must be a character vector of variable names")
    }
  }

  # Check max_steps
  if (!is.null(max_steps)) {
    if (!is.numeric(max_steps) || length(max_steps) != 1L) {
      stop("'max_steps' must be a single numeric value or NULL")
    }
    if (is.na(max_steps) || max_steps < 1) {
      stop("'max_steps' must be >= 1")
    }
  }

  # ===========================================================================
  # Engine availability check (built-in engines only)
  # ===========================================================================

  if (!is_custom_engine) {
    if (engine == "lme4") {
      if (!requireNamespace("lme4", quietly = TRUE)) {
        stop("Engine 'lme4' requires the lme4 package to be installed.\n",
             "Install it with: install.packages('lme4')")
      }
    }

    if (engine == "glmmTMB") {
      if (!requireNamespace("glmmTMB", quietly = TRUE)) {
        stop("Engine 'glmmTMB' requires the glmmTMB package to be installed.\n",
             "Install it with: install.packages('glmmTMB')")
      }
    }
  }

  # ===========================================================================
  # Parse formula and extract fixed effects
  # ===========================================================================

  parsed <- .parse_formula(formula, data)
  response_var <- parsed$response
  response_vars <- parsed$response_vars
  fixed_effects <- parsed$fixed_effects
  has_random <- parsed$has_random

  # A formula with zero fixed-effect terms (e.g. `y ~ (1|group)`, valid
  # lme4/glmmTMB syntax) has nothing for VIF/condition-number pruning to act
  # on; catch this here with a modelPrune-specific message rather than
  # letting it fall through to .rebuild_formula()'s generic internal error
  # on the first fit attempt.
  if (length(fixed_effects) == 0L) {
    stop("'formula' has no fixed-effect predictors to prune. ",
         "modelPrune() requires at least one fixed effect (random-effect-only formulas, ",
         "e.g. `y ~ (1|group)`, are not supported).")
  }

  # Validate force_in against fixed effects
  if (!is.null(force_in)) {
    missing_vars <- setdiff(force_in, fixed_effects)
    if (length(missing_vars) > 0) {
      stop(sprintf(
        "'force_in' variable(s) not found in fixed effects: %s",
        paste(missing_vars, collapse = ", ")
      ))
    }
  }

  # ===========================================================================
  # Initial model fit
  # ===========================================================================

  current_fixed <- fixed_effects
  current_formula <- .rebuild_formula(response_var, current_fixed, parsed$random_effects)

  fit_result <- .fit_model(current_formula, data, engine, ...)
  model <- fit_result$model

  # ===========================================================================
  # Compute initial diagnostics
  # ===========================================================================

  diagnostics <- .compute_diagnostics(model, engine, criterion, current_fixed)

  # ===========================================================================
  # Feasibility check for force_in
  # ===========================================================================

  if (!is.null(force_in)) {
    force_in_diags <- diagnostics[force_in]

    # An undefined diagnostic (e.g. a predictor whose design-matrix columns
    # couldn't be identified) is not a "no violation" -- surface it explicitly
    # rather than letting it fall through to which.max()/sprintf() on an
    # all-NA vector, which previously raised a blank, contentless error.
    na_vars <- force_in[is.na(force_in_diags)]
    if (length(na_vars) > 0) {
      stop(sprintf(
        "The '%s' diagnostic is undefined for 'force_in' variable(s): %s.",
        criterion, paste(na_vars, collapse = ", ")
      ))
    }

    violations <- force_in_diags[force_in_diags > limit]

    if (length(violations) > 0) {
      worst_var <- names(violations)[which.max(violations)]
      worst_val <- violations[worst_var]

      stop(sprintf(
        "Variables in 'force_in' violate the criterion threshold.\nExample: '%s' has %s = %.2f > %.2f",
        worst_var, criterion, worst_val, limit
      ))
    }
  }

  # ===========================================================================
  # Pruning loop
  # ===========================================================================

  removed_vars <- character(0)
  step <- 0

  while (TRUE) {
    # Check stopping conditions
    if (!is.null(max_steps) && step >= max_steps) {
      warning(sprintf("Reached max_steps = %d. Some diagnostics may still exceed limit.", max_steps))
      break
    }

    # Find violations (exclude NA and Inf values)
    valid_diagnostics <- diagnostics[!is.na(diagnostics) & is.finite(diagnostics)]
    violations <- valid_diagnostics[valid_diagnostics > limit]

    if (length(violations) == 0) {
      # Check if there are any NA or Inf diagnostics that we need to handle
      invalid_diagnostics <- diagnostics[is.na(diagnostics) | is.infinite(diagnostics)]
      if (length(invalid_diagnostics) > 0) {
        # Remove variables with invalid diagnostics
        violations <- invalid_diagnostics
      } else {
        # All diagnostics satisfy threshold
        break
      }
    }

    # Identify removable variables (exclude force_in)
    removable <- setdiff(names(violations), force_in)

    if (length(removable) == 0) {
      # Check if we're trying to remove everything (all variables have NA/Inf)
      if (all(is.na(diagnostics) | is.infinite(diagnostics))) {
        warning("All diagnostic values are NA or Inf. Cannot compute VIF properly. Stopping pruning.")
        break
      }
      stop("Cannot satisfy criterion: only force_in variables remain and they violate the threshold")
    }

    # If we would remove all variables, stop instead
    if (length(current_fixed) - 1 < 1) {
      warning("Pruning would remove all predictors. Stopping with current set.")
      break
    }

    # Select worst variable (highest diagnostic value)
    # Tie-breaking: choose last in formula order
    # Handle Inf and NA by treating them as worse than any finite value
    removable_vals <- violations[removable]
    finite_vals <- removable_vals[is.finite(removable_vals)]

    if (length(finite_vals) == 0) {
      # All candidates are NA/Inf (e.g. a constant predictor's undefined
      # VIF) -- select any of them (prefer last in formula order) directly,
      # rather than calling max(na.rm = TRUE) on a vector that is entirely
      # NA/Inf, which raises base R's own "no non-missing arguments" warning.
      inf_or_na <- removable[is.infinite(removable_vals) | is.na(removable_vals)]
      worst_var <- inf_or_na[length(inf_or_na)]
    } else {
      worst_val <- max(finite_vals)
      candidates <- removable[!is.na(removable_vals) & removable_vals == worst_val]
      worst_var <- candidates[length(candidates)]  # Last in order
    }

    # Remove worst variable
    current_fixed <- setdiff(current_fixed, worst_var)
    removed_vars <- c(removed_vars, worst_var)

    # Refit model
    current_formula <- .rebuild_formula(response_var, current_fixed, parsed$random_effects)
    fit_result <- .fit_model(current_formula, data, engine, ...)
    model <- fit_result$model

    # Recompute diagnostics
    diagnostics <- .compute_diagnostics(model, engine, criterion, current_fixed)

    step <- step + 1
  }

  # ===========================================================================
  # Final output
  # ===========================================================================

  # Extract relevant columns from data. Use the bare variable name(s)
  # referenced by the response (not `response_var`, which may be a full
  # expression like "log(mpg)" and is not itself a column of `data`) and,
  # analogously, the bare variable names referenced by each surviving fixed
  # term (not `current_fixed` itself, which holds raw term labels like
  # "poly(disp, 2)" or "cyl:disp" and is not itself a column of `data`).
  fixed_vars <- unique(unlist(lapply(current_fixed, function(term) all.vars(str2lang(term)))))
  all_vars <- unique(c(response_vars, fixed_vars))
  data_pruned <- data[, all_vars, drop = FALSE]

  # Add attributes
  attr(data_pruned, "selected_vars") <- current_fixed
  attr(data_pruned, "removed_vars") <- removed_vars
  attr(data_pruned, "engine") <- if (is_custom_engine) engine$name else engine
  attr(data_pruned, "criterion") <- if (is_custom_engine) "custom" else criterion
  attr(data_pruned, "limit") <- limit
  attr(data_pruned, "final_model") <- model
  attr(data_pruned, "n_vars_original") <- length(fixed_effects)
  attr(data_pruned, "n_vars_selected") <- length(current_fixed)

  return(data_pruned)
}

# ===========================================================================
# Helper functions
# ===========================================================================

#' Parse formula to extract fixed and random effects
#' @noRd
.parse_formula <- function(formula, data) {
  # Extract terms
  terms_obj <- terms(formula, data = data)

  # Get response variable. Keep the full expression (e.g. "log(mpg)") as a
  # string for rebuilding the formula -- as.character() on a call node (any
  # transformed response) returns a multi-element vector of its parts (e.g.
  # c("log", "mpg")), silently corrupting downstream formula construction.
  # Separately record the bare variable name(s) actually referenced (e.g.
  # "mpg", or both sides of "y1/y2 ~ .") for extracting columns from `data`.
  response <- paste(deparse(formula[[2]]), collapse = " ")
  response_vars <- all.vars(formula[[2]])

  # Get all term labels
  all_terms <- attr(terms_obj, "term.labels")

  # Separate random effects from fixed effects
  # Random effects contain "|" character
  is_random <- grepl("\\|", all_terms)
  random_effects <- all_terms[is_random]
  fixed_effects <- all_terms[!is_random]

  list(
    response = response,
    response_vars = response_vars,
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    has_random = length(random_effects) > 0
  )
}

#' Rebuild formula from components
#' @noRd
.rebuild_formula <- function(response, fixed_effects, random_effects = NULL) {
  if (length(fixed_effects) == 0) {
    stop("Cannot build formula with no fixed effects")
  }

  # Build right-hand side
  rhs <- paste(fixed_effects, collapse = " + ")

  if (!is.null(random_effects) && length(random_effects) > 0) {
    # `random_effects` comes from .parse_formula()'s term.labels extraction,
    # which strips the parens `terms()` treats as pure grouping (e.g.
    # "(1 | group)" becomes "1 | group"). Re-wrapping each term in parens is
    # required, not cosmetic: `|` has lower precedence than `+`, so pasting
    # unparenthesized bar terms back onto the fixed-effect side changes what
    # they mean to lme4/glmmTMB's formula parser (a single "1 | group" term
    # happens to still parse close enough to fit without erroring, silently
    # turning a random intercept into an unintended random-slopes model; two
    # or more terms, e.g. "1 | subject" and "1 | site", collide outright and
    # lme4 raises "model frame and formula mismatch in model.matrix()"). See
    # #102.
    rhs <- paste(c(rhs, paste0("(", random_effects, ")")), collapse = " + ")
  }

  # Build formula
  as.formula(paste(response, "~", rhs))
}

#' Fit model using specified engine
#' @noRd
.fit_model <- function(formula, data, engine, ...) {
  # Determine if custom engine
  is_custom <- is.list(engine)
  engine_name <- if (is_custom) engine$name else engine

  model <- tryCatch({
    if (is_custom) {
      # Custom engine: call user-provided fit function
      engine$fit(formula, data, ...)
    } else {
      # Built-in engine
      switch(engine,
        lm = stats::lm(formula, data = data, ...),
        glm = stats::glm(formula, data = data, ...),
        lme4 = .fit_lme4(formula, data, ...),
        glmmTMB = glmmTMB::glmmTMB(formula, data = data, ...)
      )
    }
  }, error = function(e) {
    stop(sprintf("Model fitting with engine '%s' failed: %s", engine_name, e$message))
  })

  list(model = model)
}

#' Fit lme4 model (lmer or glmer based on family argument)
#' @noRd
.fit_lme4 <- function(formula, data, ...) {
  dots <- list(...)

  if ("family" %in% names(dots)) {
    # Use glmer for GLMMs
    lme4::glmer(formula, data = data, ...)
  } else {
    # Use lmer for LMMs
    lme4::lmer(formula, data = data, ...)
  }
}

#' Compute diagnostics for fixed effects
#' @noRd
.compute_diagnostics <- function(model, engine, criterion, fixed_effects) {
  # Determine if custom engine
  is_custom <- is.list(engine)

  if (is_custom) {
    # Custom engine: use user-provided diagnostics function
    engine_name <- engine$name

    diagnostics <- tryCatch({
      engine$diagnostics(model, fixed_effects)
    }, error = function(e) {
      stop(sprintf("Custom engine '%s' diagnostics computation failed: %s",
                   engine_name, e$message))
    })

    # Validate output
    if (!is.numeric(diagnostics)) {
      stop(sprintf(
        "Custom engine '%s' diagnostics function must return a numeric vector, got: %s",
        engine_name, class(diagnostics)[1]
      ))
    }

    # Check names before length (more specific error)
    if (is.null(names(diagnostics))) {
      # If no names and wrong length, report length error
      if (length(diagnostics) != length(fixed_effects)) {
        stop(sprintf(
          "Custom engine '%s' diagnostics function must return exactly %d value(s) (one per fixed effect), got %d",
          engine_name, length(fixed_effects), length(diagnostics)
        ))
      }
      # Has correct length but no names
      stop(sprintf(
        "Custom engine '%s' diagnostics function must return a named vector with names matching fixed effects",
        engine_name
      ))
    }

    # Has names - check they match
    missing_names <- setdiff(fixed_effects, names(diagnostics))
    if (length(missing_names) > 0) {
      stop(sprintf(
        "Custom engine '%s' diagnostics output missing names for: %s",
        engine_name, paste(missing_names, collapse = ", ")
      ))
    }

    # Check length last (after name validation)
    if (length(diagnostics) != length(fixed_effects)) {
      stop(sprintf(
        "Custom engine '%s' diagnostics function must return exactly %d value(s) (one per fixed effect), got %d",
        engine_name, length(fixed_effects), length(diagnostics)
      ))
    }

    # Return in correct order
    diagnostics[fixed_effects]

  } else {
    # Built-in engine
    if (criterion == "vif") {
      .compute_vif(model, engine, fixed_effects)
    } else if (criterion == "condition_number") {
      # Condition number for each variable's contribution to multicollinearity
      # Uses eigenvalue decomposition of scaled design matrix
      .compute_condition_indices(model, engine, fixed_effects)
    } else {
      stop(sprintf("Built-in criterion '%s' not implemented", criterion))
    }
  }
}

#' Flags design-matrix columns that are constant (zero variance). A constant
#' column makes both VIF's surrogate R^2 (0/0 in exact arithmetic) and
#' condition_number's `scale()` step (division by a zero SD) produce
#' floating-point noise instead of the documented undefined (NA) result, so
#' both diagnostics need to detect and gate on this before scoring.
#' @noRd
.zero_variance_cols <- function(X) {
  tol <- sqrt(.Machine$double.eps)
  apply(X, 2, function(col) {
    rng <- range(col)
    (rng[2] - rng[1]) < tol * max(1, abs(mean(col)))
  })
}

#' Compute VIF for fixed effects
#' @noRd
.compute_vif <- function(model, engine, fixed_effects) {
  # Determine engine string (handle both string and list inputs)
  engine_str <- if (is.list(engine)) "custom" else engine

  # Extract fixed-effects design matrix (intercept already removed, with
  # `assign` kept aligned to the remaining columns)
  X <- .extract_design_matrix(model, engine_str)

  if (ncol(X) == 0) {
    warning(sprintf(
      "Design matrix has no columns after removing intercept. Engine: %s, Fixed effects: %s",
      engine_str, paste(fixed_effects, collapse = ", ")
    ))
    return(setNames(rep(NA, length(fixed_effects)), fixed_effects))
  }

  # Handle edge case: single predictor
  if (ncol(X) == 1) {
    return(setNames(1.0, fixed_effects))
  }

  # A non-finite entry (Inf/-Inf/NaN) anywhere in the design matrix means the
  # model/data is corrupted, not merely collinear -- stats::cor()/det() would
  # silently propagate it into NaN correlations rather than erroring, which
  # would be indistinguishable from genuine perfect collinearity (also a
  # NaN-determinant case). Surface it explicitly as a computation failure
  # instead, matching the tryCatch error path below.
  if (!all(is.finite(X))) {
    warning("VIF computation failed: design matrix contains non-finite values (Inf/-Inf/NaN).")
    return(setNames(rep(NA, length(fixed_effects)), fixed_effects))
  }

  zero_var <- .zero_variance_cols(X)

  # Generalized VIF (Fox & Monette 1992): for a term occupying columns `idx`
  # among all p non-constant design columns, GVIF = det(R[idx,idx]) *
  # det(R[-idx,-idx]) / det(R), where R is the correlation matrix of all
  # non-constant columns. This reduces to the classic 1/(1-R^2) formula when
  # `idx` is a single column, and -- unlike averaging a multi-column term's
  # dummy columns into one vector -- correctly captures collinearity for
  # multi-level factors, matching car::vif()'s GVIF.
  nonconst_cols <- colnames(X)[!zero_var]
  R_full <- if (length(nonconst_cols) >= 2) stats::cor(X[, nonconst_cols, drop = FALSE]) else NULL

  .map_fixed_effects_by_columns(
    X, model, engine_str, fixed_effects,
    score_fn = function(pred, matching_cols) {
      # A constant predictor has an undefined VIF -- return NA directly
      # rather than letting the determinant ratio below land on
      # floating-point noise close to (but not exactly) 0/0.
      if (any(zero_var[matching_cols])) return(NA)

      other_cols <- setdiff(nonconst_cols, matching_cols)
      if (length(other_cols) == 0) return(1.0)  # No other predictors

      tryCatch({
        det_own   <- det(R_full[matching_cols, matching_cols, drop = FALSE])
        det_other <- det(R_full[other_cols, other_cols, drop = FALSE])
        det_full  <- det(R_full)
        gvif <- (det_own * det_other) / det_full

        if (is.na(gvif) || !is.finite(gvif) || gvif < 0) Inf else gvif
      }, error = function(e) {
        warning(sprintf("VIF computation failed for '%s': %s", pred, e$message))
        NA
      })
    },
    on_missing = function(pred) {
      avail_cols <- paste(colnames(X), collapse = ", ")
      warning(sprintf(
        "Could not find columns for predictor '%s' in design matrix. Available columns: %s",
        pred, avail_cols
      ))
      NA
    }
  )
}

#' Resolves each fixed effect to its design-matrix columns (via
#' .design_columns_for_predictor()) and applies `score_fn(pred,
#' matching_cols)` to compute its diagnostic value, or `on_missing(pred)`
#' when no columns resolve. Shared by .compute_vif() and
#' .compute_condition_indices(), which differ only in how they turn a
#' predictor's resolved columns into a single diagnostic number.
#' @noRd
.map_fixed_effects_by_columns <- function(X, model, engine_str, fixed_effects, score_fn, on_missing) {
  term_labels <- attr(.terms_for_engine(model, engine_str), "term.labels")
  assign_vec <- attr(X, "assign")

  result <- numeric(length(fixed_effects))
  names(result) <- fixed_effects

  for (i in seq_along(fixed_effects)) {
    pred <- fixed_effects[i]
    matching_cols <- .design_columns_for_predictor(X, term_labels, assign_vec, pred)

    result[i] <- if (length(matching_cols) == 0) {
      on_missing(pred)
    } else {
      score_fn(pred, matching_cols)
    }
  }

  result
}

#' Terms object used to build a fitted model's design matrix, per engine, so
#' design-matrix columns can be mapped back to fixed-effect terms via
#' `attr(X, "assign")` instead of matching on column name strings.
#' @noRd
.terms_for_engine <- function(model, engine) {
  switch(engine,
    glmmTMB = stats::terms(model$modelInfo$terms$cond$fixed),
    stats::terms(model)
  )
}

#' Design-matrix columns belonging to a fixed-effect term, resolved via
#' `attr(X, "assign")` (which `stats::model.matrix()` sets to the 1-based
#' position of each column's originating term in `term_labels`). This is
#' exact regardless of term name collisions -- unlike a name-prefix match
#' (e.g. `grep("^x1", ...)`), which also matches an unrelated "x10" column.
#' @noRd
.design_columns_for_predictor <- function(X, term_labels, assign_vec, pred) {
  term_idx <- match(pred, term_labels)
  if (is.na(term_idx)) return(character(0))
  colnames(X)[assign_vec == term_idx]
}

#' Extract fixed-effects design matrix from model
#' @noRd
.extract_design_matrix <- function(model, engine) {
  X <- switch(engine,
    lm = stats::model.matrix(model),
    glm = stats::model.matrix(model),
    lme4 = {
      # Use stats::model.matrix instead of lme4::getME
      # getME can have issues with model subsetting
      stats::model.matrix(model)
    },
    glmmTMB = {
      # For glmmTMB, extract conditional component design matrix
      stats::model.matrix(model$modelInfo$terms$cond$fixed, model$frame)
    },
    # Default case for unrecognized engines
    {
      warning(sprintf("Unknown engine '%s' in .extract_design_matrix, using stats::model.matrix", engine))
      stats::model.matrix(model)
    }
  )

  # Check if X is NULL (switch returned NULL for unmatched case)
  if (is.null(X)) {
    stop(sprintf("Design matrix extraction returned NULL for engine '%s'", engine))
  }

  # Ensure column names exist (some methods may return unnamed matrices)
  if (is.null(colnames(X))) {
    colnames(X) <- paste0("V", seq_len(ncol(X)))
  }

  # Drop the intercept column (if present) while keeping `assign` aligned with
  # the remaining columns. Plain `[` subsetting on a matrix silently drops
  # custom attributes like "assign", so it must be subset in parallel here --
  # not left to callers, who would otherwise each need to remember to do this.
  assign_vec <- attr(X, "assign")
  if ("(Intercept)" %in% colnames(X)) {
    keep <- colnames(X) != "(Intercept)"
    if (!is.null(assign_vec)) assign_vec <- assign_vec[keep]
    X <- X[, keep, drop = FALSE]
  }
  attr(X, "assign") <- assign_vec

  X
}

#' Compute condition indices for fixed effects
#' @noRd
.compute_condition_indices <- function(model, engine, fixed_effects) {
  # Determine engine string (handle both string and list inputs)
  engine_str <- if (is.list(engine)) "custom" else engine

  # Extract fixed-effects design matrix (intercept already removed, with
  # `assign` kept aligned to the remaining columns)
  X <- .extract_design_matrix(model, engine_str)

  # Check if X became empty
  if (ncol(X) == 0) {
    warning("Design matrix has no columns after removing intercept.")
    return(setNames(rep(NA, length(fixed_effects)), fixed_effects))
  }

  # Handle edge case: single predictor
  if (ncol(X) == 1) {
    return(setNames(1.0, fixed_effects))
  }

  # Constant columns give `scale()` a zero SD to divide by, producing an
  # all-NaN column that fails `svd()` -- as a fallback that used to return
  # Inf for *every* predictor, not just the constant one. Exclude them from
  # the SVD input so the remaining, well-behaved predictors still get a real
  # condition index; the constant predictor itself is scored NA below,
  # matching VIF's documented undefined-for-constant-predictor contract.
  zero_var <- .zero_variance_cols(X)
  X_use <- X[, !zero_var, drop = FALSE]

  condition_indices <- NULL
  if (ncol(X_use) == 1) {
    condition_indices <- setNames(1.0, colnames(X_use))
  } else if (ncol(X_use) >= 2) {
    # Scale columns to unit length (required for proper condition number)
    X_scaled <- scale(X_use, center = TRUE, scale = TRUE)

    svd_result <- tryCatch(svd(X_scaled), error = function(e) NULL)

    if (is.null(svd_result)) {
      warning("SVD failed for design matrix.")
    } else {
      d <- svd_result$d
      max_sv <- max(d)
      condition_indices <- setNames(max_sv / d, colnames(X_use))
    }
  }

  # Map condition indices back to fixed effects: use the maximum condition
  # index across the columns associated with each effect (an approximation --
  # see ?modelPrune's Details for why a full per-factor decomposition isn't
  # done here).
  .map_fixed_effects_by_columns(
    X, model, engine_str, fixed_effects,
    score_fn = function(pred, matching_cols) {
      if (any(zero_var[matching_cols])) return(NA)
      if (is.null(condition_indices)) return(Inf)  # SVD failed on the non-constant columns

      col_indices <- which(colnames(X_use) %in% matching_cols)
      if (length(col_indices) > 0) {
        max(condition_indices[col_indices])
      } else {
        max(condition_indices)  # Fallback: use overall condition number
      }
    },
    on_missing = function(pred) NA
  )
}
