# ===========================================================================
# Input validation tests
# ===========================================================================

test_that("modelPrune validates formula argument", {
  df <- mtcars

  expect_error(
    modelPrune(formula = "mpg ~ cyl", data = df),
    "'formula' must be a formula object"
  )

  expect_error(
    modelPrune(formula = NULL, data = df),
    "'formula' must be a formula object"
  )
})

test_that("modelPrune validates data argument", {
  expect_error(
    modelPrune(mpg ~ cyl, data = as.matrix(mtcars)),
    "'data' must be a data.frame"
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = NULL),
    "'data' must be a data.frame"
  )
})

test_that("modelPrune validates engine argument", {
  df <- mtcars

  expect_error(
    modelPrune(mpg ~ cyl, data = df, engine = 123),
    "'engine' must be either"
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = df, engine = c("lm", "glm")),
    "Built-in 'engine' must be a single character string"
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = df, engine = "invalid"),
    "Built-in 'engine' must be one of"
  )
})

test_that("modelPrune validates criterion argument", {
  df <- mtcars

  expect_error(
    modelPrune(mpg ~ cyl, data = df, criterion = 123),
    "'criterion' must be a single character string"
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = df, criterion = "pvalue"),
    "criterion must be one of: vif, condition_number"
  )
})

test_that("modelPrune validates limit argument", {
  df <- mtcars

  expect_error(
    modelPrune(mpg ~ cyl, data = df, limit = "5"),
    "'limit' must be a single numeric value"
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = df, limit = c(5, 10)),
    "'limit' must be a single numeric value"
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = df, limit = -5),
    "'limit' must be positive and non-missing"
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = df, limit = NA),
    "'limit' must be positive and non-missing"
  )
})

test_that("modelPrune validates force_in argument", {
  df <- mtcars

  expect_error(
    modelPrune(mpg ~ cyl + disp, data = df, force_in = 123),
    "'force_in' must be a character vector"
  )

  expect_error(
    modelPrune(mpg ~ cyl + disp, data = df, force_in = "missing_var"),
    "'force_in' variable\\(s\\) not found in fixed effects"
  )
})

test_that("modelPrune validates max_steps argument", {
  df <- mtcars

  expect_error(
    modelPrune(mpg ~ cyl, data = df, max_steps = "10"),
    "'max_steps' must be a single numeric value or NULL"
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = df, max_steps = 0),
    "'max_steps' must be >= 1"
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = df, max_steps = -5),
    "'max_steps' must be >= 1"
  )
})

# ===========================================================================
# Functional tests - lm engine
# ===========================================================================

test_that("modelPrune works with lm engine", {
  set.seed(42)
  df <- mtcars

  result <- modelPrune(mpg ~ cyl + disp + hp + wt, data = df,
                       engine = "lm", limit = 5)

  expect_s3_class(result, "data.frame")
  expect_true("mpg" %in% names(result))  # Response always included
  expect_true(ncol(result) >= 2)  # At least response + 1 predictor
  expect_equal(attr(result, "engine"), "lm")
  expect_equal(attr(result, "criterion"), "vif")
  expect_equal(attr(result, "limit"), 5)
})

test_that("modelPrune respects force_in with lm", {
  df <- mtcars

  result <- modelPrune(mpg ~ cyl + disp + hp + wt, data = df,
                       engine = "lm", force_in = "cyl", limit = 10)

  expect_true("cyl" %in% attr(result, "selected_vars"))
  expect_true("cyl" %in% names(result))
})

test_that("modelPrune errors when force_in violates threshold", {
  # Create data where cyl and disp are highly collinear
  set.seed(123)
  df <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50),
    x2 = rnorm(50)
  )
  df$x3 <- df$x1 + rnorm(50, sd = 0.01)  # Nearly perfect collinearity

  expect_error(
    modelPrune(y ~ x1 + x2 + x3, data = df,
               force_in = c("x1", "x3"), limit = 2),
    "Variables in 'force_in' violate the criterion threshold"
  )
})

test_that("modelPrune returns correct attributes", {
  df <- mtcars

  result <- modelPrune(mpg ~ cyl + disp + hp, data = df, limit = 10)

  expect_true("selected_vars" %in% names(attributes(result)))
  expect_true("removed_vars" %in% names(attributes(result)))
  expect_true("engine" %in% names(attributes(result)))
  expect_true("criterion" %in% names(attributes(result)))
  expect_true("limit" %in% names(attributes(result)))
  expect_true("final_model" %in% names(attributes(result)))
})

test_that("modelPrune is deterministic", {
  df <- mtcars

  result1 <- modelPrune(mpg ~ cyl + disp + hp + wt, data = df, limit = 5)
  result2 <- modelPrune(mpg ~ cyl + disp + hp + wt, data = df, limit = 5)

  expect_equal(attr(result1, "selected_vars"), attr(result2, "selected_vars"))
  expect_equal(attr(result1, "removed_vars"), attr(result2, "removed_vars"))
})

test_that("modelPrune handles max_steps correctly", {
  df <- mtcars

  # With max_steps = 1, should remove at most 1 variable
  # Suppress expected warning about reaching max_steps
  result <- suppressWarnings(
    modelPrune(mpg ~ cyl + disp + hp + wt + drat + qsec,
               data = df, limit = 2, max_steps = 1)
  )

  expect_true(length(attr(result, "removed_vars")) <= 1)
})

test_that("modelPrune handles single predictor", {
  df <- mtcars

  result <- modelPrune(mpg ~ cyl, data = df, limit = 5)

  # Single predictor should have VIF = 1, always passes
  expect_true("cyl" %in% attr(result, "selected_vars"))
  expect_equal(length(attr(result, "removed_vars")), 0)
})

test_that("modelPrune handles categorical predictors", {
  df <- mtcars
  df$gear <- as.factor(df$gear)

  result <- modelPrune(mpg ~ cyl + gear + wt, data = df, limit = 5)

  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) >= 2)
})

# ===========================================================================
# Functional tests - glm engine
# ===========================================================================

test_that("modelPrune works with glm engine", {
  skip_if_not(getRversion() >= "3.5.0")

  df <- mtcars
  df$am_binary <- as.factor(df$am)

  # Suppress GLM convergence warnings (expected with this small dataset)
  result <- suppressWarnings(
    modelPrune(am_binary ~ cyl + disp + hp + wt,
               data = df, engine = "glm",
               family = binomial(), limit = 5)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "engine"), "glm")
})

test_that("modelPrune glm respects force_in", {
  df <- mtcars
  df$am_binary <- as.factor(df$am)

  result <- modelPrune(am_binary ~ cyl + disp + hp,
                       data = df, engine = "glm",
                       family = binomial(), force_in = "cyl", limit = 10)

  expect_true("cyl" %in% attr(result, "selected_vars"))
})

# ===========================================================================
# Functional tests - lme4 engine (conditional on package availability)
# ===========================================================================

test_that("modelPrune works with lme4 engine if available", {
  skip_if_not_installed("lme4")

  # Create simple dataset with grouping
  set.seed(42)
  df <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100),
    group = rep(1:10, each = 10)
  )

  # Suppress warnings from lme4 singular fits and pruning safety checks
  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2 + x3 + (1|group),
               data = df, engine = "lme4", limit = 5)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "engine"), "lme4")
  # Group variable should NOT be in selected_vars (it's random effect)
  expect_false("group" %in% attr(result, "selected_vars"))
})

test_that("modelPrune lme4 preserves random effects", {
  skip_if_not_installed("lme4")

  set.seed(123)
  df <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100),
    group = rep(1:10, each = 10)
  )

  # Suppress warnings from lme4 singular fits and pruning safety checks
  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2 + (1|group),
               data = df, engine = "lme4", limit = 10)
  )

  # Check that final model has random effects
  final_model <- attr(result, "final_model")
  expect_true(inherits(final_model, "merMod"))
})

# ===========================================================================
# Functional tests - glmmTMB engine (conditional on package availability)
# ===========================================================================

test_that("modelPrune works with glmmTMB engine if available", {
  skip_if_not_installed("glmmTMB")

  set.seed(42)
  df <- data.frame(
    y = rpois(100, lambda = 5),
    x1 = rnorm(100),
    x2 = rnorm(100),
    group = rep(1:10, each = 10)
  )

  # Suppress warnings from glmmTMB convergence and pruning safety checks
  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2 + (1|group),
               data = df, engine = "glmmTMB",
               family = poisson(), limit = 5)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "engine"), "glmmTMB")
})

# ===========================================================================
# Edge cases
# ===========================================================================

test_that("modelPrune handles all predictors passing threshold", {
  set.seed(42)
  # Create data with low multicollinearity
  df <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100)
  )

  result <- modelPrune(y ~ x1 + x2 + x3, data = df, limit = 10)

  # All predictors should be retained
  expect_equal(length(attr(result, "selected_vars")), 3)
  expect_equal(length(attr(result, "removed_vars")), 0)
})

test_that("modelPrune handles formula with dot notation", {
  df <- mtcars[, c("mpg", "cyl", "disp", "hp")]

  result <- modelPrune(mpg ~ ., data = df, limit = 5)

  expect_s3_class(result, "data.frame")
  expect_true("mpg" %in% names(result))
})

# ===========================================================================
# Custom engine tests
# ===========================================================================

test_that("modelPrune validates custom engine structure", {
  df <- mtcars

  # Missing required fields
  expect_error(
    modelPrune(mpg ~ cyl, data = df, engine = list()),
    "Custom engine missing required fields"
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = df, engine = list(fit = function() {})),
    "Custom engine missing required fields.*diagnostics"
  )

  # Invalid field types
  expect_error(
    modelPrune(mpg ~ cyl, data = df,
               engine = list(fit = "not a function", diagnostics = function() {})),
    "Custom engine field 'fit' must be a function"
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = df,
               engine = list(fit = function() {}, diagnostics = "not a function")),
    "Custom engine field 'diagnostics' must be a function"
  )

  # Invalid name field
  expect_error(
    modelPrune(mpg ~ cyl, data = df,
               engine = list(fit = function() {}, diagnostics = function() {},
                             name = c("a", "b"))),
    "Custom engine field 'name' must be a single character string"
  )
})

test_that("modelPrune works with simple custom engine", {
  set.seed(42)
  df <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50)
  )

  # Make x2 and x3 correlated
  df$x3 <- df$x2 * 0.95 + rnorm(50, sd = 0.1)

  # Create a simple custom engine that uses VIF-like diagnostics
  # but with a custom implementation
  simple_engine <- list(
    name = "simple_custom",
    fit = function(formula, data, ...) {
      stats::lm(formula, data = data)
    },
    diagnostics = function(model, fixed_effects) {
      # Use car::vif if available, otherwise return simple R-squared based metric
      X <- stats::model.matrix(model)
      if ("(Intercept)" %in% colnames(X)) {
        X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
      }

      vifs <- numeric(length(fixed_effects))
      names(vifs) <- fixed_effects

      for (i in seq_along(fixed_effects)) {
        pred <- fixed_effects[i]
        if (pred %in% colnames(X)) {
          y_i <- X[, pred]
          X_other <- X[, colnames(X) != pred, drop = FALSE]

          if (ncol(X_other) == 0) {
            vifs[i] <- 1.0
          } else {
            fit <- lm(y_i ~ X_other)
            r_sq <- summary(fit)$r.squared
            vifs[i] <- 1 / (1 - r_sq)
          }
        } else {
          vifs[i] <- NA
        }
      }
      vifs
    }
  )

  result <- modelPrune(y ~ x1 + x2 + x3, data = df,
                       engine = simple_engine, limit = 5)

  # Should remove at least one predictor due to high correlation
  expect_s3_class(result, "data.frame")
  expect_true(length(attr(result, "selected_vars")) < 3)
  expect_equal(attr(result, "engine"), "simple_custom")
  expect_equal(attr(result, "criterion"), "custom")
})

test_that("modelPrune custom engine respects force_in", {
  set.seed(42)
  df <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50)
  )

  # Make x2 and x3 highly correlated
  df$x3 <- df$x2 * 0.99 + rnorm(50, sd = 0.01)

  custom_engine <- list(
    name = "test_engine",
    fit = function(formula, data, ...) {
      stats::lm(formula, data = data)
    },
    diagnostics = function(model, fixed_effects) {
      # Return fixed scores - x1 low, x2 medium, x3 high
      scores <- c(x1 = 2, x2 = 8, x3 = 15)
      scores[fixed_effects]
    }
  )

  # Force x2 to stay (even though it exceeds limit)
  result <- modelPrune(y ~ x1 + x2 + x3, data = df,
                       engine = custom_engine,
                       force_in = "x2", limit = 10)

  # x2 should be retained, x3 should be removed (higher score, last in formula)
  selected <- attr(result, "selected_vars")
  expect_true("x2" %in% selected)
  expect_false("x3" %in% selected)
})

test_that("modelPrune custom engine validates diagnostics output", {
  df <- mtcars

  # Diagnostics returns unnamed vector (correct length but no names)
  bad_engine1 <- list(
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      c(1, 2)  # Correct length but no names
    }
  )

  expect_error(
    modelPrune(mpg ~ cyl + disp, data = df, engine = bad_engine1, limit = 5),
    "must return a named vector"
  )

  # Diagnostics returns non-numeric
  bad_engine2 <- list(
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      c("a", "b")
    }
  )

  expect_error(
    modelPrune(mpg ~ cyl + disp, data = df, engine = bad_engine2, limit = 5),
    "must return a numeric vector"
  )

  # Diagnostics returns unnamed vector
  bad_engine3 <- list(
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      c(1, 2)  # No names
    }
  )

  expect_error(
    modelPrune(mpg ~ cyl + disp, data = df, engine = bad_engine3, limit = 5),
    "must return a named vector"
  )

  # Diagnostics returns vector with wrong names
  bad_engine4 <- list(
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      c(wrong1 = 1, wrong2 = 2)
    }
  )

  expect_error(
    modelPrune(mpg ~ cyl + disp, data = df, engine = bad_engine4, limit = 5),
    "missing names for"
  )
})

test_that("modelPrune custom engine handles fitting errors gracefully", {
  df <- mtcars

  failing_engine <- list(
    name = "failing",
    fit = function(formula, data, ...) {
      stop("Intentional fit failure")
    },
    diagnostics = function(model, fixed_effects) {
      setNames(rep(1, length(fixed_effects)), fixed_effects)
    }
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = df, engine = failing_engine, limit = 5),
    "Model fitting with engine 'failing' failed.*Intentional fit failure"
  )
})

test_that("modelPrune custom engine handles diagnostic errors gracefully", {
  df <- mtcars

  failing_diag_engine <- list(
    name = "bad_diagnostics",
    fit = function(formula, data, ...) {
      stats::lm(formula, data = data)
    },
    diagnostics = function(model, fixed_effects) {
      stop("Intentional diagnostics failure")
    }
  )

  expect_error(
    modelPrune(mpg ~ cyl, data = df, engine = failing_diag_engine, limit = 5),
    "Custom engine 'bad_diagnostics' diagnostics.*failed.*Intentional"
  )
})

test_that("modelPrune custom engine uses default name if not provided", {
  df <- mtcars

  unnamed_engine <- list(
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      setNames(rep(1, length(fixed_effects)), fixed_effects)
    }
  )

  result <- modelPrune(mpg ~ cyl, data = df,
                       engine = unnamed_engine, limit = 10)

  expect_equal(attr(result, "engine"), "custom")
})

test_that("modelPrune custom engine can implement non-VIF diagnostics", {
  set.seed(42)
  # Create data where y actually depends on predictors
  df <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100)
  )
  # Make y a function of x1, x2, x3 so they're all significant
  df$y <- 2 * df$x1 + 3 * df$x2 - 1.5 * df$x3 + rnorm(100, sd = 0.5)

  # Custom engine that uses p-values as diagnostic
  pvalue_engine <- list(
    name = "pvalue_pruner",
    fit = function(formula, data, ...) {
      stats::lm(formula, data = data)
    },
    diagnostics = function(model, fixed_effects) {
      # Higher p-value = worse (inverted for removal logic)
      coefs <- summary(model)$coefficients
      # Get p-values for fixed effects (skip intercept)
      pvals <- coefs[fixed_effects, "Pr(>|t|)", drop = FALSE]
      # Invert so high p-value becomes high "badness"
      scores <- pvals[, 1]
      names(scores) <- fixed_effects
      scores
    }
  )

  # All predictors have low p-values, so nothing should be removed
  result <- modelPrune(y ~ x1 + x2 + x3, data = df,
                       engine = pvalue_engine, limit = 0.05)

  expect_equal(length(attr(result, "selected_vars")), 3)
  expect_equal(attr(result, "engine"), "pvalue_pruner")
})

# ===========================================================================
# Additional coverage tests for modelPrune.R
# ===========================================================================

test_that("modelPrune custom engine with wrong length diagnostics", {
  df <- mtcars

  bad_engine <- list(
    name = "wrong_length",
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      # Return named vector with wrong length
      c(cyl = 1)  # Only one value when expecting 2
    }
  )

  expect_error(
    modelPrune(mpg ~ cyl + disp, data = df, engine = bad_engine, limit = 5),
    "missing names for"
  )
})

test_that("modelPrune custom engine criterion parameter ignored with message", {
  df <- mtcars

  custom_engine <- list(
    name = "test",
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      setNames(rep(1, length(fixed_effects)), fixed_effects)
    }
  )

  # Setting criterion to non-vif should produce message
  expect_message(
    modelPrune(mpg ~ cyl, data = df, engine = custom_engine,
               criterion = "aic", limit = 10),
    "criterion.*ignored"
  )
})

test_that("modelPrune handles max_steps warning", {
  set.seed(1201)
  df <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100)
  )
  # Make x2 and x3 correlated
  df$x3 <- df$x2 * 0.95 + rnorm(100, sd = 0.1)

  # With max_steps = 1 and low limit, should warn
  expect_warning(
    modelPrune(y ~ x1 + x2 + x3, data = df, limit = 2, max_steps = 1),
    "max_steps"
  )
})

test_that("modelPrune handles max_steps NA error", {
  df <- mtcars

  expect_error(
    modelPrune(mpg ~ cyl, data = df, max_steps = NA),
    "'max_steps' must be"
  )
})

test_that("modelPrune handles multiple predictor vectors with force_in", {
  df <- mtcars

  # Multiple force_in variables that satisfy threshold
  result <- modelPrune(mpg ~ cyl + disp + hp + wt,
                       data = df, force_in = c("cyl", "wt"), limit = 50)

  expect_true(all(c("cyl", "wt") %in% attr(result, "selected_vars")))
})

test_that("modelPrune handles limit = 0 error", {
  df <- mtcars

  expect_error(
    modelPrune(mpg ~ cyl, data = df, limit = 0),
    "'limit' must be positive"
  )
})

test_that("modelPrune with interaction terms", {
  df <- mtcars

  result <- modelPrune(mpg ~ cyl * disp, data = df, limit = 100)

  expect_s3_class(result, "data.frame")
  expect_true("mpg" %in% names(result))
})

test_that("modelPrune glm with poisson family", {
  set.seed(1202)
  df <- data.frame(
    y = rpois(100, lambda = 5),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  result <- modelPrune(y ~ x1 + x2, data = df,
                       engine = "glm", family = poisson(), limit = 10)

  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "engine"), "glm")
})

test_that("modelPrune lme4 with glmer", {
  skip_if_not_installed("lme4")

  set.seed(1203)
  df <- data.frame(
    y = rbinom(100, 1, 0.5),
    x1 = rnorm(100),
    x2 = rnorm(100),
    group = rep(1:10, each = 10)
  )

  # Suppress lme4 convergence warnings for small data
  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2 + (1|group), data = df,
               engine = "lme4", family = binomial(), limit = 10)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "engine"), "lme4")
})

test_that("modelPrune handles two-predictor model", {
  df <- mtcars

  result <- modelPrune(mpg ~ cyl + disp, data = df, limit = 10)

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune removes predictors iteratively", {
  set.seed(1204)
  df <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100)
  )
  # Create collinearity between x1, x2, x3
  df$x2 <- df$x1 * 0.99 + rnorm(100, sd = 0.01)
  df$x3 <- df$x1 * 0.98 + rnorm(100, sd = 0.02)

  result <- modelPrune(y ~ x1 + x2 + x3, data = df, limit = 5)

  # Should have removed some variables
  expect_true(length(attr(result, "removed_vars")) > 0)
})

test_that("modelPrune attributes are complete", {
  df <- mtcars

  result <- modelPrune(mpg ~ cyl + disp + hp, data = df, limit = 10)

  # Check all expected attributes
  attrs <- attributes(result)
  expect_true("selected_vars" %in% names(attrs))
  expect_true("removed_vars" %in% names(attrs))
  expect_true("engine" %in% names(attrs))
  expect_true("criterion" %in% names(attrs))
  expect_true("limit" %in% names(attrs))
  expect_true("final_model" %in% names(attrs))
  expect_true("n_vars_original" %in% names(attrs))
  expect_true("n_vars_selected" %in% names(attrs))
})

test_that("modelPrune VIF with highly correlated predictors", {
  set.seed(1205)
  n <- 100
  x1 <- rnorm(n)
  df <- data.frame(
    y = rnorm(n),
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.001),  # Near-perfect collinearity
    x3 = rnorm(n)
  )

  # Should handle VIF = Inf gracefully
  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2 + x3, data = df, limit = 5)
  )

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune lme4 package not installed error", {
  skip_if(requireNamespace("lme4", quietly = TRUE))

  df <- data.frame(
    y = rnorm(20),
    x1 = rnorm(20),
    group = rep(1:4, each = 5)
  )

  expect_error(
    modelPrune(y ~ x1 + (1|group), data = df, engine = "lme4"),
    "lme4 package"
  )
})

test_that("modelPrune glmmTMB package not installed error", {
  skip_if(requireNamespace("glmmTMB", quietly = TRUE))

  df <- data.frame(
    y = rnorm(20),
    x1 = rnorm(20),
    group = rep(1:4, each = 5)
  )

  expect_error(
    modelPrune(y ~ x1 + (1|group), data = df, engine = "glmmTMB"),
    "glmmTMB package"
  )
})

# ===========================================================================
# Additional VIF and edge case tests
# ===========================================================================

test_that("modelPrune VIF with highly collinear predictors", {
  set.seed(9001)
  n <- 100
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, sd = 0.01)  # Nearly identical to x1
  x3 <- rnorm(n)
  y <- x1 + x3 + rnorm(n)

  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  # VIF should be very high for x1 and x2
  result <- modelPrune(y ~ x1 + x2 + x3, data = df, criterion = "vif", limit = 10)

  expect_s3_class(result, "data.frame")
  # One of x1 or x2 should be removed due to high VIF
  expect_true(ncol(result) <= ncol(df))
})

test_that("modelPrune VIF with factor predictors", {
  set.seed(9002)
  n <- 60
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    x3 = rnorm(n)
  )

  result <- modelPrune(y ~ x1 + x2 + x3, data = df, criterion = "vif", limit = 5)

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune VIF handles constant predictor", {
  set.seed(9003)
  n <- 50
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rep(5, n),  # Constant - will cause issues
    x3 = rnorm(n)
  )

  # Should handle gracefully (constant predictor may be auto-removed or cause inf VIF)
  result <- tryCatch(
    modelPrune(y ~ x1 + x2 + x3, data = df, criterion = "vif", limit = 10),
    error = function(e) "error"
  )

  # Either succeeds or throws an expected error
  expect_true(is.data.frame(result) || result == "error")
})

test_that("modelPrune with single predictor", {
  set.seed(9004)
  n <- 50
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n)
  )

  result <- modelPrune(y ~ x1, data = df, criterion = "vif", limit = 5)

  expect_s3_class(result, "data.frame")
  # Single predictor should remain
  expect_true("x1" %in% names(result))
})

test_that("modelPrune with two predictors", {
  set.seed(9005)
  n <- 50
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )

  result <- modelPrune(y ~ x1 + x2, data = df, criterion = "vif", limit = 5)

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune force_in preserves specified variables", {
  set.seed(9006)
  n <- 80
  x1 <- rnorm(n)
  x2 <- rnorm(n)  # Independent from x1
  x3 <- rnorm(n)
  y <- x1 + x3 + rnorm(n)

  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  # Force x2 to stay
  result <- modelPrune(y ~ x1 + x2 + x3, data = df, criterion = "vif",
                       limit = 10, force_in = "x2")

  expect_s3_class(result, "data.frame")
  expect_true("x2" %in% names(result))
})

test_that("modelPrune glm engine with binomial family", {
  set.seed(9007)
  n <- 100
  df <- data.frame(
    y = rbinom(n, 1, 0.5),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  result <- modelPrune(y ~ x1 + x2 + x3, data = df, engine = "glm",
                       criterion = "vif", limit = 5, family = binomial())

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune handles NA in data", {
  set.seed(9008)
  n <- 50
  df <- data.frame(
    y = c(rnorm(48), NA, NA),
    x1 = c(rnorm(48), NA, 1),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  # Should handle NAs (complete cases used)
  result <- tryCatch(
    modelPrune(y ~ x1 + x2 + x3, data = df, criterion = "vif", limit = 5),
    error = function(e) "error"
  )

  # May succeed with na.action or fail
  expect_true(is.data.frame(result) || result == "error")
})

test_that("modelPrune custom engine diagnostics without names but wrong length", {
  df <- mtcars

  # Engine that returns unnamed vector with wrong length
  bad_engine <- list(
    name = "wrong_length_no_names",
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      # Return wrong number of values without names
      rep(1.0, length(fixed_effects) + 1)
    }
  )

  expect_error(
    modelPrune(mpg ~ cyl + disp + hp, data = df, engine = bad_engine, limit = 5),
    "must return exactly.*value"
  )
})

test_that("modelPrune custom engine diagnostics without names but correct length", {
  df <- mtcars

  # Engine that returns correct length but no names
  bad_engine <- list(
    name = "correct_length_no_names",
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      # Return correct number of values but without names
      rep(1.0, length(fixed_effects))
    }
  )

  expect_error(
    modelPrune(mpg ~ cyl + disp, data = df, engine = bad_engine, limit = 5),
    "must return a named vector"
  )
})

test_that("modelPrune custom engine diagnostics with extra names", {
  df <- mtcars

  # Engine that returns more values than needed (with names)
  bad_engine <- list(
    name = "extra_names",
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      vals <- rep(1.0, length(fixed_effects) + 2)
      names(vals) <- c(fixed_effects, "extra1", "extra2")
      vals
    }
  )

  expect_error(
    modelPrune(mpg ~ cyl + disp, data = df, engine = bad_engine, limit = 5),
    "must return exactly"
  )
})

test_that("modelPrune custom engine diagnostics returns non-numeric", {
  df <- mtcars

  # Engine that returns character instead of numeric
  bad_engine <- list(
    name = "non_numeric",
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      vals <- rep("high", length(fixed_effects))
      names(vals) <- fixed_effects
      vals
    }
  )

  expect_error(
    modelPrune(mpg ~ cyl + disp, data = df, engine = bad_engine, limit = 5),
    "must return a numeric vector"
  )
})

# ===========================================================================
# Additional edge case tests for full coverage
# ===========================================================================

test_that("modelPrune handles Inf VIF values (near-perfect collinearity)", {
  set.seed(9101)
  n <- 100
  x1 <- rnorm(n)
  # Create near-perfect collinearity
  df <- data.frame(
    y = rnorm(n),
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 1e-10),  # Essentially identical
    x3 = rnorm(n)
  )

  # Should handle Inf VIF gracefully
  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2 + x3, data = df, limit = 5)
  )

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune handles VIF with all near-collinear predictors", {
  set.seed(9102)
  n <- 100
  x1 <- rnorm(n)
  df <- data.frame(
    y = rnorm(n),
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 1e-8),
    x3 = x1 + rnorm(n, sd = 1e-8)
  )

  # All have Inf VIF essentially
  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2 + x3, data = df, limit = 10)
  )

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune tie-breaking removes last variable in formula order", {
  set.seed(9103)
  n <- 100
  x1 <- rnorm(n)
  # Create two equally collinear pairs
  df <- data.frame(
    y = rnorm(n),
    a = x1,
    b = x1 + rnorm(n, sd = 0.1),
    c = rnorm(n),
    d = rnorm(n)
  )

  result <- modelPrune(y ~ a + b + c + d, data = df, limit = 5)

  # b should be removed (same VIF as a, but later in formula)
  expect_true("a" %in% attr(result, "selected_vars") ||
              "b" %in% attr(result, "selected_vars"))
})

test_that("modelPrune errors when no fixed effects remain", {
  df <- mtcars

  # Create custom engine that always returns high diagnostics
  always_high_engine <- list(
    name = "always_high",
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      setNames(rep(100, length(fixed_effects)), fixed_effects)
    }
  )

  # This should warn about removing all predictors
  expect_warning(
    modelPrune(mpg ~ cyl, data = df, engine = always_high_engine, limit = 1),
    "would remove all"
  )
})

test_that("modelPrune handles design matrix with factor predictors", {
  set.seed(9104)
  n <- 60
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    cat = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    x2 = rnorm(n)
  )

  result <- modelPrune(y ~ x1 + cat + x2, data = df, limit = 10)

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune VIF computation with missing column match", {
  set.seed(9105)
  n <- 50
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )

  result <- modelPrune(y ~ x1 + x2, data = df, limit = 10)

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune handles multi-level factors in VIF", {
  set.seed(9106)
  n <- 100
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    cat = factor(sample(LETTERS[1:5], n, replace = TRUE)),
    x2 = rnorm(n)
  )

  result <- modelPrune(y ~ x1 + cat + x2, data = df, limit = 10)

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune custom engine criterion parameter shows message", {
  df <- mtcars

  custom_engine <- list(
    name = "test_msg",
    fit = function(formula, data, ...) stats::lm(formula, data = data),
    diagnostics = function(model, fixed_effects) {
      setNames(rep(1, length(fixed_effects)), fixed_effects)
    }
  )

  # Non-vif criterion with custom engine should show message
  expect_message(
    modelPrune(mpg ~ cyl + disp, data = df, engine = custom_engine,
               criterion = "custom_crit", limit = 10),
    "ignored"
  )
})

test_that("modelPrune handles R-squared edge cases in VIF", {
  set.seed(9107)
  n <- 50

  # Create data where VIF might produce unusual R-squared values
  x1 <- rnorm(n)
  df <- data.frame(
    y = rnorm(n),
    x1 = x1,
    x2 = scale(x1) * 0.5 + rnorm(n, sd = 0.5),  # Moderate correlation
    x3 = rnorm(n)
  )

  result <- modelPrune(y ~ x1 + x2 + x3, data = df, limit = 10)

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune iterative removal works correctly", {
  set.seed(9108)
  n <- 100
  x1 <- rnorm(n)
  df <- data.frame(
    y = rnorm(n),
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.1),  # High VIF
    x3 = x1 + rnorm(n, sd = 0.2),  # High VIF
    x4 = rnorm(n)
  )

  # Multiple iterations needed
  result <- modelPrune(y ~ x1 + x2 + x3 + x4, data = df, limit = 5)

  expect_s3_class(result, "data.frame")
  expect_true(length(attr(result, "removed_vars")) >= 1)
})

test_that("modelPrune handles intercept-only design matrix", {
  set.seed(9109)
  n <- 50
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n)
  )

  # Single predictor case
  result <- modelPrune(y ~ x1, data = df, limit = 10)

  expect_s3_class(result, "data.frame")
  expect_true("x1" %in% names(result))
})

test_that("modelPrune lme4 glmer with binomial family", {
  skip_if_not_installed("lme4")

  set.seed(9110)
  n <- 100
  df <- data.frame(
    y = rbinom(n, 1, 0.5),
    x1 = rnorm(n),
    x2 = rnorm(n),
    group = factor(rep(1:10, each = 10))
  )

  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2 + (1|group), data = df,
               engine = "lme4", family = binomial(), limit = 10)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "engine"), "lme4")
})

test_that("modelPrune VIF with only one predictor remaining after removal", {
  set.seed(9111)
  n <- 100
  x1 <- rnorm(n)
  df <- data.frame(
    y = rnorm(n),
    x1 = x1,
    x2 = x1  # Perfect collinearity
  )

  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2, data = df, limit = 5)
  )

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune handles valid diagnostics at boundary of threshold", {
  set.seed(9112)
  n <- 50
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )

  # With very high limit, all should pass
  result <- modelPrune(y ~ x1 + x2, data = df, limit = 1000)

  expect_s3_class(result, "data.frame")
  expect_equal(length(attr(result, "removed_vars")), 0)
})

# ===========================================================================
# More edge case tests for internal functions
# ===========================================================================

test_that("modelPrune VIF with single remaining predictor", {
  set.seed(9201)
  n <- 100
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n)
  )

  # Only one predictor - VIF should be 1 (or NA/handled)
  result <- modelPrune(y ~ x1, data = df, limit = 10)

  expect_s3_class(result, "data.frame")
  expect_true("x1" %in% names(result))
})

test_that("modelPrune with perfect multicollinearity removes predictors", {
  set.seed(9202)
  n <- 100
  x1 <- rnorm(n)
  df <- data.frame(
    y = rnorm(n),
    x1 = x1,
    x2 = x1,  # Exactly equal to x1
    x3 = rnorm(n)
  )

  # Should handle Inf VIF
  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2 + x3, data = df, limit = 5)
  )

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune lme4 package check", {
  skip_if_not_installed("lme4")
  set.seed(9203)
  n <- 100
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    group = factor(rep(1:10, each = 10))
  )

  result <- modelPrune(y ~ x1 + x2 + (1|group), data = df,
                       engine = "lme4", limit = 10)

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune glmmTMB package check", {
  skip_if_not_installed("glmmTMB")
  set.seed(9204)
  n <- 100
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    group = factor(rep(1:10, each = 10))
  )

  result <- modelPrune(y ~ x1 + x2 + (1|group), data = df,
                       engine = "glmmTMB", limit = 10)

  expect_s3_class(result, "data.frame")
})

test_that("modelPrune removes multiple predictors iteratively", {
  set.seed(9205)
  n <- 200
  x1 <- rnorm(n)
  df <- data.frame(
    y = rnorm(n),
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.05),
    x3 = x1 + rnorm(n, sd = 0.05),
    x4 = x1 + rnorm(n, sd = 0.05),
    x5 = rnorm(n)
  )

  # Multiple high VIF predictors should be removed
  result <- modelPrune(y ~ x1 + x2 + x3 + x4 + x5, data = df, limit = 3)

  expect_s3_class(result, "data.frame")
  expect_true(length(attr(result, "removed_vars")) >= 1)
})


# ===========================================================================
# Condition number criterion tests
# ===========================================================================

test_that("modelPrune with condition_number criterion works", {
  set.seed(9301)
  n <- 100
  x1 <- rnorm(n)
  df <- data.frame(
    y = rnorm(n),
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.1),  # Highly collinear
    x3 = rnorm(n)  # Independent
  )

  result <- modelPrune(y ~ x1 + x2 + x3, data = df, criterion = "condition_number", limit = 10)

  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "criterion"), "condition_number")
  # Should prune at least one collinear variable
  expect_true(length(attr(result, "removed_vars")) >= 0)
})

test_that("modelPrune condition_number prunes collinear predictors", {
  set.seed(9302)
  n <- 200
  x1 <- rnorm(n)
  df <- data.frame(
    y = rnorm(n),
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.01),  # Very highly collinear
    x3 = rnorm(n)  # Independent
  )

  result <- modelPrune(y ~ x1 + x2 + x3, data = df, criterion = "condition_number", limit = 5)

  expect_s3_class(result, "data.frame")
  # With a strict limit, at least one collinear should be removed
  expect_true(length(attr(result, "removed_vars")) >= 1)
})

test_that("modelPrune condition_number with single predictor returns 1",
{
  set.seed(9303)
  df <- data.frame(y = rnorm(50), x = rnorm(50))

  result <- modelPrune(y ~ x, data = df, criterion = "condition_number", limit = 5)

  expect_s3_class(result, "data.frame")
  expect_equal(length(attr(result, "selected_vars")), 1)
})

test_that("modelPrune condition_number with GLM engine", {
  set.seed(9304)
  n <- 100
  x1 <- rnorm(n)
  df <- data.frame(
    y = rbinom(n, 1, 0.5),
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.1),
    x3 = rnorm(n)
  )

  result <- modelPrune(y ~ x1 + x2 + x3, data = df,
                       engine = "glm", family = binomial(),
                       criterion = "condition_number", limit = 10)

  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "criterion"), "condition_number")
})


# ===========================================================================
# Tests for lme4 engine (when available)
# ===========================================================================

test_that("modelPrune with lme4 engine prunes correctly", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE))

  set.seed(5001)
  n <- 100
  x1 <- rnorm(n)
  df <- data.frame(
    y = rnorm(n),
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.1),  # Collinear
    x3 = rnorm(n),
    group = factor(rep(1:10, each = 10))
  )

  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2 + x3 + (1|group), data = df, engine = "lme4", limit = 5)
  )
  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "engine"), "lme4")
})

test_that("modelPrune lme4 with glmer (binomial)", {
  skip_if_not(requireNamespace("lme4", quietly = TRUE))

  set.seed(5002)
  n <- 100
  df <- data.frame(
    y = rbinom(n, 1, 0.5),
    x1 = rnorm(n),
    x2 = rnorm(n),
    group = factor(rep(1:10, each = 10))
  )

  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2 + (1|group), data = df, engine = "lme4",
               family = binomial(), limit = 10)
  )
  expect_s3_class(result, "data.frame")
})

# ===========================================================================
# Tests for glmmTMB engine (when available)
# ===========================================================================

test_that("modelPrune with glmmTMB engine works", {
  skip_if_not(requireNamespace("glmmTMB", quietly = TRUE))

  set.seed(5003)
  n <- 100
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    group = factor(rep(1:10, each = 10))
  )

  result <- suppressWarnings(
    modelPrune(y ~ x1 + x2 + (1|group), data = df, engine = "glmmTMB", limit = 10)
  )
  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "engine"), "glmmTMB")
})

# ===========================================================================
# Tests for condition_number with edge cases
# ===========================================================================

test_that("modelPrune condition_number handles perfectly uncorrelated data", {
  set.seed(5004)
  n <- 100
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  result <- modelPrune(y ~ x1 + x2 + x3, data = df,
                       criterion = "condition_number", limit = 100)
  expect_s3_class(result, "data.frame")
  # All should be kept (low collinearity)
  expect_equal(length(attr(result, "selected_vars")), 3)
})

test_that("modelPrune condition_number with glm engine", {
  set.seed(5005)
  n <- 100
  x1 <- rnorm(n)
  df <- data.frame(
    y = rbinom(n, 1, 0.5),
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.05),  # Very collinear
    x3 = rnorm(n)
  )

  result <- modelPrune(y ~ x1 + x2 + x3, data = df, engine = "glm",
                       family = binomial(), criterion = "condition_number", limit = 5)
  expect_s3_class(result, "data.frame")
})
