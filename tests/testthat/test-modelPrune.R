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
    "For built-in engines, only criterion = 'vif' is supported"
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
