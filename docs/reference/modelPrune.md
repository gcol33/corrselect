# Model-Based Predictor Pruning

`modelPrune()` performs iterative removal of fixed-effect predictors
based on model diagnostics (e.g., VIF) until all remaining predictors
satisfy a specified threshold. It supports linear models, generalized
linear models, and mixed models.

## Usage

``` r
modelPrune(
  formula,
  data,
  engine = "lm",
  criterion = "vif",
  limit = 5,
  force_in = NULL,
  max_steps = NULL,
  ...
)
```

## Arguments

- formula:

  A model formula specifying the response and predictors. May include
  random effects for mixed models (e.g., `y ~ x1 + x2 + (1|group)`).

- data:

  A data.frame containing the variables in the formula.

- engine:

  Either a character string for built-in engines, or a list defining a
  custom engine.

  **Built-in engines** (character string):

  - `"lm"` (default): Linear models via
    [`stats::lm()`](https://rdrr.io/r/stats/lm.html)

  - `"glm"`: Generalized linear models via
    [`stats::glm()`](https://rdrr.io/r/stats/glm.html) (requires
    `family` argument)

  - `"lme4"`: Mixed models via
    [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) or
    [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) (requires
    lme4 package)

  - `"glmmTMB"`: Generalized linear mixed models via
    [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html)
    (requires glmmTMB package)

  **Custom engine** (named list with required components):

  - `fit`: function(formula, data, ...) that returns a fitted model
    object

  - `diagnostics`: function(model, fixed_effects) that returns a named
    numeric vector of diagnostic scores (one per fixed effect, higher
    values = worse)

  - `name` (optional): character string used in error messages (default:
    "custom")

- criterion:

  Character string specifying the diagnostic criterion for pruning. For
  built-in engines, only `"vif"` (Variance Inflation Factor) is
  supported. For custom engines, this parameter is ignored (diagnostics
  are computed by the engine's `diagnostics` function). Default:
  `"vif"`.

- limit:

  Numeric scalar. Maximum allowed value for the criterion. Predictors
  with diagnostic values exceeding this limit are iteratively removed.
  Default: 5 (common VIF threshold).

- force_in:

  Character vector of predictor names that must be retained in the final
  model. These variables will not be removed during pruning. Default:
  NULL.

- max_steps:

  Integer. Maximum number of pruning iterations. If NULL (default),
  pruning continues until all diagnostics are below the limit or no more
  removable predictors remain.

- ...:

  Additional arguments passed to the modeling function (e.g., `family`
  for glm/glmer, control parameters for lme4/glmmTMB).

## Value

A data.frame containing only the retained predictors (and response). The
result has the following attributes:

- selected_vars:

  Character vector of retained predictor names

- removed_vars:

  Character vector of removed predictor names (in order of removal)

- engine:

  Character string indicating which engine was used

- criterion:

  Character string indicating which criterion was used

- limit:

  The threshold value used

- final_model:

  The final fitted model object (optional)

## Details

`modelPrune()` works by:

1.  Parsing the formula to identify fixed-effect predictors

2.  Fitting the initial model

3.  Computing diagnostics for each fixed-effect predictor

4.  Checking feasibility of `force_in` constraints

5.  Iteratively removing the predictor with the worst diagnostic value
    (excluding `force_in` variables) until all diagnostics \<= `limit`

6.  Returning the pruned data frame

**Random Effects**: For mixed models (lme4, glmmTMB), only fixed-effect
predictors are considered for pruning. Random-effect structure is
preserved exactly as specified in the original formula.

**VIF Computation**: Variance Inflation Factors are computed from the
fixed-effects design matrix. For categorical predictors, VIF represents
the inflation for the entire factor (not individual dummy variables).

**Determinism**: The algorithm is deterministic. Ties in diagnostic
values are broken by removing the predictor that appears last in the
formula.

**Force-in Constraints**: If variables in `force_in` violate the
diagnostic threshold, the function will error. This ensures that the
constraint is feasible before pruning begins.

## See also

[`corrPrune`](https://gcol33.github.io/corrselect/reference/corrPrune.md)
for association-based predictor pruning,
[`corrSelect`](https://gcol33.github.io/corrselect/reference/corrSelect.md)
for exhaustive subset enumeration.

## Examples

``` r
# Linear model with VIF-based pruning
data(mtcars)
pruned <- modelPrune(mpg ~ ., data = mtcars, engine = "lm", limit = 5)
names(pruned)
#> [1] "mpg"  "drat" "qsec" "vs"   "am"   "gear" "carb"

# Force certain predictors to remain
pruned <- modelPrune(mpg ~ ., data = mtcars, force_in = "drat", limit = 20)

# GLM example (requires family argument)
pruned <- modelPrune(am ~ ., data = mtcars, engine = "glm",
                     family = binomial(), limit = 5)
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

if (FALSE) { # \dontrun{
# Custom engine example (INLA)
inla_engine <- list(
  name = "inla",
  fit = function(formula, data, ...) {
    inla::inla(formula = formula, data = data,
               family = list(...)$family %||% "gaussian",
               control.compute = list(config = TRUE))
  },
  diagnostics = function(model, fixed_effects) {
    scores <- model$summary.fixed[, "sd"]
    names(scores) <- rownames(model$summary.fixed)
    scores[fixed_effects]
  }
)

pruned <- modelPrune(y ~ x1 + x2 + x3, data = df,
                     engine = inla_engine, limit = 0.5)
} # }
```
