# Example Longitudinal Data for Clinical Research

A simulated longitudinal study dataset with 50 subjects measured at 10
timepoints each, with 20 correlated predictors and nested random effects
(subject and site).

## Usage

``` r
longitudinal_example
```

## Format

A data frame with 500 rows and 25 variables:

- obs_id:

  Integer. Observation identifier (1-500)

- subject:

  Factor. Subject identifier (1-50)

- site:

  Factor. Study site identifier (1-5)

- time:

  Integer. Measurement timepoint (1-10)

- outcome:

  Numeric. Continuous outcome variable

- x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16,
  x17, x18, x19, x20:

  Numeric. Correlated predictor variables

## Source

Simulated data based on typical clinical trial designs

## Details

This dataset represents a typical longitudinal study with repeated
measures. Predictors are correlated both within and between subjects:

- Predictors x1-x10: Highly correlated (r ~= 0.75)

- Predictors x11-x20: Moderately correlated (r ~= 0.50)

The outcome depends on time (linear trend), random effects (subject and
site), and a subset of fixed-effect predictors (x1, x5, x15).

**Use case**: Demonstrating
[`modelPrune()`](https://gcol33.github.io/corrselect/reference/modelPrune.md)
with mixed models (`lme4` engine) to prune fixed effects while
preserving random effects structure.

## See also

[`modelPrune()`](https://gcol33.github.io/corrselect/reference/modelPrune.md)

## Examples

``` r
data(longitudinal_example)

if (FALSE) { # \dontrun{
# Prune fixed effects in mixed model (requires lme4)
if (requireNamespace("lme4", quietly = TRUE)) {
  pruned <- modelPrune(
    outcome ~ x1 + x2 + x3 + x4 + x5 + (1|subject) + (1|site),
    data = longitudinal_example,
    engine = "lme4",
    limit = 5
  )

  # Random effects preserved, only fixed effects pruned
  attr(pruned, "selected_vars")
}
} # }
```
