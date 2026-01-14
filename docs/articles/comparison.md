# Comparison with Alternatives

## Overview

This vignette compares corrselect’s graph-theoretic approach to four
established methods for multicollinearity management and variable
selection:

1.  **caret::findCorrelation()** - Greedy correlation-based pruning
2.  **Boruta** - Random forest permutation importance
3.  **glmnet** - L1/L2 regularization (LASSO/Ridge)
4.  **Manual VIF removal** - Iterative variance inflation factor
    thresholding

Each comparison examines algorithmic differences, performance
characteristics, and appropriate use cases. Evaluations use the
`bioclim_example` dataset (19 bioclimatic variables, $`n = 100`$).

See
[`vignette("theory")`](https://gillescolling.com/corrselect/articles/theory.md)
for mathematical foundations. See
[`vignette("quickstart")`](https://gillescolling.com/corrselect/articles/quickstart.md)
for usage examples.

------------------------------------------------------------------------

## Evaluation Dataset

``` r

data(bioclim_example)
predictors <- bioclim_example[, -1]  # Exclude response
response <- bioclim_example[, 1]

cat("Variables:", ncol(predictors), "\n")
#> Variables: 19
cat("Observations:", nrow(predictors), "\n")
#> Observations: 100
cat("Response: species_richness (continuous)\n")
#> Response: species_richness (continuous)
```

``` r

cor_matrix <- cor(predictors)

# Correlation heatmap
col_pal <- colorRampPalette(c("#3B4992", "white", "#EE0000"))(100)

par(mar = c(1, 1, 3, 1))
nc <- ncol(cor_matrix)
nr <- nrow(cor_matrix)
image(seq_len(nc), seq_len(nr), t(cor_matrix[nr:1, ]),
      col = col_pal,
      xlab = "", ylab = "", axes = FALSE,
      main = "Bioclimatic Variable Correlations (p = 19)",
      zlim = c(-1, 1))
axis(1, at = seq_len(nc), labels = colnames(cor_matrix), las = 2, cex.axis = 0.7)
axis(2, at = nc:1, labels = colnames(cor_matrix), las = 2, cex.axis = 0.7)

for (i in seq_len(nc)) {
  for (j in seq_len(nr)) {
    text_col <- if (abs(cor_matrix[j, i]) > 0.6) "white" else "black"
    text(i, nr - j + 1, sprintf("%.2f", cor_matrix[j, i]),
         cex = 0.5, col = text_col)
  }
}
```

![Correlation heatmap of 19 bioclimatic variables displayed as a
color-coded matrix. Blue indicates negative correlations, white
indicates near-zero correlations, and red indicates positive
correlations. Numerical correlation values are overlaid on each cell.
The heatmap reveals block structure with correlations ranging from -0.15
to 0.97, showing strong correlations among temperature-related variables
and precipitation-related
variables.](comparison_files/figure-html/unnamed-chunk-2-1.svg)

Block structure present: correlations range from -0.15 to 0.97.

------------------------------------------------------------------------

## Comparison 1: caret::findCorrelation()

### Method

caret’s `findCorrelation()` applies greedy iterative removal:

1.  Identify pair with maximum $`|r_{ij}|`$
2.  Remove variable with larger mean absolute correlation
3.  Repeat until all $`|r_{ij}| < \tau`$

Non-deterministic: results depend on internal ordering. Typically
removes more variables than graph-theoretic methods.

### Execution

``` r

if (requireNamespace("caret", quietly = TRUE)) {
  # Apply caret's greedy algorithm
  to_remove_caret <- caret::findCorrelation(cor_matrix, cutoff = 0.7)
  result_caret <- predictors[, -to_remove_caret]

  cat("caret results:\n")
  cat("  Variables retained:", ncol(result_caret), "\n")
  cat("  Variables removed:", length(to_remove_caret), "\n")
  cat("  Removed:", paste(colnames(predictors)[to_remove_caret], collapse = ", "), "\n")
}
#> caret results:
#>   Variables retained: 10 
#>   Variables removed: 9 
#>   Removed: BIO8, BIO7, BIO5, BIO4, BIO3, BIO9, BIO1, BIO11, BIO15
```

``` r

# Apply corrselect (exact mode)
result_corrselect <- corrPrune(predictors, threshold = 0.7, mode = "exact")

cat("\ncorrselect results:\n")
#> 
#> corrselect results:
cat("  Variables retained:", ncol(result_corrselect), "\n")
#>   Variables retained: 12
cat("  Variables removed:", length(attr(result_corrselect, "removed_vars")), "\n")
#>   Variables removed: 7
cat("  Removed:", paste(attr(result_corrselect, "removed_vars"), collapse = ", "), "\n")
#>   Removed: BIO2, BIO4, BIO5, BIO7, BIO8, BIO10, BIO15
```

corrselect retains more variables
($`|S_{\text{corrselect}}| \ge |S_{\text{caret}}|`$) via maximal clique
enumeration while satisfying identical threshold constraint.

### Distribution Comparison

``` r

# Extract correlations
cor_orig <- cor(predictors)
cor_corrselect <- cor(result_corrselect)

if (requireNamespace("caret", quietly = TRUE)) {
  cor_caret <- cor(result_caret)

  # Overlaid histogram comparing all three
  hist(abs(cor_orig[upper.tri(cor_orig)]),
       breaks = 30,
       main = "Distribution of Absolute Correlations",
       xlab = "Absolute Correlation",
       col = rgb(0.5, 0.5, 0.5, 0.4),
       xlim = c(0, 1))

  hist(abs(cor_caret[upper.tri(cor_caret)]),
       breaks = 30,
       col = rgb(0.8, 0.2, 0.2, 0.4),
       add = TRUE)

  hist(abs(cor_corrselect[upper.tri(cor_corrselect)]),
       breaks = 30,
       col = rgb(0.2, 0.5, 0.8, 0.4),
       add = TRUE)

  abline(v = 0.7, col = "black", lwd = 2, lty = 2)

  legend("topright",
         legend = c(
           paste0("Original (", ncol(predictors), " vars)"),
           paste0("caret (", ncol(result_caret), " vars)"),
           paste0("corrselect (", ncol(result_corrselect), " vars)"),
           "Threshold"
         ),
         fill   = c(
           rgb(0.5, 0.5, 0.5, 0.4),
           rgb(0.8, 0.2, 0.2, 0.4),
           rgb(0.2, 0.5, 0.8, 0.4),
           NA
         ),
         border = c("white", "white", "white", NA),
         lty    = c(NA, NA, NA, 2),
         lwd    = c(NA, NA, NA, 2),
         col    = c(NA, NA, NA, "black"),
         bty    = "o",
         bg = "white")
}
```

![Overlaid histogram comparing absolute correlation distributions across
three methods: original data (gray bars), caret's findCorrelation (red
bars), and corrselect (blue bars). Black vertical dashed line marks the
0.7 threshold. All methods successfully reduce correlations below the
threshold, but corrselect retains more variables than caret while still
satisfying the constraint, demonstrating the advantage of maximal clique
enumeration over greedy
removal.](comparison_files/figure-html/unnamed-chunk-5-1.svg)

### Comparison

| Feature | caret | corrselect |
|----|----|----|
| **Algorithm** | Greedy iterative removal | Maximal clique enumeration |
| **Optimality** | Heuristic | Exact (mode = “exact”) |
| **Reproducibility** | Non-deterministic | Deterministic |
| **Variables retained** | $`\le`$ optimal | Maximal |
| **Forced variables** | No | Yes (`force_in`) |
| **Mixed data** | No | Yes (`assocSelect`) |
| **Complexity** | $`O(p^2)`$ | $`O(p^2)`$ greedy, $`O(3^{p/3})`$ exact |

### Applications

**caret**: Exploratory analysis, non-critical reproducibility
requirements.

**corrselect**: Reproducible research, maximal variable retention,
forced variable constraints, mixed data types.

------------------------------------------------------------------------

## Comparison 2: Boruta

### Method

Boruta tests variable importance via random forest permutation:

1.  Create shadow features (permuted copies)
2.  Fit random forest on original + shadow features
3.  Test:
    $`\text{importance}(X_i) > \max(\text{importance}(\text{shadow}))`$
4.  Iteratively confirm/reject until convergence

**Orthogonal objective**: Boruta selects predictive variables
(supervised). corrselect removes redundant variables (unsupervised).

### Execution

``` r

if (requireNamespace("Boruta", quietly = TRUE)) {
  # Boruta: "Which variables predict species_richness?"
  set.seed(123)
  boruta_result <- Boruta::Boruta(
    species_richness ~ .,
    data    = bioclim_example,
    maxRuns = 100
  )

  cat("Boruta variable importance screening:\n")
  print(table(boruta_result$finalDecision))

  important_vars <- names(boruta_result$finalDecision[
    boruta_result$finalDecision == "Confirmed"
  ])

  cat("\n  Confirmed predictors:", length(important_vars), "\n")
  cat(" ", paste(important_vars, collapse = ", "), "\n")
}
```

``` r

# corrselect: "Which variables are redundant?"
corrselect_result <- corrPrune(predictors, threshold = 0.7)

cat("\ncorrselect multicollinearity pruning:\n")
#> 
#> corrselect multicollinearity pruning:
cat("  Non-redundant variables:", ncol(corrselect_result), "\n")
#>   Non-redundant variables: 12
cat(" ", paste(names(corrselect_result), collapse = ", "), "\n")
#>   BIO1, BIO3, BIO6, BIO9, BIO11, BIO12, BIO13, BIO14, BIO16, BIO17, BIO18, BIO19
```

Different variable sets selected: Boruta optimizes prediction;
corrselect minimizes redundancy.

### Comparison

| Criterion             | Boruta                     | corrselect            |
|-----------------------|----------------------------|-----------------------|
| **Objective**         | Predictive power           | Redundancy removal    |
| **Criterion**         | Permutation importance     | $`\|r_{ij}\| < \tau`$ |
| **Response**          | Required                   | Not required          |
| **Multicollinearity** | Indirect                   | Direct                |
| **Stochastic**        | Yes                        | No                    |
| **Complexity**        | High ($`\ge 100`$ forests) | Low (graph)           |

### Sequential Application

``` r

# Stage 1: Correlation-based pruning
data_pruned <- corrPrune(raw_data, threshold = 0.7)

# Stage 2: Importance testing
boruta_result <- Boruta::Boruta(response ~ ., data = cbind(response, data_pruned))
final_vars <- names(boruta_result$finalDecision[
  boruta_result$finalDecision == "Confirmed"
])

# Stage 3: Final model
final_model <- lm(response ~ ., data = cbind(response, data_pruned)[, c("response", final_vars)])
```

Stage 1 removes redundancy (reproducible). Stage 2 tests importance
(stochastic). Stage 3 fits model with non-redundant, predictive
variables.

### Applications

**Boruta**: Prediction-focused analysis with response variable.

**corrselect**: Multicollinearity removal, exploratory analysis without
response, reproducible selection.

**Sequential**: High-dimensional correlated data requiring both
redundancy removal and importance testing.

------------------------------------------------------------------------

## Comparison 3: glmnet (LASSO/Ridge)

### Method

glmnet minimizes regularized loss:

``` math
\min_{\beta} \frac{1}{2n} \|y - X\beta\|_2^2 + \lambda \left[\alpha \|\beta\|_1 + (1-\alpha) \|\beta\|_2^2\right]
```

- $`\alpha = 1`$: LASSO (L1 penalty, sparse $`\beta`$)
- $`\alpha = 0`$: Ridge (L2 penalty, shrinkage)
- $`\lambda`$: Cross-validation selected

**Difference**: glmnet performs soft selection (shrinkage) optimizing
prediction. corrselect performs hard selection (removal) based on
correlation structure.

### Execution

``` r

if (requireNamespace("glmnet", quietly = TRUE)) {
  # Fit LASSO with cross-validation
  X <- as.matrix(predictors)
  y <- response

  set.seed(123)
  cv_lasso <- glmnet::cv.glmnet(X, y, alpha = 1)

  # Extract non-zero coefficients at lambda.1se (conservative choice)
  coef_lasso <- stats::coef(cv_lasso, s = "lambda.1se")
  selected_lasso <- rownames(coef_lasso)[coef_lasso[, 1] != 0][-1]  # Remove intercept

  cat("glmnet (LASSO, λ = lambda.1se):\n")
  cat("  Variables retained:", length(selected_lasso), "\n")
  cat(" ", paste(selected_lasso, collapse = ", "), "\n")
}
```

``` r

if (requireNamespace("glmnet", quietly = TRUE)) {
  # Compare model performance
  model_glmnet <- lm(species_richness ~ .,
                     data = bioclim_example[, c("species_richness", selected_lasso)])

  model_corrselect <- lm(species_richness ~ .,
                         data = cbind(species_richness = response, result_corrselect))

  cat("\nModel comparison (OLS on selected variables):\n")
  cat("  glmnet:     R² =", round(summary(model_glmnet)$r.squared, 3),
      "with", length(selected_lasso), "predictors\n")
  cat("  corrselect: R² =", round(summary(model_corrselect)$r.squared, 3),
      "with", ncol(result_corrselect), "predictors\n")
}
```

glmnet selects fewer variables
($`|S_{\text{glmnet}}| \le |S_{\text{corrselect}}|`$) optimizing
prediction. corrselect maximizes retention under correlation constraint.

### Coefficient Comparison

``` r

if (requireNamespace("glmnet", quietly = TRUE)) {
  par(mfrow = c(1, 2), mar = c(8, 4, 3, 2))

  # glmnet coefficients (shrinkage)
  coef_vals <- coef_lasso[coef_lasso[, 1] != 0, ][-1]
  barplot(sort(abs(coef_vals), decreasing = TRUE),
          las = 2,
          main = "glmnet: Shrunk Coefficients",
          ylab = "Absolute Coefficient Value",
          col = "salmon",
          cex.names = 0.7)

  # corrselect: unbiased OLS coefficients
  coef_corrselect <- coef(model_corrselect)[-1]  # Remove intercept
  barplot(sort(abs(coef_corrselect), decreasing = TRUE),
          las = 2,
          main = "corrselect: Unbiased OLS Coefficients",
          ylab = "Absolute Coefficient Value",
          col = rgb(0.2, 0.5, 0.8, 0.7),
          cex.names = 0.7)
}
```

Left: L1 penalty shrinks coefficients toward zero (biased). Right: OLS
on pruned variables (unbiased). glmnet optimizes prediction with
shrinkage. corrselect preserves effect sizes.

### Comparison

| Criterion | glmnet | corrselect |
|----|----|----|
| **Objective** | Prediction accuracy | Multicollinearity removal |
| **Selection** | Soft (shrinkage) | Hard (removal) |
| **Coefficient bias** | Yes (L1/L2) | No |
| **Multicollinearity** | Regularization | Pruning |
| **Response** | Required | Not required |
| **Tuning** | $`\lambda`$ (cross-validation) | $`\tau`$ (user-specified) |
| **Interpretability** | Shrunk effects | Direct effects |

### Applications

**glmnet**: Prediction-focused, high-dimensional ($`p > n`$), accepts
biased coefficients.

**corrselect**: Interpretable coefficients, exploratory analysis,
explicit correlation constraint, unregularized modeling.

**Sequential**: Correlation pruning (corrselect) followed by sparse
prediction (glmnet).

------------------------------------------------------------------------

## Comparison 4: modelPrune() vs Manual VIF Removal

### Method

Variance Inflation Factor quantifies predictor multicollinearity:

``` math
\text{VIF}_j = \frac{1}{1 - R^2_j}
```

where $`R^2_j`$ results from regressing $`X_j`$ on remaining predictors.
Thresholds:

- VIF \< 5: Low collinearity
- VIF \< 10: Moderate (acceptable)
- VIF ≥ 10: High (problematic)

Manual approach: iteratively remove max(VIF) until all VIF \< threshold.

### Manual Implementation

``` r

# Manual iterative VIF removal
manual_vif_removal <- function(formula, data, threshold = 5, max_iter = 10) {
  require(car)

  # Get response variable name
  response_var <- all.vars(formula)[1]

  # Get predictor names (handles ~ . notation)
  model <- lm(formula, data = data)
  current_vars <- names(coef(model))[-1]  # Exclude intercept

  removed_vars <- character(0)
  vif_vals <- car::vif(model)

  while (max(vif_vals) > threshold && length(current_vars) > 1 && length(removed_vars) < max_iter) {
    # Remove variable with highest VIF
    var_to_remove <- names(which.max(vif_vals))
    removed_vars <- c(removed_vars, var_to_remove)
    cat("Iteration", length(removed_vars), ": Removing", var_to_remove,
        "(VIF =", round(max(vif_vals), 2), ")\n")

    # Update variable list and refit
    current_vars <- setdiff(current_vars, var_to_remove)
    new_formula <- as.formula(paste(response_var, "~", paste(current_vars, collapse = " + ")))
    model <- lm(new_formula, data = data)
    vif_vals <- car::vif(model)
  }

  list(model = model, iterations = length(removed_vars),
       vif = vif_vals, removed = removed_vars, converged = max(vif_vals) <= threshold)
}

# Run manual VIF removal
if (requireNamespace("car", quietly = TRUE)) {
  cat("Manual VIF removal (iterative):\n")
  manual_result <- manual_vif_removal(species_richness ~ ., data = bioclim_example, threshold = 5)
  cat("\nVariables kept:", length(manual_result$vif), "\n")
  if (!manual_result$converged) {
    cat("(Stopped at max_iter = 10; VIF threshold not yet reached)\n")
  }
}
```

## modelPrune() Comparison

``` r

# Run modelPrune
modelprune_result <- modelPrune(species_richness ~ ., data = bioclim_example, limit = 5)

cat("\nmodelPrune results:\n")
#> 
#> modelPrune results:
cat("Variables removed:", attr(modelprune_result, "removed_vars"), "\n")
#> Variables removed: BIO2 BIO7 BIO5
cat("Variables kept:", length(attr(modelprune_result, "selected_vars")), "\n")
#> Variables kept: 16

# Extract final model
final_model <- attr(modelprune_result, "final_model")
if (requireNamespace("car", quietly = TRUE)) {
  cat("\nFinal VIF values:\n")
  print(round(car::vif(final_model), 2))
}
```

## Visual: VIF Comparison

``` r

if (requireNamespace("car", quietly = TRUE)) {
  # Compute VIF for original model
  model_full <- lm(species_richness ~ ., data = bioclim_example)
  vif_before <- car::vif(model_full)

  # VIF after modelPrune
  vif_after <- car::vif(final_model)

  # Combined barplot
  par(mar = c(8, 4, 4, 2))
  all_vars <- unique(c(names(vif_before), names(vif_after)))
  vif_combined <- data.frame(
    before = vif_before[match(all_vars, names(vif_before))],
    after = vif_after[match(all_vars, names(vif_after))]
  )
  vif_combined[is.na(vif_combined)] <- 0
  vif_combined <- vif_combined[order(vif_combined$before, decreasing = TRUE), ]

  # Show top 15
  n_show <- min(15, nrow(vif_combined))
  barplot(t(as.matrix(vif_combined[1:n_show, ])),
          beside = TRUE,
          las = 2,
          main = "VIF Before and After modelPrune()",
          ylab = "VIF",
          col = c(rgb(0.8, 0.2, 0.2, 0.7), rgb(0.2, 0.5, 0.8, 0.7)),
          cex.names = 0.6,
          names.arg = rownames(vif_combined)[1:n_show])
  abline(h = 5, col = "black", lwd = 2, lty = 2)
  legend("topright",
         legend = c("Before", "After", "Limit = 5"),
         fill   = c(rgb(0.8, 0.2, 0.2, 0.7), rgb(0.2, 0.5, 0.8, 0.7), NA),
         border = c("white", "white", NA),
         lty    = c(NA, NA, 2),
         lwd    = c(NA, NA, 2),
         col    = c(NA, NA, "black"),
         bty    = "o",
         bg = "white")
}
```

### Comparison

| Criterion            | Manual VIF       | modelPrune()   |
|----------------------|------------------|----------------|
| **Algorithm**        | Iterative greedy | Graph-based    |
| **Automation**       | Manual           | Automated      |
| **Optimality**       | Heuristic        | Maximal subset |
| **Forced variables** | Manual exclusion | `force_in`     |
| **Output**           | Verbose log      | Summary        |

### Applications

**Manual VIF**: Educational use, diagnostic understanding, legacy
workflows.

**modelPrune()**: Production pipelines, forced variable constraints,
reproducible documentation.

------------------------------------------------------------------------

## Summary

### Method Selection

| Goal | Primary Method | Alternative |
|----|----|----|
| Redundancy removal (unsupervised) | corrPrune() | caret::findCorrelation() |
| VIF reduction (regression) | modelPrune() | Manual VIF |
| Predictive variable selection | Boruta | RF importance |
| Prediction accuracy | glmnet | Elastic net |
| Mixed data types | assocSelect() | Manual metrics |
| Forced variable constraints | corrselect (`force_in`) | N/A |
| Exploratory (fast) | caret | corrPrune (greedy) |

### corrselect Distinguishing Features

1.  **Maximal clique enumeration**: Optimal retention under
    $`|r_{ij}| < \tau`$ constraint
2.  **Deterministic**: ELS and Bron-Kerbosch algorithms guarantee
    reproducibility
3.  **Flexible**: Forced variables, mixed types, greedy/exact modes
4.  **Unbiased estimates**: Hard removal preserves coefficient
    interpretability
5.  **Model-agnostic**: Correlation-based preprocessing

### Integrated Workflow

``` r

# Correlation pruning
data_pruned <- corrPrune(raw_data, threshold = 0.7)

# VIF refinement
model_data <- modelPrune(response ~ ., data = data_pruned, limit = 5)

# Importance testing (optional)
if (requireNamespace("Boruta", quietly = TRUE)) {
  boruta_result <- Boruta::Boruta(response ~ ., data = model_data)
  important_vars <- names(boruta_result$finalDecision[
    boruta_result$finalDecision == "Confirmed"
  ])
}

# Final model: OLS (interpretable) or glmnet (prediction)
final_model <- lm(response ~ ., data = model_data[, c("response", important_vars)])
```

## References

- **caret**: Kuhn, M. (2008). Building predictive models in R using the
  caret package. *Journal of Statistical Software*, 28(5), 1-26.
  [doi:10.18637/jss.v028.i05](https://doi.org/10.18637/jss.v028.i05)

- **Boruta**: Kursa, M. B., & Rudnicki, W. R. (2010). Feature selection
  with the Boruta package. *Journal of Statistical Software*, 36(11),
  1-13.
  [doi:10.18637/jss.v036.i11](https://doi.org/10.18637/jss.v036.i11)

- **glmnet**: Friedman, J., Hastie, T., & Tibshirani, R. (2010).
  Regularization paths for generalized linear models via coordinate
  descent. *Journal of Statistical Software*, 33(1), 1-22.
  [doi:10.18637/jss.v033.i01](https://doi.org/10.18637/jss.v033.i01)

- **VIF**: Belsley, D. A., Kuh, E., & Welsch, R. E. (1980). *Regression
  Diagnostics: Identifying Influential Data and Sources of
  Collinearity*. Wiley.
  [doi:10.1002/0471725153](https://doi.org/10.1002/0471725153)

## See Also

- [`vignette("quickstart")`](https://gillescolling.com/corrselect/articles/quickstart.md) -
  Interface overview and usage examples
- [`vignette("workflows")`](https://gillescolling.com/corrselect/articles/workflows.md) -
  Domain-specific workflows (genomics, ecology, surveys)
- [`vignette("advanced")`](https://gillescolling.com/corrselect/articles/advanced.md) -
  Algorithm selection and performance tuning
- [`vignette("theory")`](https://gillescolling.com/corrselect/articles/theory.md) -
  Graph-theoretic foundations and formal proofs

## Session Info

``` r

sessionInfo()
#> R version 4.5.2 (2025-10-31 ucrt)
#> Platform: x86_64-w64-mingw32/x64
#> Running under: Windows 11 x64 (build 26200)
#> 
#> Matrix products: default
#>   LAPACK version 3.12.1
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.utf8 
#> [2] LC_CTYPE=English_United States.utf8   
#> [3] LC_MONETARY=English_United States.utf8
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.utf8    
#> 
#> time zone: Europe/Luxembourg
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] corrselect_3.1.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.6         xfun_0.55            bslib_0.9.0         
#>  [4] ggplot2_4.0.1        htmlwidgets_1.6.4    recipes_1.3.1       
#>  [7] lattice_0.22-7       vctrs_0.6.5          tools_4.5.2         
#> [10] generics_0.1.4       stats4_4.5.2         parallel_4.5.2      
#> [13] tibble_3.3.0         ModelMetrics_1.2.2.2 pkgconfig_2.0.3     
#> [16] Matrix_1.7-4         data.table_1.18.0    RColorBrewer_1.1-3  
#> [19] S7_0.2.1             desc_1.4.3           lifecycle_1.0.5     
#> [22] compiler_4.5.2       farver_2.1.2         stringr_1.6.0       
#> [25] textshaping_1.0.4    codetools_0.2-20     htmltools_0.5.9     
#> [28] class_7.3-23         sass_0.4.10          yaml_2.3.12         
#> [31] prodlim_2025.04.28   pillar_1.11.1        pkgdown_2.2.0       
#> [34] jquerylib_0.1.4      MASS_7.3-65          cachem_1.1.0        
#> [37] gower_1.0.2          iterators_1.0.14     rpart_4.1.24        
#> [40] foreach_1.5.2        nlme_3.1-168         parallelly_1.46.1   
#> [43] lava_1.8.2           tidyselect_1.2.1     digest_0.6.39       
#> [46] stringi_1.8.7        future_1.68.0        dplyr_1.1.4         
#> [49] reshape2_1.4.5       purrr_1.2.0          listenv_0.10.0      
#> [52] splines_4.5.2        fastmap_1.2.0        grid_4.5.2          
#> [55] cli_3.6.5            magrittr_2.0.4       survival_3.8-3      
#> [58] future.apply_1.20.1  withr_3.0.2          scales_1.4.0        
#> [61] lubridate_1.9.4      timechange_0.3.0     rmarkdown_2.30      
#> [64] globals_0.18.0       otel_0.2.0           nnet_7.3-20         
#> [67] timeDate_4051.111    evaluate_1.0.5       knitr_1.51          
#> [70] hardhat_1.4.2        caret_7.0-1          rlang_1.1.6         
#> [73] Rcpp_1.1.1           glue_1.8.0           pROC_1.19.0.1       
#> [76] ipred_0.9-15         svglite_2.2.2        jsonlite_2.0.0      
#> [79] R6_2.6.1             plyr_1.8.9           systemfonts_1.3.1   
#> [82] fs_1.6.6
```
