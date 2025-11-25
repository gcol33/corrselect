## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5
)
library(corrselect)

## -----------------------------------------------------------------------------
data(bioclim_example)
predictors <- bioclim_example[, -1]  # Exclude response
response <- bioclim_example[, 1]

cat("Variables:", ncol(predictors), "\n")
cat("Observations:", nrow(predictors), "\n")
cat("Response: species_richness (continuous)\n")

## ----fig.width=8, fig.height=6, fig.alt="Correlation heatmap of 19 bioclimatic variables displayed as a color-coded matrix. Blue indicates negative correlations, white indicates near-zero correlations, and red indicates positive correlations. Numerical correlation values are overlaid on each cell. The heatmap reveals block structure with correlations ranging from -0.15 to 0.97, showing strong correlations among temperature-related variables and precipitation-related variables."----
cor_matrix <- cor(predictors)

# Correlation heatmap
col_pal <- colorRampPalette(c("#3B4992", "white", "#EE0000"))(100)

par(mar = c(1, 1, 3, 1))
image(1:ncol(cor_matrix), 1:nrow(cor_matrix), t(cor_matrix[nrow(cor_matrix):1, ]),
      col = col_pal,
      xlab = "", ylab = "", axes = FALSE,
      main = "Bioclimatic Variable Correlations (p = 19)",
      zlim = c(-1, 1))
axis(1, at = 1:ncol(cor_matrix), labels = colnames(cor_matrix), las = 2, cex.axis = 0.7)
axis(2, at = ncol(cor_matrix):1, labels = colnames(cor_matrix), las = 2, cex.axis = 0.7)

for (i in 1:ncol(cor_matrix)) {
  for (j in 1:nrow(cor_matrix)) {
    text_col <- if (abs(cor_matrix[j, i]) > 0.6) "white" else "black"
    text(i, nrow(cor_matrix) - j + 1, sprintf("%.2f", cor_matrix[j, i]),
         cex = 0.5, col = text_col)
  }
}

## -----------------------------------------------------------------------------
if (requireNamespace("caret", quietly = TRUE)) {
  # Apply caret's greedy algorithm
  to_remove_caret <- caret::findCorrelation(cor_matrix, cutoff = 0.7)
  result_caret <- predictors[, -to_remove_caret]

  cat("caret results:\n")
  cat("  Variables retained:", ncol(result_caret), "\n")
  cat("  Variables removed:", length(to_remove_caret), "\n")
  cat("  Removed:", paste(colnames(predictors)[to_remove_caret], collapse = ", "), "\n")
}

## -----------------------------------------------------------------------------
# Apply corrselect (exact mode)
result_corrselect <- corrPrune(predictors, threshold = 0.7, mode = "exact")

cat("\ncorrselect results:\n")
cat("  Variables retained:", ncol(result_corrselect), "\n")
cat("  Variables removed:", length(attr(result_corrselect, "removed_vars")), "\n")
cat("  Removed:", paste(attr(result_corrselect, "removed_vars"), collapse = ", "), "\n")

## ----fig.width=7, fig.height=5, fig.alt="Overlaid histogram comparing absolute correlation distributions across three methods: original data (gray bars), caret's findCorrelation (red bars), and corrselect (blue bars). Black vertical dashed line marks the 0.7 threshold. All methods successfully reduce correlations below the threshold, but corrselect retains more variables than caret while still satisfying the constraint, demonstrating the advantage of maximal clique enumeration over greedy removal."----
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
         bty    = "n")
}

## ----eval=requireNamespace("Boruta", quietly=TRUE)----------------------------
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

## -----------------------------------------------------------------------------
# corrselect: "Which variables are redundant?"
corrselect_result <- corrPrune(predictors, threshold = 0.7)

cat("\ncorrselect multicollinearity pruning:\n")
cat("  Non-redundant variables:", ncol(corrselect_result), "\n")
cat(" ", paste(names(corrselect_result), collapse = ", "), "\n")

## ----eval=FALSE---------------------------------------------------------------
# # Stage 1: Correlation-based pruning
# data_pruned <- corrPrune(raw_data, threshold = 0.7)
# 
# # Stage 2: Importance testing
# boruta_result <- Boruta::Boruta(response ~ ., data = cbind(response, data_pruned))
# final_vars <- names(boruta_result$finalDecision[
#   boruta_result$finalDecision == "Confirmed"
# ])
# 
# # Stage 3: Final model
# final_model <- lm(response ~ ., data = cbind(response, data_pruned)[, c("response", final_vars)])

## ----eval=requireNamespace("glmnet", quietly=TRUE)----------------------------
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

## ----eval=requireNamespace("glmnet", quietly=TRUE)----------------------------
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

## ----eval=requireNamespace("glmnet", quietly=TRUE), fig.width=10, fig.height=5, fig.alt="Side-by-side barplots comparing coefficient magnitudes between glmnet (left panel, salmon bars) and corrselect (right panel, blue bars). Left panel shows glmnet's shrunk coefficients affected by L1 penalty, biased toward zero. Right panel shows corrselect's unbiased OLS coefficients on pruned variables with preserved effect sizes. The comparison illustrates the tradeoff between prediction-focused shrinkage and interpretation-focused hard selection."----
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

## -----------------------------------------------------------------------------
# Manual iterative VIF removal
manual_vif_removal <- function(formula, data, threshold = 5) {
  require(car)

  model <- lm(formula, data = data)
  vif_vals <- car::vif(model)

  iterations <- 0
  while (max(vif_vals) > threshold && iterations < 100) {
    iterations <- iterations + 1

    # Remove variable with highest VIF
    var_to_remove <- names(which.max(vif_vals))
    cat("Iteration", iterations, ": Removing", var_to_remove, "(VIF =",
        round(max(vif_vals), 2), ")\n")

    # Update formula
    formula_str <- paste(deparse(formula), collapse = "")
    formula_str <- gsub(paste0("\\+\\s*", var_to_remove), "", formula_str)
    formula_str <- gsub(paste0(var_to_remove, "\\s*\\+"), "", formula_str)
    formula <- as.formula(formula_str)

    # Refit model
    model <- lm(formula, data = data)
    vif_vals <- car::vif(model)
  }

  list(model = model, iterations = iterations, vif = vif_vals)
}

# Run manual VIF removal
if (requireNamespace("car", quietly = TRUE)) {
  cat("Manual VIF removal (iterative):\n")
  manual_result <- manual_vif_removal(species_richness ~ ., data = bioclim_example, threshold = 5)
  cat("\nFinal VIF values:\n")
  print(round(manual_result$vif, 2))
  cat("\nTotal iterations:", manual_result$iterations, "\n")
}

## -----------------------------------------------------------------------------
# Run modelPrune
modelprune_result <- modelPrune(species_richness ~ ., data = bioclim_example, limit = 5)

cat("\nmodelPrune results:\n")
cat("Variables removed:", attr(modelprune_result, "removed_vars"), "\n")
cat("Variables kept:", length(attr(modelprune_result, "selected_vars")), "\n")

# Extract final model
final_model <- attr(modelprune_result, "final_model")
if (requireNamespace("car", quietly = TRUE)) {
  cat("\nFinal VIF values:\n")
  print(round(car::vif(final_model), 2))
}

## ----fig.width=8, fig.height=5, fig.alt="Side-by-side barplot showing VIF values before (red bars) and after (blue bars) applying modelPrune() for the top 15 variables ordered by initial VIF. Black horizontal dashed line marks the VIF limit of 5. Before pruning, many variables show high VIF values indicating severe multicollinearity. After modelPrune(), all retained variables have VIF below the threshold, and high-VIF variables are completely removed (shown as red-only bars), demonstrating automated and effective multicollinearity reduction."----
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
         bty    = "n")
}

## ----eval=FALSE---------------------------------------------------------------
# # Correlation pruning
# data_pruned <- corrPrune(raw_data, threshold = 0.7)
# 
# # VIF refinement
# model_data <- modelPrune(response ~ ., data = data_pruned, limit = 5)
# 
# # Importance testing (optional)
# if (requireNamespace("Boruta", quietly = TRUE)) {
#   boruta_result <- Boruta::Boruta(response ~ ., data = model_data)
#   important_vars <- names(boruta_result$finalDecision[
#     boruta_result$finalDecision == "Confirmed"
#   ])
# }
# 
# # Final model: OLS (interpretable) or glmnet (prediction)
# final_model <- lm(response ~ ., data = model_data[, c("response", important_vars)])

