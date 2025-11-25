## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(corrselect)
data(bioclim_example)

# Remove correlated predictors (threshold = 0.7)
pruned <- corrPrune(
  data = bioclim_example[, -1],  # Exclude response
  threshold = 0.7
)

# Variables retained
ncol(pruned)
head(names(pruned))

## -----------------------------------------------------------------------------
attr(pruned, "removed_vars")

## ----fig.width=7, fig.height=5, fig.alt="Histogram showing distribution of absolute correlations before (red bars) and after (blue bars) pruning with corrPrune(). Black vertical dashed line marks the 0.7 threshold. Before pruning, many correlations exceed the threshold. After pruning, all pairwise correlations remain below 0.7, demonstrating successful redundancy reduction."----
cor_before <- cor(bioclim_example[, -1])
cor_after <- cor(pruned)

hist(abs(cor_before[upper.tri(cor_before)]),
     breaks = 30,
     main = "Distribution of Absolute Correlations",
     xlab = "Absolute Correlation",
     col = rgb(0.8, 0.2, 0.2, 0.5),
     xlim = c(0, 1),
     ylim = c(0, max(table(cut(abs(cor_before[upper.tri(cor_before)]), breaks = 30))) * 1.2))

hist(abs(cor_after[upper.tri(cor_after)]),
     breaks = 30,
     col = rgb(0.2, 0.5, 0.8, 0.5),
     add = TRUE)

abline(v = 0.7, col = "black", lwd = 2, lty = 2)
legend("topright",
       legend = c("Before", "After", "Threshold"),
       fill = c(rgb(0.8, 0.2, 0.2, 0.5), rgb(0.2, 0.5, 0.8, 0.5), NA),
       border = c("black", "black", NA),
       lty = c(NA, NA, 2),
       lwd = c(NA, NA, 2),
       bty = "n")

## -----------------------------------------------------------------------------
# Prune based on VIF (limit = 5)
model_data <- modelPrune(
  formula = species_richness ~ .,
  data = bioclim_example,
  limit = 5
)

# Predictors retained
length(attr(model_data, "selected_vars"))

## -----------------------------------------------------------------------------
attr(model_data, "removed_vars")

## -----------------------------------------------------------------------------
final_model <- attr(model_data, "final_model")
summary(final_model)$coefficients[1:5, ]  # First 5 coefficients

## ----fig.width=8, fig.height=5, fig.alt="Side-by-side barplot comparing VIF values before (red bars) and after (blue bars) modelPrune() for up to 15 variables. Black horizontal dashed line marks the VIF threshold of 5. Before pruning, several variables exceed the limit with high VIF values. After pruning, all retained variables have VIF below 5, and high-VIF variables are removed (shown as red-only bars), demonstrating successful multicollinearity reduction."----
# Increase bottom margin for rotated axis labels
par(mar = c(8, 4, 4, 2))

full_model <- lm(species_richness ~ ., data = bioclim_example)

X_full <- model.matrix(full_model)[, -1]
vif_before <- sapply(colnames(X_full), function(var) {
  1 / (1 - summary(lm(X_full[, var] ~ X_full[, -which(colnames(X_full) == var)]))$r.squared)
})

X_pruned <- model.matrix(final_model)[, -1]
vif_after <- sapply(colnames(X_pruned), function(var) {
  1 / (1 - summary(lm(X_pruned[, var] ~ X_pruned[, -which(colnames(X_pruned) == var)]))$r.squared)
})

all_vars <- unique(c(names(vif_before), names(vif_after)))
vif_combined <- data.frame(
  before = vif_before[match(all_vars, names(vif_before))],
  after  = vif_after[match(all_vars, names(vif_after))]
)
vif_combined[is.na(vif_combined)] <- 0
vif_combined <- vif_combined[order(vif_combined$before, decreasing = TRUE), ]

n_show <- min(15, nrow(vif_combined))

barplot(
  t(as.matrix(vif_combined[1:n_show, ])),
  beside     = TRUE,
  las        = 2,
  main       = "VIF Before and After modelPrune()",
  ylab       = "VIF",
  col        = c(
    rgb(0.8, 0.2, 0.2, 0.7),
    rgb(0.2, 0.5, 0.8, 0.7)
  ),
  cex.names  = 0.6,
  names.arg  = rownames(vif_combined)[1:n_show]
)

abline(h = 5, col = "black", lwd = 2, lty = 2)

legend("topright",
       legend = c("Before", "After", "Limit"),
       fill   = c(
         rgb(0.8, 0.2, 0.2, 0.7),
         rgb(0.2, 0.5, 0.8, 0.7),
         NA
       ),
       border = c("black", "black", NA),
       lty    = c(NA, NA, 2),
       lwd    = c(NA, NA, 2),
       bty    = "o",    # opaque box
       bg     = "white" # hides dashed line behind legend
)


## -----------------------------------------------------------------------------
results <- corrSelect(bioclim_example[, -1], threshold = 0.7)
show(results)

## -----------------------------------------------------------------------------
as.data.frame(results)[1:5, ]  # First 5 subsets

## -----------------------------------------------------------------------------
subset_data <- corrSubset(results, bioclim_example[, -1], which = 1)
head(names(subset_data))

## -----------------------------------------------------------------------------
data(survey_example)

# Exclude ID and response
survey_predictors <- survey_example[, !(names(survey_example) %in%
                                         c("respondent_id", "overall_satisfaction"))]

# Handle mixed types
results_mixed <- assocSelect(survey_predictors, threshold = 0.6)
show(results_mixed)

## -----------------------------------------------------------------------------
# Check available variable names
head(names(bioclim_example[, -1]))

# Force a variable to remain in all subsets
first_var <- names(bioclim_example)[2]  # First predictor
pruned_force <- corrPrune(
  data = bioclim_example[, -1],
  threshold = 0.7,
  force_in = first_var
)

# Verify forced variable is present
first_var %in% names(pruned_force)

## ----fig.width=7, fig.height=5, fig.alt="Histogram of absolute correlation distribution with three colored vertical dashed lines indicating threshold levels: strict at 0.5 (red), moderate at 0.7 (blue), and lenient at 0.9 (green). The distribution helps users visualize correlation structure and choose an appropriate threshold based on the data's correlation characteristics."----
cor_mat <- cor(bioclim_example[, -1])
cor_vec <- cor_mat[upper.tri(cor_mat)]

# Histogram with fixed x-limits so the 0.9 line is always in view
hist(abs(cor_vec), breaks = 30,
     main = "Distribution of Absolute Correlations",
     xlab = "Absolute Correlation",
     col = rgb(0.2, 0.5, 0.8, 0.6),
     border = "white",
     xlim = c(0, 1))

# Threshold lines (all visible because of xlim)
abline(v = c(0.5, 0.7, 0.9),
       col = c("#d73027", "#4575b4", "#91cf60"),
       lwd = 2, lty = 2)

# Legend with opaque background so dashed lines don't pass through
legend("topright",
       legend = c("0.5 (strict)", "0.7 (moderate)", "0.9 (lenient)"),
       col = c("#d73027", "#4575b4", "#91cf60"),
       lwd = 2, lty = 2,
       bty = "o",           # draw a box
       bg  = "white")       # solid white background so lines stay hidden


## ----fig.width=7, fig.height=5, fig.alt="Line plot showing threshold sensitivity analysis. X-axis shows correlation thresholds from 0.3 to 0.95, y-axis shows number of variables retained. Blue line with dots shows how the number of retained variables increases as threshold becomes more lenient. Horizontal gray dashed line marks the original number of variables. The curve helps identify the plateau region where additional threshold relaxation yields diminishing returns."----
thresholds <- seq(0.3, 0.95, by = 0.05)
sizes <- sapply(thresholds, function(t) {
  tryCatch(ncol(corrPrune(bioclim_example[, -1], threshold = t)), error = function(e) NA)
})

plot(thresholds, sizes, type = "b",
     xlab = "Threshold",
     ylab = "Variables Retained",
     main = "Threshold Sensitivity",
     col = rgb(0.2, 0.5, 0.8), lwd = 2, pch = 19)
abline(h = ncol(bioclim_example) - 1, lty = 2, col = "gray50")

