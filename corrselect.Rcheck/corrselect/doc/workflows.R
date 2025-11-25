## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  cache = TRUE  # Cache expensive computations to speed up builds
)

## -----------------------------------------------------------------------------
library(corrselect)
data(bioclim_example)

# Data structure
dim(bioclim_example)
head(names(bioclim_example))

# Response variable
summary(bioclim_example$species_richness)

## -----------------------------------------------------------------------------
# Remove highly correlated predictors
bio_clean <- corrPrune(
  data = bioclim_example[, -1],  # Exclude response
  threshold = 0.7,
  mode = "auto"
)

# How much did we reduce?
cat(sprintf("Reduced from %d → %d variables\n",
            ncol(bioclim_example) - 1,
            ncol(bio_clean)))

# Which variables were kept?
head(attr(bio_clean, "selected_vars"), 10)

## ----fig.width=7, fig.height=5, fig.alt="Histogram showing distribution of absolute correlations before and after pruning. Red bars show many correlations exceed 0.7 threshold before pruning. Blue bars show all correlations below 0.7 after pruning."----
cor_before <- cor(bioclim_example[, -1])
cor_after  <- cor(bio_clean)

vals_before <- abs(cor_before[upper.tri(cor_before)])
vals_after  <- abs(cor_after[upper.tri(cor_after)])

# Common x limits
xlim <- c(0, 1)

# Shared breaks for fair comparison
breaks <- seq(0, 1, length.out = 30)

# Before histogram
hist(vals_before,
     breaks = breaks,
     freq = FALSE,
     main = "Distribution of Absolute Correlations",
     xlab = "Absolute Correlation",
     col = rgb(0.8, 0.2, 0.2, 0.4),
     border = "white",
     xlim = xlim)

# After histogram
hist(vals_after,
     breaks = breaks,
     freq = FALSE,
     col = rgb(0.2, 0.5, 0.8, 0.4),
     border = "white",
     add = TRUE)

# Threshold line
abline(v = 0.7, col = "black", lty = 2, lwd = 2)

legend("topright",
       legend = c("Before", "After", "Threshold"),
       fill   = c(rgb(0.8, 0.2, 0.2, 0.4),
                  rgb(0.2, 0.5, 0.8, 0.4),
                  NA),
       border = c("white", "white", NA),
       lty    = c(NA, NA, 2),
       lwd    = c(NA, NA, 2),
       col    = c(NA, NA, "black"),
       bty    = "n")

## -----------------------------------------------------------------------------
# Model 1: Full model (19 variables)
model_full <- lm(species_richness ~ ., data = bioclim_example)

# Model 2: After corrPrune (correlation-based pruning)
bio_clean_full <- data.frame(
  species_richness = bioclim_example$species_richness,
  bio_clean
)
model_corrprune <- lm(species_richness ~ ., data = bio_clean_full)

# Model 3: Sequential VIF-based refinement
bio_final <- modelPrune(
  formula = species_richness ~ .,
  data = bio_clean_full,
  limit = 5
)
model_final <- attr(bio_final, "final_model")

## -----------------------------------------------------------------------------
# Variable counts
n_full      <- 19
n_corrprune <- length(attr(bio_clean, "selected_vars"))
n_final     <- length(attr(bio_final, "selected_vars"))

# Compute condition numbers (measure of collinearity)
X_full      <- model.matrix(model_full)[, -1]
X_corrprune <- model.matrix(model_corrprune)[, -1]
X_final     <- model.matrix(model_final)[, -1]

kappa_full      <- kappa(X_full, exact = TRUE)
kappa_corrprune <- kappa(X_corrprune, exact = TRUE)
kappa_final     <- kappa(X_final, exact = TRUE)

# Summary table
comparison <- data.frame(
  Step = c("Full", "corrPrune", "+ modelPrune"),
  Predictors = c(n_full, n_corrprune, n_final),
  Adj_R2 = c(
    summary(model_full)$adj.r.squared,
    summary(model_corrprune)$adj.r.squared,
    summary(model_final)$adj.r.squared
  ),
  Kappa = c(kappa_full, kappa_corrprune, kappa_final)
)

print(comparison)

## ----fig.width=8, fig.height=5, fig.alt="Dual-axis line plot showing number of predictors (x-axis) versus adjusted R² (blue, left y-axis) and condition number κ (red, right y-axis, log scale). As predictors decrease from 19 to 8, adjusted R² remains high (above 0.7) while κ drops from over 10000 to below 100, demonstrating that pruning dramatically improves numerical stability with minimal loss of explanatory power."----
# Extract data
n_vars <- comparison$Predictors
adj_r2 <- comparison$Adj_R2
kappa  <- comparison$Kappa

# Left y-axis: Adjusted R²
par(mar = c(5, 4, 4, 4))  # extra space on the right for second axis

plot(
  n_vars, adj_r2,
  type = "b",
  pch  = 19,
  cex  = 1.5,
  col  = rgb(0.2, 0.5, 0.8, 1),
  lwd  = 2,
  xlab = "Number of Predictors",
  ylab = "Adjusted R²",
  ylim = c(0, 1),
  main = "Pruning Reduces Collinearity While Preserving Fit"
)

# Right y-axis: log10(κ)
log_kappa <- log10(kappa)

# Nice ylim for log10(κ)
ylim_right <- range(log_kappa, finite = TRUE)
ylim_right <- ylim_right * c(0.9, 1.1)

par(new = TRUE)
plot(
  n_vars, log_kappa,
  type = "b",
  pch  = 17,
  cex  = 1.5,
  col  = rgb(0.8, 0.2, 0.2, 1),
  lwd  = 2,
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = "",
  ylim = ylim_right
)

# Right-hand axis ticks (10, 100, 1K, 10K) within range
kappa_ticks <- c(10, 100, 1000, 10000)
log_ticks   <- log10(kappa_ticks)
log_ticks   <- log_ticks[log_ticks >= ylim_right[1] & log_ticks <= ylim_right[2]]

axis(4, at = log_ticks, labels = format(10^log_ticks, scientific = FALSE))
mtext("Condition Number (κ)", side = 4, line = 3)

# Legend centered at top
legend(
  "top",
  inset = 0.02,
  legend = c("Adjusted R² (higher better)", "κ (lower better)"),
  col    = c(
    rgb(0.2, 0.5, 0.8, 1),
    rgb(0.8, 0.2, 0.2, 1)
  ),
  pch    = c(19, 17),
  lwd    = 2,
  horiz  = TRUE,
  bty    = "o",
  bg     = "white",
  x.intersp = 0.8
)


## ----fig.width=7, fig.height=5, fig.alt="Bar chart comparing regression coefficients between full model (19 variables, red bars) and pruned model (8 variables, blue bars). Variables dropped in pruning show only red bars with large magnitudes. Variables retained in both models show overlapping red-blue bars with more consistent effect sizes, demonstrating improved coefficient stability after pruning."----
# Extract coefficients (excluding intercept)
coef_full  <- coef(model_full)[-1]
coef_final <- coef(model_final)[-1]

# Use *all* variables (not just common ones)
all_vars <- union(names(coef_full), names(coef_final))

# Align coefficients to the same full variable list
vals_full   <- coef_full[all_vars]
vals_pruned <- coef_final[all_vars]

# Replace missing values (variables dropped in a model) with 0
vals_full[is.na(vals_full)]     <- 0
vals_pruned[is.na(vals_pruned)] <- 0

# Colours
col_full   <- rgb(0.8, 0.2, 0.2, 0.5)
col_pruned <- rgb(0.2, 0.5, 0.8, 0.5)

x <- seq_along(all_vars)

# Symmetric Y range
ylim <- range(c(vals_full, vals_pruned)) * 1.15

# Empty plot first
plot(
  x, vals_full,
  type = "n",
  xaxt = "n",
  xlab = "",
  ylab = "Coefficient",
  main = "Coefficient Comparison (Full vs modelPrune)",
  ylim = ylim
)

axis(1, at = x, labels = all_vars, las = 2, cex.axis = 0.7)

# Full model bars
rect(
  x - 0.4, 0,
  x + 0.4, vals_full,
  col = col_full, border = NA
)

# Pruned model bars
rect(
  x - 0.4, 0,
  x + 0.4, vals_pruned,
  col = col_pruned, border = NA
)

legend(
  "topright",
  legend = c(
    sprintf("Full (%d vars)", n_full),
    sprintf("Final (%d vars)", n_final)
  ),
  fill = c(col_full, col_pruned),
  border = "white",
  bty = "n"
)


## -----------------------------------------------------------------------------
data(survey_example)

# Data structure
dim(survey_example)
str(survey_example[, 1:10])  # First 10 columns

## -----------------------------------------------------------------------------
# Exclude respondent_id, overall_satisfaction, and factor variables
survey_predictors <- survey_example[, !(names(survey_example) %in%
                                         c("respondent_id", "overall_satisfaction",
                                           "gender", "education"))]

# Convert ordered factors (Likert items 1-7) to numeric for correlation analysis
survey_numeric <- as.data.frame(lapply(survey_predictors, function(x) {
  if (is.ordered(x)) as.numeric(as.character(x)) else as.numeric(x)
}))

# Prune with protected variables
survey_clean <- corrPrune(
  data = survey_numeric,
  threshold = 0.6,
  force_in = "age"
)

# How many items remain?
cat(sprintf("Reduced from %d → %d variables\n",
            ncol(survey_numeric),
            ncol(survey_clean)))

# Which items were kept?
selected <- attr(survey_clean, "selected_vars")
print(selected)

## -----------------------------------------------------------------------------
# Count items per construct
satisfaction_kept <- sum(grepl("satisfaction_", selected))
engagement_kept <- sum(grepl("engagement_", selected))
loyalty_kept <- sum(grepl("loyalty_", selected))

cat(sprintf("Satisfaction: %d/10 items kept\n", satisfaction_kept))
cat(sprintf("Engagement: %d/10 items kept\n", engagement_kept))
cat(sprintf("Loyalty: %d/10 items kept\n", loyalty_kept))

## ----fig.width=8, fig.height=5, fig.alt="Two side-by-side barplots. Left panel shows items kept per construct (Satisfaction, Engagement, Loyalty) with gray bars for original 10 items and blue bars showing reduced counts after pruning, demonstrating balanced retention across all three constructs. Right panel shows total variable reduction from 31 to approximately 10 variables using salmon and light green bars."----
par(mfrow = c(1, 2))

# Items kept per construct
construct_data <- rbind(
  c(10, 10, 10),
  c(satisfaction_kept, engagement_kept, loyalty_kept)
)

barplot(construct_data,
        beside = TRUE,
        names.arg = c("Satisfaction", "Engagement", "Loyalty"),
        col = c("lightgray", "lightblue"),
        legend.text = c("Original (10)", "After pruning"),
        args.legend = list(x = "topright", bty = "n"),
        main = "Items per Construct",
        ylab = "Number of Items",
        ylim = c(0, 12))

# Percentage reduction
barplot(c(ncol(survey_numeric), ncol(survey_clean)),
        names.arg = c("Before", "After"),
        col = c("salmon", "lightgreen"),
        main = "Total Variables",
        ylab = "Count",
        ylim = c(0, max(ncol(survey_numeric)) * 1.2))
text(0.7, ncol(survey_numeric) + 1, ncol(survey_numeric), pos = 3)
text(1.9, ncol(survey_clean) + 1, ncol(survey_clean), pos = 3)

## -----------------------------------------------------------------------------
# Add response back
survey_model_data <- data.frame(
  overall_satisfaction = survey_example$overall_satisfaction,
  survey_clean
)

# Fit regression model
model_survey <- lm(overall_satisfaction ~ ., data = survey_model_data)

# Summary
summary(model_survey)

## -----------------------------------------------------------------------------
# Full model (all 30 items + demographics)
full_survey_data <- data.frame(
  overall_satisfaction = survey_example$overall_satisfaction,
  survey_predictors
)

model_full_survey <- lm(overall_satisfaction ~ ., data = full_survey_data)

# Compare
data.frame(
  Model = c("Full (33 vars)", "Pruned (10 vars)"),
  R2 = c(summary(model_full_survey)$r.squared,
         summary(model_survey)$r.squared),
  Adj_R2 = c(summary(model_full_survey)$adj.r.squared,
             summary(model_survey)$adj.r.squared),
  Num_Predictors = c(33, 10)
)

## -----------------------------------------------------------------------------
data(genes_example)

# Data structure
dim(genes_example)

# Disease prevalence
table(genes_example$disease_status)

## -----------------------------------------------------------------------------
# Extract gene expression data (exclude ID and outcome)
gene_expr <- genes_example[, -(1:2)]

# Greedy pruning
system.time({
  genes_pruned <- corrPrune(
    data = gene_expr,
    threshold = 0.8,
    mode = "greedy"  # Fast for large p
  )
})

# Reduction
cat(sprintf("Reduced from %d → %d genes\n",
            ncol(gene_expr),
            ncol(genes_pruned)))

## ----fig.width=8, fig.height=5, fig.alt="Barplot comparing gene counts before and after pruning. Salmon bar shows 200 original genes, light blue bar shows approximately 100 genes after pruning (50% retained). Text labels display exact counts and retention percentage, demonstrating substantial dimensionality reduction while maintaining low pairwise correlations."----
# Barplot showing reduction
reduction_data <- c(ncol(gene_expr), ncol(genes_pruned))
barplot(reduction_data,
        names.arg = c("Original", "After Pruning"),
        main = "Gene Dimensionality Reduction",
        ylab = "Number of Genes",
        col = c("salmon", "lightblue"),
        ylim = c(0, max(reduction_data) * 1.2))
text(0.7, reduction_data[1] + 10, paste(reduction_data[1], "genes"), pos = 3)
text(1.9, reduction_data[2] + 10, paste(reduction_data[2], "genes\n(",
     round(100 * reduction_data[2] / reduction_data[1], 1), "% retained)"), pos = 3)

## -----------------------------------------------------------------------------
library(microbenchmark)

# Subset for comparison (use smaller subset for vignette build speed)
gene_subset <- gene_expr[, 1:20]  # Reduced from 50 to 20 for faster builds

# Benchmark exact mode
exact_time <- median(microbenchmark(
  exact_result <- corrPrune(gene_subset, threshold = 0.8, mode = "exact"),
  times = 3,
  unit = "ms"
)$time) / 1e6  # Convert nanoseconds to milliseconds

# Benchmark greedy mode
greedy_time <- median(microbenchmark(
  greedy_result <- corrPrune(gene_subset, threshold = 0.8, mode = "greedy"),
  times = 3,
  unit = "ms"
)$time) / 1e6  # Convert nanoseconds to milliseconds

# Run once more to get actual results for comparison
exact_result <- corrPrune(gene_subset, threshold = 0.8, mode = "exact")
greedy_result <- corrPrune(gene_subset, threshold = 0.8, mode = "greedy")

# Compare
cat(sprintf("Exact mode: %d genes kept (%.1f ms)\n", ncol(exact_result), exact_time))
cat(sprintf("Greedy mode: %d genes kept (%.1f ms)\n", ncol(greedy_result), greedy_time))
cat(sprintf("Speedup: %.1fx faster\n", exact_time / greedy_time))

## -----------------------------------------------------------------------------
# Prepare classification data
classification_data <- data.frame(
  disease_status = genes_example$disease_status,
  genes_pruned
)

# Logistic regression
model_genes <- glm(disease_status ~ .,
                   data = classification_data,
                   family = binomial())

# Prediction accuracy
predictions <- ifelse(predict(model_genes, type = "response") > 0.5,
                     "Disease", "Healthy")
accuracy <- mean(predictions == genes_example$disease_status)

cat(sprintf("Classification accuracy: %.1f%%\n", accuracy * 100))

## -----------------------------------------------------------------------------
data(longitudinal_example)

# Data structure
dim(longitudinal_example)
head(longitudinal_example)

# Study design
cat(sprintf("Subjects: %d\n", length(unique(longitudinal_example$subject))))
cat(sprintf("Sites: %d\n", length(unique(longitudinal_example$site))))
cat(sprintf("Observations per subject: %d\n",
            nrow(longitudinal_example) / length(unique(longitudinal_example$subject))))

## ----eval=FALSE---------------------------------------------------------------
# # Note: This example requires lme4 package
# library(lme4)
# 
# # Define formula with random effects
# # Note: Only fixed effects (x1-x5) will be pruned
# #       Random effects (1|subject), (1|site) are preserved
# 
# pruned_mixed <- modelPrune(
#   formula = outcome ~ x1 + x2 + x3 + x4 + x5 + (1|subject) + (1|site),
#   data = longitudinal_example,
#   engine = "lme4",
#   limit = 5
# )
# 
# # Which fixed effects were kept?
# selected_fixed <- attr(pruned_mixed, "selected_vars")
# cat("Fixed effects kept:\n")
# print(selected_fixed)
# 
# # Which were removed?
# removed_fixed <- attr(pruned_mixed, "removed_vars")
# cat("\nFixed effects removed:\n")
# print(removed_fixed)

## ----eval=FALSE---------------------------------------------------------------
# final_mixed <- attr(pruned_mixed, "final_model")
# summary(final_mixed)

## ----eval=FALSE---------------------------------------------------------------
# # Note: This example requires lme4 package
# library(lme4)
# 
# # Fit full model
# full_formula <- as.formula(paste("outcome ~",
#                                  paste(paste0("x", 1:5), collapse = " + "),
#                                  "+ (1|subject) + (1|site)"))
# 
# model_full_mixed <- lmer(full_formula, data = longitudinal_example)
# 
# # Extract fixed effects design matrices
# X_full <- getME(model_full_mixed, "X")
# X_pruned <- getME(final_mixed, "X")
# 
# # Compute VIF
# compute_vif <- function(X) {
#   X_scaled <- scale(X[, -1])  # Remove intercept
#   sapply(seq_len(ncol(X_scaled)), function(i) {
#     r2 <- summary(lm(X_scaled[, i] ~ X_scaled[, -i]))$r.squared
#     1 / (1 - r2)
#   })
# }
# 
# vif_full <- compute_vif(X_full)
# vif_pruned <- compute_vif(X_pruned)
# 
# # Compare
# comparison_vif <- data.frame(
#   Predictor = colnames(X_pruned)[-1],
#   VIF_Before = vif_full,
#   VIF_After = vif_pruned
# )
# print(comparison_vif)

## ----eval=FALSE, fig.width=8, fig.height=5, fig.alt="Side-by-side barplot showing VIF values for fixed effects before (red bars) and after (blue bars) pruning in a mixed-effects model. Red dashed horizontal line marks VIF threshold of 5. Before pruning, several predictors exceed the threshold with VIF values above 5. After pruning, all retained predictors have VIF below 5, demonstrating successful multicollinearity reduction while preserving random effects structure."----
# # Extract VIF values
# predictors <- comparison_vif$Predictor
# vif_before <- comparison_vif$VIF_Before
# vif_after <- comparison_vif$VIF_After
# 
# # Set up bar positions
# x <- seq_along(predictors)
# width <- 0.35
# 
# # Create plot
# par(mar = c(5, 4, 4, 2))
# plot(
#   x, vif_before,
#   type = "n",
#   xaxt = "n",
#   xlab = "Fixed Effects",
#   ylab = "VIF",
#   main = "VIF Reduction After Pruning (Mixed Model)",
#   ylim = c(0, max(vif_before) * 1.15)
# )
# 
# # Add horizontal line at VIF = 5 threshold
# abline(h = 5, col = "red", lty = 2, lwd = 2)
# 
# # Before bars (darker)
# rect(
#   x - width, 0,
#   x, vif_before,
#   col = rgb(0.8, 0.2, 0.2, 0.6),
#   border = "white"
# )
# 
# # After bars (lighter)
# rect(
#   x, 0,
#   x + width, vif_after,
#   col = rgb(0.2, 0.5, 0.8, 0.6),
#   border = "white"
# )
# 
# # Add x-axis labels
# axis(1, at = x, labels = predictors, las = 2)
# 
# # Add legend
# legend(
#   "topright",
#   legend = c("Before Pruning", "After Pruning", "VIF = 5 Threshold"),
#   fill = c(rgb(0.8, 0.2, 0.2, 0.6), rgb(0.2, 0.5, 0.8, 0.6), NA),
#   border = c("white", "white", NA),
#   lty = c(NA, NA, 2),
#   lwd = c(NA, NA, 2),
#   col = c(NA, NA, "red"),
#   bty = "n"
# )

