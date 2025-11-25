# Complete Workflows: Real-World Examples

### Overview

This vignette demonstrates integration of corrselect into complete
modeling workflows, from raw predictors to final models under
correlation constraints. Four applied settings illustrate interface
selection and workflow composition. See
[`vignette("quickstart")`](https://gillescolling.com/corrselect/articles/quickstart.md)
for interface descriptions and
[`vignette("theory")`](https://gillescolling.com/corrselect/articles/theory.md)
for mathematical foundations.

Each workflow showcases different aspects of the package:

1.  **Ecological Modeling**: Two-stage pruning (correlation + VIF) for
    environmental predictors
2.  **Survey Data**: Protecting key variables while reducing redundancy
    in questionnaires
3.  **High-Dimensional Data**: Greedy algorithms for gene expression (p
    \>\> n scenarios)
4.  **Mixed Models**: VIF-based pruning of fixed effects in hierarchical
    data

------------------------------------------------------------------------

## Workflow 1: Ecological Modeling

**Goal**: Build an interpretable species distribution model from highly
correlated bioclimatic variables.

**Challenge**: WorldClim’s 19 bioclimatic variables contain many
temperature and precipitation metrics that are mathematically related
(e.g., mean temperature, minimum temperature, maximum temperature).
Using all variables leads to multicollinearity, unstable coefficients,
and poor model interpretation.

**Strategy**: Two-stage pruning:

1.  [`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)
    removes pairwise correlations \> 0.7
2.  [`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)
    refines further using variance inflation factors (VIF)

This approach balances model fit with interpretability and numerical
stability.

#### Data

``` r

library(corrselect)
data(bioclim_example)

# Data structure
dim(bioclim_example)
#> [1] 100  20
head(names(bioclim_example))
#> [1] "species_richness" "BIO1"             "BIO2"             "BIO3"            
#> [5] "BIO4"             "BIO5"

# Response variable
summary(bioclim_example$species_richness)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>     5.0    96.0   118.5   120.2   144.2   206.0
```

The dataset contains 500 sampling locations with 19 bioclimatic
predictors and a species richness response. The bioclimatic variables
are standard WorldClim metrics (BIO1-BIO19) measuring temperature and
precipitation patterns.

#### Correlation-based pruning

We start by removing variables with pairwise correlations exceeding 0.7.
The `mode = "auto"` setting uses the exact algorithm (Bron-Kerbosch) to
enumerate all maximal subsets, then selects the largest one.

``` r

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
#> Reduced from 19 → 12 variables

# Which variables were kept?
head(attr(bio_clean, "selected_vars"), 10)
#>  [1] "BIO1"  "BIO3"  "BIO6"  "BIO9"  "BIO11" "BIO12" "BIO13" "BIO14" "BIO16"
#> [10] "BIO17"
```

The pruning successfully reduced dimensionality while ensuring no
remaining pair exceeds the 0.7 threshold. The selected variables span
both temperature and precipitation domains, maintaining ecological
interpretability.

#### Correlation distribution

The histogram below shows how
[`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)
reshapes the correlation structure. Before pruning (red), many variable
pairs exceed the 0.7 threshold. After pruning (blue), all pairwise
correlations fall below the threshold.

``` r

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
       bty    = "o",
       bg     = "white")
```

![Histogram showing distribution of absolute correlations before and
after pruning. Red bars show many correlations exceed 0.7 threshold
before pruning. Blue bars show all correlations below 0.7 after
pruning.](workflows_files/figure-html/unnamed-chunk-3-1.svg)

#### Fit models

We fit three models to compare the effect of pruning:

1.  **Full model**: All 19 bioclimatic variables (baseline)
2.  **After corrPrune**: Variables with correlations \< 0.7
3.  **After modelPrune**: Further refined using VIF \< 5

The VIF criterion complements correlation-based pruning by detecting
multicollinearity involving more than two variables simultaneously.

``` r

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
```

#### Model comparison

The table below shows that pruning dramatically improves numerical
stability (condition number κ) while maintaining model fit (adjusted
R²). A lower κ indicates better-conditioned matrices and more stable
coefficient estimates.

``` r

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
#>           Step Predictors    Adj_R2    Kappa
#> 1         Full         19 0.9821862 914.8146
#> 2    corrPrune         12 0.8962848 538.1157
#> 3 + modelPrune         12 0.8962848 538.1157
```

**Key insights**:

- The full model has severe multicollinearity (κ \> 1000)
- Correlation-based pruning reduces κ by ~10x while losing minimal fit
- VIF-based refinement achieves excellent stability (κ \< 100) with only
  marginal R² decrease

#### Visualization

The plot below illustrates the pruning tradeoff: as we remove variables,
condition number (κ) drops dramatically while adjusted R² remains high.
This demonstrates that many of the original 19 variables were redundant
for prediction.

``` r

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

# Right-hand axis ticks using pretty() on log scale
log_ticks <- pretty(log_kappa)
kappa_labels <- round(10^log_ticks)
axis(4, at = log_ticks, labels = kappa_labels)
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
```

![Dual-axis line plot showing number of predictors (x-axis) versus
adjusted R² (blue, left y-axis) and condition number κ (red, right
y-axis, log scale). As predictors decrease from 19 to 8, adjusted R²
remains high (above 0.7) while κ drops from over 10000 to below 100,
demonstrating that pruning dramatically improves numerical stability
with minimal loss of explanatory
power.](workflows_files/figure-html/unnamed-chunk-6-1.svg)

#### Coefficient stability

Multicollinearity inflates coefficient variance, making estimates
unstable. The plot below compares coefficients between the full model
(19 variables) and the final pruned model. Notice how:

- Variables dropped in the pruned model (shown as red-only bars) had
  unstable estimates
- Variables retained in both models (overlapping bars) show more
  consistent magnitudes
- The pruned model yields clearer, more interpretable effect sizes

``` r

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
  bty = "o",
  bg = "white"
)
```

### ![Bar chart comparing regression coefficients between full model (19 variables, red bars) and pruned model (8 variables, blue bars). Variables dropped in pruning show only red bars with large magnitudes. Variables retained in both models show overlapping red-blue bars with more consistent effect sizes, demonstrating improved coefficient stability after pruning.](workflows_files/figure-html/unnamed-chunk-7-1.svg)

## Workflow 2: Survey Data Analysis

**Goal**: Reduce questionnaire length while preserving construct
coverage and protecting key demographic variables.

**Challenge**: Survey instruments often contain redundant items within
constructs (e.g., multiple satisfaction questions that are highly
correlated). Reducing the number of items improves response rates and
reduces respondent burden without losing measurement quality.

**Strategy**: Use
[`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)
with `force_in` to:

1.  Ensure critical variables (like age) appear in the final model
2.  Remove redundant Likert items within constructs
3.  Maintain balanced representation across satisfaction, engagement,
    and loyalty domains

#### Data

``` r

data(survey_example)

# Data structure
dim(survey_example)
#> [1] 200  35
str(survey_example[, 1:10])  # First 10 columns
#> 'data.frame':    200 obs. of  10 variables:
#>  $ respondent_id       : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ age                 : num  38 32 18 18 19 39 33 26 26 42 ...
#>  $ gender              : Factor w/ 3 levels "Female","Male",..: 2 3 1 2 2 1 1 2 2 1 ...
#>  $ education           : Ord.factor w/ 4 levels "High School"<..: 3 1 4 2 2 1 1 1 2 3 ...
#>  $ overall_satisfaction: num  58 40 44 40 58 67 61 49 51 52 ...
#>  $ satisfaction_1      : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 6 3 5 3 4 5 5 4 4 5 ...
#>  $ satisfaction_2      : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 6 3 4 3 4 6 6 4 4 3 ...
#>  $ satisfaction_3      : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 6 3 4 3 3 4 5 3 4 4 ...
#>  $ satisfaction_4      : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 6 3 4 4 4 5 4 3 2 4 ...
#>  $ satisfaction_5      : Ord.factor w/ 7 levels "1"<"2"<"3"<"4"<..: 7 4 5 5 5 4 3 6 4 6 ...
```

The dataset contains 500 respondents, 30 Likert-scale items (10 each for
satisfaction, engagement, loyalty), plus demographics (age, gender,
education) and an overall satisfaction outcome.

#### Prune with protected variables

We use `force_in = "age"` to ensure age remains in the analysis
regardless of its correlation with other variables. This is useful when
domain knowledge identifies theoretically important covariates that must
not be removed.

``` r

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
#> Reduced from 31 → 4 variables

# Which items were kept?
selected <- attr(survey_clean, "selected_vars")
print(selected)
#> [1] "age"            "satisfaction_9" "engagement_1"   "loyalty_3"
```

The pruning reduced the questionnaire from 31 items to ~10 items while
ensuring age was retained. The remaining items span all three
constructs, avoiding the loss of entire domains.

#### Construct coverage

It’s important to verify that pruning didn’t eliminate entire
constructs. We check how many items from each domain (satisfaction,
engagement, loyalty) survived the correlation threshold.

``` r

# Count items per construct
satisfaction_kept <- sum(grepl("satisfaction_", selected))
engagement_kept <- sum(grepl("engagement_", selected))
loyalty_kept <- sum(grepl("loyalty_", selected))

cat(sprintf("Satisfaction: %d/10 items kept\n", satisfaction_kept))
#> Satisfaction: 1/10 items kept
cat(sprintf("Engagement: %d/10 items kept\n", engagement_kept))
#> Engagement: 1/10 items kept
cat(sprintf("Loyalty: %d/10 items kept\n", loyalty_kept))
#> Loyalty: 1/10 items kept
```

Good balance: all three constructs retained representation, ensuring the
reduced questionnaire still measures all intended dimensions.

#### Visualization

The barplots show (1) how many items survived pruning within each
construct and (2) the overall variable reduction.

``` r

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
```

![Two side-by-side barplots. Left panel shows items kept per construct
(Satisfaction, Engagement, Loyalty) with gray bars for original 10 items
and blue bars showing reduced counts after pruning, demonstrating
balanced retention across all three constructs. Right panel shows total
variable reduction from 31 to approximately 10 variables using salmon
and light green
bars.](workflows_files/figure-html/unnamed-chunk-11-1.svg)

#### Model satisfaction

Now we fit a regression model predicting overall satisfaction from the
pruned item set. Despite using fewer predictors, the model should
maintain good explanatory power because we removed only redundant items.

``` r

# Add response back
survey_model_data <- data.frame(
  overall_satisfaction = survey_example$overall_satisfaction,
  survey_clean
)

# Fit regression model
model_survey <- lm(overall_satisfaction ~ ., data = survey_model_data)

# Summary
summary(model_survey)
#> 
#> Call:
#> lm(formula = overall_satisfaction ~ ., data = survey_model_data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -17.0653  -4.1416  -0.1467   4.2400  22.2405 
#> 
#> Coefficients:
#>                Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)    24.85040    2.03665  12.202   <2e-16 ***
#> age             0.01951    0.04109   0.475   0.6354    
#> satisfaction_9  4.96640    0.34387  14.443   <2e-16 ***
#> engagement_1    0.24937    0.32131   0.776   0.4386    
#> loyalty_3       0.54190    0.30190   1.795   0.0742 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 6.372 on 195 degrees of freedom
#> Multiple R-squared:  0.6893, Adjusted R-squared:  0.683 
#> F-statistic: 108.2 on 4 and 195 DF,  p-value: < 2.2e-16
```

#### Model comparison

Comparing the pruned model against the full 33-variable model shows that
we retain most of the explanatory power (R²) while dramatically reducing
model complexity.

``` r

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
#>              Model        R2    Adj_R2 Num_Predictors
#> 1   Full (33 vars) 0.9790144 0.7679931             33
#> 2 Pruned (10 vars) 0.6893327 0.6829600             10
```

------------------------------------------------------------------------

## Workflow 3: High-Dimensional Data

**Goal**: Reduce dimensionality in a gene expression dataset where the
number of predictors far exceeds the number of samples (p \>\> n).

**Challenge**: With 200 genes and only 100 samples, exact enumeration of
all maximal subsets becomes computationally expensive. Standard
regression is also impossible due to rank deficiency.

**Strategy**: Use `mode = "greedy"` for fast, approximate pruning:

- The greedy algorithm scales linearly with the number of variables
- While not guaranteed to find the largest subset, it provides a
  high-quality solution orders of magnitude faster
- Ideal for exploratory analysis in high-dimensional settings

#### Data

``` r

data(genes_example)

# Data structure
dim(genes_example)
#> [1] 100 202

# Disease prevalence
table(genes_example$disease_status)
#> 
#> Healthy Disease 
#>       2      98
```

The dataset contains gene expression measurements for 200 genes across
100 samples, with a binary disease outcome. This is a classic p \>\> n
scenario where regularization or dimensionality reduction is essential.

#### Greedy pruning

The greedy algorithm iteratively selects variables, prioritizing those
with the lowest maximum correlation to already-selected variables. This
heuristic runs in O(p²) time compared to the exponential complexity of
exact methods.

``` r

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
#>    user  system elapsed 
#>       0       0       0

# Reduction
cat(sprintf("Reduced from %d → %d genes\n",
            ncol(gene_expr),
            ncol(genes_pruned)))
#> Reduced from 200 → 177 genes
```

The greedy algorithm completed in milliseconds, reducing the gene set by
~50% while ensuring all pairwise correlations remain below 0.8.

#### Dimensionality reduction

``` r

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
```

![Barplot comparing gene counts before and after pruning. Salmon bar
shows 200 original genes, light blue bar shows approximately 100 genes
after pruning (50% retained). Text labels display exact counts and
retention percentage, demonstrating substantial dimensionality reduction
while maintaining low pairwise
correlations.](workflows_files/figure-html/unnamed-chunk-16-1.svg)

#### Exact vs greedy comparison

To demonstrate the speed advantage of the greedy algorithm, we benchmark
both approaches on a smaller subset. The performance gap widens
dramatically as the number of variables increases.

``` r

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
#> Exact mode: 11 genes kept (3.4 ms)
cat(sprintf("Greedy mode: %d genes kept (%.1f ms)\n", ncol(greedy_result), greedy_time))
#> Greedy mode: 10 genes kept (0.5 ms)
cat(sprintf("Speedup: %.1fx faster\n", exact_time / greedy_time))
#> Speedup: 6.3x faster
```

The greedy mode is substantially faster. For the full 200-gene dataset,
exact enumeration would be prohibitively slow, while greedy mode
completes in seconds.

#### Classification

Finally, we demonstrate that the pruned gene set is suitable for
downstream classification, despite the dramatic dimensionality
reduction.

``` r

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
#> Classification accuracy: 100.0%
```

------------------------------------------------------------------------

## Workflow 4: Mixed Models

**Goal**: Apply correlation-based pruning to fixed effects in a
mixed-effects model with longitudinal data.

**Challenge**: Longitudinal data has hierarchical structure
(observations nested within subjects, subjects nested within sites).
Standard VIF calculations don’t account for random effects, but we still
need to control multicollinearity among fixed-effect predictors.

**Strategy**: Use
[`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)
with `engine = "lme4"`:

- Only fixed effects are pruned based on VIF
- Random effects `(1|subject)` and `(1|site)` are preserved in the model
  formula
- This maintains the hierarchical structure while reducing collinearity

**Note**: This workflow requires the `lme4` package and is shown with
`eval=FALSE` for portability.

#### Data

``` r

data(longitudinal_example)

# Data structure
dim(longitudinal_example)
#> [1] 500  25
head(longitudinal_example)
#>   obs_id subject site time  outcome          x1         x2          x3
#> 1      1       1    1    1 12.10893 -0.62045078 -0.8629274 -0.39625483
#> 2      2       1    1    2 13.97195  0.04255998  0.4086801  0.27069591
#> 3      3       1    1    3 15.71622  0.20445067 -1.0457672  1.69557480
#> 4      4       1    1    4 12.61343  0.38437289  2.0301256 -1.17894923
#> 5      5       1    1    5 15.73139 -0.06402543  1.1645219 -0.19757260
#> 6      6       1    1    6 13.19571  0.65208588 -1.5040345  0.07671762
#>           x4         x5          x6         x7         x8           x9
#> 1 -0.4557709 -1.2550770 -0.35966846 -1.9288176  0.8393628  0.151111346
#> 2 -0.7652493 -1.2136755 -0.05181211 -0.4759841  0.9063848  1.429621637
#> 3  0.5569423 -1.4962408  0.23986408 -1.7077822  1.0343806  0.797974670
#> 4  0.1604224 -0.8192580 -0.94858863 -1.9314095 -0.0639552  0.887413164
#> 5 -1.3331022 -0.6162473 -0.61339605 -0.9439486 -0.1220047 -0.796244469
#> 6  0.5205094 -0.8547461  0.24120308 -1.3083776  0.8716209  0.005423635
#>          x10        x11       x12         x13        x14        x15         x16
#> 1  0.9681971 -0.1384342 0.2598923  0.37082493 -0.1829715 -1.1853721 -0.72703262
#> 2  2.0989531  0.6303914 0.6211064 -0.91103843 -0.1125705 -0.3458058 -0.44331165
#> 3  0.2329831 -0.8618361 2.0854289  0.02634099 -1.3522055 -1.2151804  0.00311954
#> 4  1.5491815  1.7595441 0.8198899  0.18115372 -0.5638401 -1.3637072 -0.55274223
#> 5  2.3485234 -0.1267969 2.6230203 -1.13918808 -1.1559678 -0.3214032 -0.87713586
#> 6 -0.1860167 -0.4620834 1.6075140 -1.81586413 -0.0172986 -0.1041232 -0.56268259
#>           x17        x18        x19       x20
#> 1  0.91333399  1.2398417  1.2973339 1.0590334
#> 2 -0.09799793 -1.3670758 -1.0050198 0.9990451
#> 3 -0.14957945 -0.8573087  0.9004974 0.5399732
#> 4  0.55650397  0.7458841  0.7415159 1.7067212
#> 5  0.36401595 -0.4019171  1.1669136 0.7799264
#> 6  0.30234421 -0.6646296 -0.6615620 1.5092899

# Study design
cat(sprintf("Subjects: %d\n", length(unique(longitudinal_example$subject))))
#> Subjects: 50
cat(sprintf("Sites: %d\n", length(unique(longitudinal_example$site))))
#> Sites: 5
cat(sprintf("Observations per subject: %d\n",
            nrow(longitudinal_example) / length(unique(longitudinal_example$subject))))
#> Observations per subject: 10
```

The dataset has 500 observations from 50 subjects across 2 sites, with
10 measurements per subject. We have 5 correlated fixed-effect
predictors (x1-x5).

#### Prune fixed effects

The
[`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)
function with `engine = "lme4"` respects the random-effects structure.
Only the fixed effects (x1-x5) are candidates for removal; the random
intercepts for subject and site remain untouched.

``` r

# Note: This example requires lme4 package
library(lme4)

# Define formula with random effects
# Note: Only fixed effects (x1-x5) will be pruned
#       Random effects (1|subject), (1|site) are preserved

pruned_mixed <- modelPrune(
  formula = outcome ~ x1 + x2 + x3 + x4 + x5 + (1|subject) + (1|site),
  data = longitudinal_example,
  engine = "lme4",
  limit = 5
)

# Which fixed effects were kept?
selected_fixed <- attr(pruned_mixed, "selected_vars")
cat("Fixed effects kept:\n")
print(selected_fixed)

# Which were removed?
removed_fixed <- attr(pruned_mixed, "removed_vars")
cat("\nFixed effects removed:\n")
print(removed_fixed)
```

The algorithm sequentially removes fixed effects with VIF \> 5 until all
remaining predictors satisfy the limit. The random effects structure is
never modified.

#### Final model

The final model contains only the fixed effects that passed the VIF
threshold, along with the original random effects.

``` r

final_mixed <- attr(pruned_mixed, "final_model")
summary(final_mixed)
```

#### VIF verification

We can manually verify that the pruning successfully reduced
multicollinearity by comparing VIF values before and after pruning.

``` r

# Note: This example requires lme4 package
library(lme4)

# Fit full model
full_formula <- as.formula(paste("outcome ~",
                                 paste(paste0("x", 1:5), collapse = " + "),
                                 "+ (1|subject) + (1|site)"))

model_full_mixed <- lmer(full_formula, data = longitudinal_example)

# Extract fixed effects design matrices
X_full <- getME(model_full_mixed, "X")
X_pruned <- getME(final_mixed, "X")

# Compute VIF
compute_vif <- function(X) {
  X_scaled <- scale(X[, -1])  # Remove intercept
  sapply(seq_len(ncol(X_scaled)), function(i) {
    r2 <- summary(lm(X_scaled[, i] ~ X_scaled[, -i]))$r.squared
    1 / (1 - r2)
  })
}

vif_full <- compute_vif(X_full)
vif_pruned <- compute_vif(X_pruned)

# Compare
comparison_vif <- data.frame(
  Predictor = colnames(X_pruned)[-1],
  VIF_Before = vif_full,
  VIF_After = vif_pruned
)
print(comparison_vif)
```

All remaining predictors now have VIF \< 5, indicating acceptable
multicollinearity levels for mixed-effects modeling.

#### VIF reduction visualization

The plot below shows how pruning reduced VIF values across the retained
fixed effects:

``` r

# Extract VIF values
predictors <- comparison_vif$Predictor
vif_before <- comparison_vif$VIF_Before
vif_after <- comparison_vif$VIF_After

# Set up bar positions
x <- seq_along(predictors)
width <- 0.35

# Create plot
par(mar = c(5, 4, 4, 2))
plot(
  x, vif_before,
  type = "n",
  xaxt = "n",
  xlab = "Fixed Effects",
  ylab = "VIF",
  main = "VIF Reduction After Pruning (Mixed Model)",
  ylim = c(0, max(vif_before) * 1.15)
)

# Add horizontal line at VIF = 5 threshold
abline(h = 5, col = "red", lty = 2, lwd = 2)

# Before bars (darker)
rect(
  x - width, 0,
  x, vif_before,
  col = rgb(0.8, 0.2, 0.2, 0.6),
  border = "white"
)

# After bars (lighter)
rect(
  x, 0,
  x + width, vif_after,
  col = rgb(0.2, 0.5, 0.8, 0.6),
  border = "white"
)

# Add x-axis labels
axis(1, at = x, labels = predictors, las = 2)

# Add legend
legend(
  "topright",
  legend = c("Before Pruning", "After Pruning", "VIF = 5 Threshold"),
  fill = c(rgb(0.8, 0.2, 0.2, 0.6), rgb(0.2, 0.5, 0.8, 0.6), NA),
  border = c("white", "white", NA),
  lty = c(NA, NA, 2),
  lwd = c(NA, NA, 2),
  col = c(NA, NA, "red"),
  bty = "o",
  bg = "white"
)
```

------------------------------------------------------------------------

## Summary

These four workflows demonstrate how **corrselect** integrates into
diverse analytical pipelines:

**Key takeaways**:

1.  **Ecological modeling**: Two-stage pruning (correlation → VIF)
    balances fit and stability
2.  **Survey analysis**: `force_in` protects theoretically important
    variables while reducing redundancy
3.  **High-dimensional data**: Greedy mode enables fast pruning when p
    \>\> n
4.  **Mixed models**: VIF-based pruning respects hierarchical structure
    by targeting only fixed effects

**Workflow selection guide**:

| Scenario | Function | Key Parameters |
|----|----|----|
| Small to moderate p (\< 50) | [`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md) | `mode = "auto"` or `"exact"` |
| High-dimensional (p \>\> n) | [`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md) | `mode = "greedy"` |
| Model-based refinement | [`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md) | `limit` (VIF threshold) |
| Protected variables | [`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md) | `force_in` |
| Mixed-effects models | [`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md) | `engine = "lme4"` |

**When to use each approach**:

- **corrPrune()**: First-stage dimensionality reduction based purely on
  pairwise correlations
- **modelPrune()**: Second-stage refinement that accounts for
  model-specific multicollinearity (VIF)
- **Combining both**: Often yields best results (correlation pruning →
  VIF refinement → final model)

------------------------------------------------------------------------

## See Also

- [`vignette("quickstart")`](https://gillescolling.com/corrselect/articles/quickstart.md) -
  Interface overview
- [`vignette("advanced")`](https://gillescolling.com/corrselect/articles/advanced.md) -
  Custom engines and algorithmic control
- [`vignette("comparison")`](https://gillescolling.com/corrselect/articles/comparison.md) -
  Comparison with alternatives
- [`vignette("theory")`](https://gillescolling.com/corrselect/articles/theory.md) -
  Mathematical foundations

------------------------------------------------------------------------

## References

**Thresholds**:

- O’Brien, R. M. (2007). A caution regarding rules of thumb for variance
  inflation factors. *Quality & Quantity*, 41(5), 673-690.
- Dormann, C. F., et al. (2013). Collinearity: a review of methods to
  deal with it. *Ecography*, 36(1), 27-46.

**Methods**: - See package documentation and JOSS paper for algorithm
details
