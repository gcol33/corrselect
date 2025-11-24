# Script to create example datasets for corrselect vignettes
# Run this script to regenerate the example data files

set.seed(42)  # For reproducibility

# ==============================================================================
# 1. Bioclimatic Data (Ecological Modeling)
# ==============================================================================

# Simulate the 19 WorldClim bioclimatic variables for 100 locations
# https://www.worldclim.org/data/bioclim.html
# Variables are correlated within groups (temperature, precipitation, seasonality)

n_locations <- 100
n_vars <- 19

# Create correlation structure: variables in blocks are correlated
# Temperature variables (BIO1-BIO11): highly correlated
# Precipitation variables (BIO12-BIO19): moderately correlated

# Generate correlated normal data
library(MASS)

# Temperature block (BIO1-BIO11): r = 0.80
temp_vars <- mvrnorm(n = n_locations,
                     mu = c(15, 10, 8, 200, 30, 5, 25, 20, 18, 22, 10),
                     Sigma = 0.80^abs(outer(1:11, 1:11, "-")) * 25)

# Precipitation block (BIO12-BIO19): r = 0.65
precip_vars <- mvrnorm(n = n_locations,
                       mu = c(1200, 100, 50, 200, 400, 300, 250, 350),
                       Sigma = 0.65^abs(outer(1:8, 1:8, "-")) * 10000)

# Combine all bioclim variables
bioclim_matrix <- cbind(temp_vars, precip_vars)

# Create WorldClim variable names (BIO1-BIO19)
var_names <- paste0("BIO", 1:19)
colnames(bioclim_matrix) <- var_names

# Create response variable: species richness
# Depends on subset of predictors + noise
species_richness <-
  0.5 * bioclim_matrix[, "BIO1"] +    # Annual Mean Temperature
  0.3 * bioclim_matrix[, "BIO12"] +   # Annual Precipitation
  0.2 * bioclim_matrix[, "BIO15"] +   # Precipitation Seasonality
  rnorm(n_locations, sd = 5)

# Scale to positive integer counts
species_richness <- round(pmax(0, species_richness - min(species_richness) + 5))

# Create final data frame
bioclim_example <- as.data.frame(cbind(species_richness, bioclim_matrix))

# ==============================================================================
# 2. Survey Data (Social Science)
# ==============================================================================

# Simulate questionnaire with 30 Likert-scale items + demographics
# Items measure 3 latent constructs (satisfaction, engagement, loyalty)
# Each construct has 10 items (with redundancy)

n_respondents <- 200

# Demographics
age <- round(rnorm(n_respondents, mean = 35, sd = 12))
age <- pmax(18, pmin(75, age))  # Bound between 18-75

gender <- factor(sample(c("Male", "Female", "Other"),
                       n_respondents,
                       replace = TRUE,
                       prob = c(0.48, 0.48, 0.04)))

education <- ordered(sample(c("High School", "Bachelor", "Master", "PhD"),
                           n_respondents,
                           replace = TRUE,
                           prob = c(0.3, 0.4, 0.2, 0.1)),
                    levels = c("High School", "Bachelor", "Master", "PhD"))

# Latent constructs (standardized)
satisfaction_latent <- rnorm(n_respondents)
engagement_latent <- 0.6 * satisfaction_latent + rnorm(n_respondents, sd = 0.8)
loyalty_latent <- 0.5 * satisfaction_latent + 0.4 * engagement_latent + rnorm(n_respondents, sd = 0.7)

# Function to convert continuous to Likert scale (1-7)
to_likert <- function(x, noise_sd = 0.3) {
  # Add noise and discretize
  y <- x + rnorm(length(x), sd = noise_sd)
  cut(y, breaks = c(-Inf, -1.5, -1, -0.5, 0, 0.5, 1.5, Inf),
      labels = 1:7, ordered_result = TRUE)
}

# Generate Likert items (10 per construct, correlated within construct)
satisfaction_items <- data.frame(
  lapply(1:10, function(i) {
    to_likert(satisfaction_latent + rnorm(n_respondents, sd = 0.2))
  })
)
names(satisfaction_items) <- paste0("satisfaction_", 1:10)

engagement_items <- data.frame(
  lapply(1:10, function(i) {
    to_likert(engagement_latent + rnorm(n_respondents, sd = 0.2))
  })
)
names(engagement_items) <- paste0("engagement_", 1:10)

loyalty_items <- data.frame(
  lapply(1:10, function(i) {
    to_likert(loyalty_latent + rnorm(n_respondents, sd = 0.2))
  })
)
names(loyalty_items) <- paste0("loyalty_", 1:10)

# Overall satisfaction score (response variable)
overall_satisfaction <- round(
  as.numeric(satisfaction_latent) * 10 + 50 + rnorm(n_respondents, sd = 5)
)
overall_satisfaction <- pmax(0, pmin(100, overall_satisfaction))

# Combine into survey data
survey_example <- data.frame(
  respondent_id = 1:n_respondents,
  age = age,
  gender = gender,
  education = education,
  overall_satisfaction = overall_satisfaction,
  satisfaction_items,
  engagement_items,
  loyalty_items
)

# ==============================================================================
# 3. Gene Expression Data (Bioinformatics)
# ==============================================================================

# Simulate gene expression data: 200 genes × 100 samples
# Genes are organized in co-expression modules
# Disease status depends on subset of genes

n_samples <- 100
n_genes <- 200

# Create 4 gene modules (50 genes each)
# Module 1: Highly correlated (r = 0.80)
# Module 2: Moderately correlated (r = 0.60)
# Module 3: Weakly correlated (r = 0.40)
# Module 4: Independent (r = 0)

module1 <- mvrnorm(n = n_samples,
                   mu = rep(8, 50),
                   Sigma = 0.80^abs(outer(1:50, 1:50, "-")) * 2)

module2 <- mvrnorm(n = n_samples,
                   mu = rep(7, 50),
                   Sigma = 0.60^abs(outer(1:50, 1:50, "-")) * 2)

module3 <- mvrnorm(n = n_samples,
                   mu = rep(6, 50),
                   Sigma = 0.40^abs(outer(1:50, 1:50, "-")) * 2)

module4 <- matrix(rnorm(n_samples * 50, mean = 5, sd = 1.5), ncol = 50)

# Combine modules
gene_matrix <- cbind(module1, module2, module3, module4)

# Gene names
gene_names <- paste0("GENE", sprintf("%03d", 1:n_genes))
colnames(gene_matrix) <- gene_names

# Disease outcome (binary)
# Depends on module 1 genes (disease-associated)
logit_p <- -2 +
  0.3 * gene_matrix[, "GENE001"] +
  0.2 * gene_matrix[, "GENE010"] +
  0.15 * gene_matrix[, "GENE020"]

disease_prob <- 1 / (1 + exp(-logit_p))
disease_status <- factor(rbinom(n_samples, 1, disease_prob),
                        levels = c(0, 1),
                        labels = c("Healthy", "Disease"))

# Create final data frame
genes_example <- data.frame(
  sample_id = paste0("S", sprintf("%03d", 1:n_samples)),
  disease_status = disease_status,
  gene_matrix
)

# ==============================================================================
# 4. Longitudinal Data (Clinical Research)
# ==============================================================================

# Simulate longitudinal study: 50 subjects × 10 timepoints each
# 20 predictors (correlated within subjects)
# Random effects: subject and site

n_subjects <- 50
n_timepoints <- 10
n_sites <- 5
n_predictors <- 20

# Assign subjects to sites
site <- factor(rep(1:n_sites, length.out = n_subjects))

# Subject random effects
subject_re <- rnorm(n_subjects, sd = 2)
site_re <- rnorm(n_sites, sd = 1)

# Create longitudinal structure
subject_id <- rep(1:n_subjects, each = n_timepoints)
time <- rep(1:n_timepoints, times = n_subjects)
site_id <- rep(site, each = n_timepoints)

# Generate correlated predictors (vary within and between subjects)
# Predictors 1-10: highly correlated (r = 0.75)
# Predictors 11-20: moderately correlated (r = 0.50)

# Between-subject component (constant within subject)
between_pred1_10 <- mvrnorm(n = n_subjects,
                            mu = rep(0, 10),
                            Sigma = 0.75^abs(outer(1:10, 1:10, "-")))

between_pred11_20 <- mvrnorm(n = n_subjects,
                             mu = rep(0, 10),
                             Sigma = 0.50^abs(outer(1:10, 1:10, "-")))

# Within-subject component (varies over time)
within_pred1_10 <- mvrnorm(n = n_subjects * n_timepoints,
                           mu = rep(0, 10),
                           Sigma = diag(10) * 0.5)

within_pred11_20 <- mvrnorm(n = n_subjects * n_timepoints,
                            mu = rep(0, 10),
                            Sigma = diag(10) * 0.5)

# Combine between + within
pred1_10 <- between_pred1_10[subject_id, ] + within_pred1_10
pred11_20 <- between_pred11_20[subject_id, ] + within_pred11_20

predictors <- cbind(pred1_10, pred11_20)
colnames(predictors) <- paste0("x", 1:n_predictors)

# Outcome variable (continuous)
# Depends on time, random effects, and subset of predictors
outcome <-
  10 +                                          # Intercept
  0.5 * time +                                  # Time trend
  subject_re[subject_id] +                      # Subject RE
  site_re[as.numeric(site_id)] +                # Site RE
  0.8 * predictors[, "x1"] +                    # Fixed effects
  0.5 * predictors[, "x5"] +
  0.3 * predictors[, "x15"] +
  rnorm(n_subjects * n_timepoints, sd = 1.5)    # Residual error

# Create final data frame
longitudinal_example <- data.frame(
  obs_id = 1:(n_subjects * n_timepoints),
  subject = factor(subject_id),
  site = site_id,
  time = time,
  outcome = outcome,
  predictors
)

# ==============================================================================
# Save datasets
# ==============================================================================

usethis::use_data(bioclim_example, overwrite = TRUE)
usethis::use_data(survey_example, overwrite = TRUE)
usethis::use_data(genes_example, overwrite = TRUE)
usethis::use_data(longitudinal_example, overwrite = TRUE)

# Print summaries
cat("\n=== Bioclim Example ===\n")
cat("Dimensions:", dim(bioclim_example), "\n")
cat("Correlation range:", range(cor(bioclim_example[, -1])), "\n")

cat("\n=== Survey Example ===\n")
cat("Dimensions:", dim(survey_example), "\n")

cat("\n=== Genes Example ===\n")
cat("Dimensions:", dim(genes_example), "\n")
cat("Disease prevalence:", table(genes_example$disease_status), "\n")

cat("\n=== Longitudinal Example ===\n")
cat("Dimensions:", dim(longitudinal_example), "\n")
cat("Subjects:", length(unique(longitudinal_example$subject)), "\n")
cat("Observations per subject:", n_timepoints, "\n")
