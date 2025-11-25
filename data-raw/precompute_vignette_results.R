# Script to precompute computationally intensive vignette results
# Run this script when updating vignettes that have expensive computations

# Load the package in development mode
devtools::load_all(".")

# Create output directory
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# Workflow 1: Ecology (bioclim_example)
# ============================================================================

data(bioclim_example)

# Step 1: corrPrune
bio_clean <- corrPrune(bioclim_example[, -1], threshold = 0.7)

# Step 2: modelPrune with VIF
bio_model_data <- modelPrune(
  formula = species_richness ~ .,
  data = bioclim_example,
  limit = 5
)

# Save results
saveRDS(list(
  bio_clean = bio_clean,
  bio_model_data = bio_model_data,
  original_dim = dim(bioclim_example)
), file = "inst/extdata/workflow_ecology.rds")

cat("✓ Saved workflow_ecology.rds\n")

# ============================================================================
# Workflow 2: Survey Data (survey_example)
# ============================================================================

data(survey_example)

# Exclude ID and response
survey_predictors <- survey_example[, !(names(survey_example) %in%
                                         c("respondent_id", "overall_satisfaction"))]

# Prune with force_in (only use numeric columns)
numeric_cols <- sapply(survey_predictors, is.numeric)
survey_numeric <- survey_predictors[, numeric_cols, drop = FALSE]
survey_clean <- corrPrune(
  survey_numeric,
  threshold = 0.6,
  force_in = if("age" %in% names(survey_numeric)) "age" else NULL
)

# Save results
saveRDS(list(
  survey_clean = survey_clean,
  original_dim = dim(survey_predictors)
), file = "inst/extdata/workflow_survey.rds")

cat("✓ Saved workflow_survey.rds\n")

# ============================================================================
# Workflow 3: High-Dimensional Data (genes_example)
# ============================================================================

data(genes_example)

# Separate response
genes_predictors <- genes_example[, !names(genes_example) %in% "disease_status"]

# Prune
genes_clean <- corrPrune(genes_predictors, threshold = 0.8)

# Save results
saveRDS(list(
  genes_clean = genes_clean,
  original_dim = dim(genes_predictors),
  response = genes_example$disease_status
), file = "inst/extdata/workflow_genes.rds")

cat("✓ Saved workflow_genes.rds\n")

# ============================================================================
# Workflow 4: Mixed Models (longitudinal_example)
# ============================================================================

data(longitudinal_example)

# Prepare data
longitudinal_predictors <- longitudinal_example[, !names(longitudinal_example) %in%
                                                  c("subject_id", "timepoint", "outcome")]

# Prune
longitudinal_clean <- corrPrune(longitudinal_predictors, threshold = 0.7)

# Model-based pruning
longitudinal_model <- modelPrune(
  formula = outcome ~ .,
  data = cbind(
    outcome = longitudinal_example$outcome,
    longitudinal_clean
  ),
  limit = 5
)

# Save results
saveRDS(list(
  longitudinal_clean = longitudinal_clean,
  longitudinal_model = longitudinal_model,
  original_dim = dim(longitudinal_predictors)
), file = "inst/extdata/workflow_longitudinal.rds")

cat("✓ Saved workflow_longitudinal.rds\n")

# ============================================================================
# Advanced Vignette: Benchmarking Results
# ============================================================================

# Benchmark corrPrune modes (this is the slow part!)
benchmark_corrPrune <- function(p_values) {
  results <- data.frame(
    p = integer(),
    exact_time = numeric(),
    greedy_time = numeric()
  )

  for (p in p_values) {
    set.seed(123)
    data <- as.data.frame(matrix(rnorm(100 * p), ncol = p))

    # Exact mode
    exact_time <- tryCatch({
      system.time(corrPrune(data, threshold = 0.7, mode = "exact"))[3]
    }, error = function(e) NA)

    # Greedy mode
    greedy_time <- system.time(corrPrune(data, threshold = 0.7, mode = "greedy"))[3]

    results <- rbind(results, data.frame(
      p = p,
      exact_time = exact_time,
      greedy_time = greedy_time
    ))

    cat(sprintf("  Benchmarked p=%d: exact=%.3fs, greedy=%.3fs\n",
                p, exact_time, greedy_time))
  }

  results
}

p_values <- c(10, 15, 20, 25, 50, 100)
benchmark <- benchmark_corrPrune(p_values)

saveRDS(benchmark, file = "inst/extdata/benchmark_results.rds")

cat("✓ Saved benchmark_results.rds\n")

# ============================================================================
# Summary
# ============================================================================

cat("\n✅ All precomputed results saved to inst/extdata/\n")
cat("\nFiles created:\n")
cat("  - workflow_ecology.rds\n")
cat("  - workflow_survey.rds\n")
cat("  - workflow_genes.rds\n")
cat("  - workflow_longitudinal.rds\n")
cat("  - benchmark_results.rds\n")
cat("\nTotal size:\n")
files <- list.files("inst/extdata", pattern = "\\.rds$", full.names = TRUE)
total_size <- sum(file.info(files)$size) / 1024
cat(sprintf("  %.1f KB\n", total_size))
