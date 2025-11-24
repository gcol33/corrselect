#' Example Bioclimatic Data for Ecological Modeling
#'
#' A simulated dataset representing 50 WorldClim-like bioclimatic variables
#' measured at 100 geographic locations, with species richness as the response variable.
#' Variables are organized into correlated blocks representing temperature,
#' precipitation, seasonality, and elevation.
#'
#' @format A data frame with 100 rows and 51 variables:
#' \describe{
#'   \item{species_richness}{Integer. Number of species observed (response variable)}
#'   \item{bio1_temp through bio15_temp}{Numeric. Temperature-related variables (highly correlated, r ≈ 0.85)}
#'   \item{bio16_precip through bio30_precip}{Numeric. Precipitation-related variables (moderately correlated, r ≈ 0.70)}
#'   \item{bio31_season through bio45_season}{Numeric. Seasonality variables (weakly correlated, r ≈ 0.50)}
#'   \item{bio46_elev through bio50_elev}{Numeric. Elevation variables (independent, r ≈ 0)}
#' }
#'
#' @details
#' This dataset demonstrates a common problem in ecological modeling: many bioclimatic
#' predictors are highly correlated within groups (e.g., different temperature metrics),
#' leading to multicollinearity issues. The species richness response depends on a subset
#' of predictors from each group.
#'
#' **Use case**: Demonstrating `corrPrune()` for reducing correlated environmental
#' predictors before fitting species distribution models.
#'
#' @source Simulated data based on typical WorldClim variable structures
#'
#' @seealso [corrPrune()], [modelPrune()]
#'
#' @examples
#' data(bioclim_example)
#'
#' # Remove highly correlated bioclimatic variables
#' pruned <- corrPrune(bioclim_example[, -1], threshold = 0.7)
#' ncol(pruned)  # Reduced from 50 to ~15 variables
#'
#' # Model-based pruning with VIF
#' model_data <- modelPrune(species_richness ~ .,
#'                          data = bioclim_example,
#'                          limit = 5)
#' attr(model_data, "selected_vars")
"bioclim_example"

#' Example Survey Data for Social Science Research
#'
#' A simulated questionnaire dataset with 30 Likert-scale items measuring three
#' latent constructs (satisfaction, engagement, loyalty), plus demographic variables
#' and an overall satisfaction score.
#'
#' @format A data frame with 200 rows and 35 variables:
#' \describe{
#'   \item{respondent_id}{Integer. Unique respondent identifier}
#'   \item{age}{Integer. Respondent age (18-75 years)}
#'   \item{gender}{Factor. Gender (Male, Female, Other)}
#'   \item{education}{Ordered factor. Education level (High School, Bachelor, Master, PhD)}
#'   \item{overall_satisfaction}{Integer. Overall satisfaction score (0-100)}
#'   \item{satisfaction_1 through satisfaction_10}{Ordered factor. Satisfaction items (1-7 Likert scale)}
#'   \item{engagement_1 through engagement_10}{Ordered factor. Engagement items (1-7 Likert scale)}
#'   \item{loyalty_1 through loyalty_10}{Ordered factor. Loyalty items (1-7 Likert scale)}
#' }
#'
#' @details
#' This dataset represents a common scenario in survey research: multiple items
#' measuring similar constructs lead to redundancy and multicollinearity. Items
#' within each construct are correlated (satisfaction, engagement, loyalty), and
#' the constructs themselves are inter-correlated.
#'
#' **Use case**: Demonstrating `corrPrune()` for identifying redundant questionnaire
#' items while preserving key demographic variables with `force_in`.
#'
#' @source Simulated data based on typical customer satisfaction survey structures
#'
#' @seealso [corrPrune()], [assocSelect()]
#'
#' @examples
#' data(survey_example)
#'
#' # Remove redundant survey items (numeric columns only)
#' # Keep age and overall satisfaction, prune correlated Likert items
#' numeric_cols <- sapply(survey_example, is.numeric)
#' pruned <- corrPrune(survey_example[, numeric_cols & names(survey_example) != "respondent_id"],
#'                     threshold = 0.6,
#'                     force_in = c("age", "overall_satisfaction"))
#'
#' # Check which items were removed
#' attr(pruned, "removed_vars")
"survey_example"

#' Example Gene Expression Data for Bioinformatics
#'
#' A simulated gene expression dataset with 200 genes measured across 100 samples,
#' organized into co-expression modules with a binary disease outcome.
#'
#' @format A data frame with 100 rows and 202 variables:
#' \describe{
#'   \item{sample_id}{Character. Unique sample identifier}
#'   \item{disease_status}{Factor. Disease status (Healthy, Disease)}
#'   \item{GENE001 through GENE200}{Numeric. Gene expression values (log-transformed)}
#' }
#'
#' @details
#' This dataset simulates a high-dimensional, low-sample scenario common in genomics.
#' Genes are organized into four co-expression modules:
#' - Module 1 (GENE001-GENE050): Highly correlated (r ≈ 0.80), disease-associated
#' - Module 2 (GENE051-GENE100): Moderately correlated (r ≈ 0.60)
#' - Module 3 (GENE101-GENE150): Weakly correlated (r ≈ 0.40)
#' - Module 4 (GENE151-GENE200): Independent (r ≈ 0)
#'
#' Disease outcome depends on a subset of genes from Module 1.
#'
#' **Use case**: Demonstrating `corrPrune()` with `mode = "greedy"` for handling
#' high-dimensional data efficiently.
#'
#' @source Simulated data based on typical gene expression microarray structures
#'
#' @seealso [corrPrune()]
#'
#' @examples
#' data(genes_example)
#'
#' # Greedy pruning for high-dimensional data
#' gene_data <- genes_example[, -(1:2)]  # Exclude ID and outcome
#' pruned <- corrPrune(gene_data, threshold = 0.8, mode = "greedy")
#' ncol(pruned)  # Reduced from 200 to ~50 genes
#'
#' # Use pruned genes for classification
#' pruned_with_outcome <- data.frame(
#'   disease_status = genes_example$disease_status,
#'   pruned
#' )
"genes_example"

#' Example Longitudinal Data for Clinical Research
#'
#' A simulated longitudinal study dataset with 50 subjects measured at 10 timepoints
#' each, with 20 correlated predictors and nested random effects (subject and site).
#'
#' @format A data frame with 500 rows and 25 variables:
#' \describe{
#'   \item{obs_id}{Integer. Observation identifier (1-500)}
#'   \item{subject}{Factor. Subject identifier (1-50)}
#'   \item{site}{Factor. Study site identifier (1-5)}
#'   \item{time}{Integer. Measurement timepoint (1-10)}
#'   \item{outcome}{Numeric. Continuous outcome variable}
#'   \item{x1 through x20}{Numeric. Correlated predictor variables}
#' }
#'
#' @details
#' This dataset represents a typical longitudinal study with repeated measures.
#' Predictors are correlated both within and between subjects:
#' - Predictors x1-x10: Highly correlated (r ≈ 0.75)
#' - Predictors x11-x20: Moderately correlated (r ≈ 0.50)
#'
#' The outcome depends on time (linear trend), random effects (subject and site),
#' and a subset of fixed-effect predictors (x1, x5, x15).
#'
#' **Use case**: Demonstrating `modelPrune()` with mixed models (`lme4` engine)
#' to prune fixed effects while preserving random effects structure.
#'
#' @source Simulated data based on typical clinical trial designs
#'
#' @seealso [modelPrune()]
#'
#' @examples
#' data(longitudinal_example)
#'
#' \dontrun{
#' # Prune fixed effects in mixed model (requires lme4)
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   pruned <- modelPrune(
#'     outcome ~ x1 + x2 + x3 + x4 + x5 + (1|subject) + (1|site),
#'     data = longitudinal_example,
#'     engine = "lme4",
#'     limit = 5
#'   )
#'
#'   # Random effects preserved, only fixed effects pruned
#'   attr(pruned, "selected_vars")
#' }
#' }
"longitudinal_example"
