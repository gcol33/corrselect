#' Example Bioclimatic Data for Ecological Modeling
#'
#' A simulated dataset with the 19 WorldClim bioclimatic variables
#' (https://www.worldclim.org/data/bioclim.html) measured at 100 geographic locations,
#' with species richness as the response variable. Variables are organized into
#' correlated blocks representing temperature (BIO1-BIO11) and precipitation (BIO12-BIO19).
#'
#' @format A data frame with 100 rows and 20 variables:
#' \describe{
#'   \item{species_richness}{Integer. Number of species observed (response variable)}
#'   \item{BIO1}{Numeric. Annual Mean Temperature}
#'   \item{BIO2}{Numeric. Mean Diurnal Range}
#'   \item{BIO3}{Numeric. Isothermality}
#'   \item{BIO4}{Numeric. Temperature Seasonality}
#'   \item{BIO5}{Numeric. Max Temperature of Warmest Month}
#'   \item{BIO6}{Numeric. Min Temperature of Coldest Month}
#'   \item{BIO7}{Numeric. Temperature Annual Range}
#'   \item{BIO8}{Numeric. Mean Temperature of Wettest Quarter}
#'   \item{BIO9}{Numeric. Mean Temperature of Driest Quarter}
#'   \item{BIO10}{Numeric. Mean Temperature of Warmest Quarter}
#'   \item{BIO11}{Numeric. Mean Temperature of Coldest Quarter}
#'   \item{BIO12}{Numeric. Annual Precipitation}
#'   \item{BIO13}{Numeric. Precipitation of Wettest Month}
#'   \item{BIO14}{Numeric. Precipitation of Driest Month}
#'   \item{BIO15}{Numeric. Precipitation Seasonality}
#'   \item{BIO16}{Numeric. Precipitation of Wettest Quarter}
#'   \item{BIO17}{Numeric. Precipitation of Driest Quarter}
#'   \item{BIO18}{Numeric. Precipitation of Warmest Quarter}
#'   \item{BIO19}{Numeric. Precipitation of Coldest Quarter}
#' }
#'
#' @details
#' This dataset demonstrates a common problem in ecological modeling: bioclimatic
#' predictors are highly correlated within groups (temperature variables BIO1-BIO11 are
#' highly correlated; precipitation variables BIO12-BIO19 are moderately correlated),
#' leading to multicollinearity issues. The species richness response depends on a subset
#' of predictors.
#'
#' **Use case**: Demonstrating `corrPrune()` and `modelPrune()` for reducing correlated
#' environmental predictors before fitting species distribution models.
#'
#' @source Simulated data based on the 19 WorldClim bioclimatic variables
#'
#' @seealso [corrPrune()], [modelPrune()]
#'
#' @examples
#' data(bioclim_example)
#'
#' # The 19 WorldClim bioclimatic variables (https://www.worldclim.org/data/bioclim.html)
#' # Many are highly correlated, making them ideal for pruning
#'
#' # Remove highly correlated variables
#' pruned <- corrPrune(bioclim_example[, -1], threshold = 0.7)
#' ncol(pruned)  # Reduced from 19 to ~8 variables
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
#'   \item{satisfaction_1, satisfaction_2, satisfaction_3, satisfaction_4, satisfaction_5, satisfaction_6, satisfaction_7, satisfaction_8, satisfaction_9, satisfaction_10}{Ordered factor. Satisfaction items (1-7 Likert scale)}
#'   \item{engagement_1, engagement_2, engagement_3, engagement_4, engagement_5, engagement_6, engagement_7, engagement_8, engagement_9, engagement_10}{Ordered factor. Engagement items (1-7 Likert scale)}
#'   \item{loyalty_1, loyalty_2, loyalty_3, loyalty_4, loyalty_5, loyalty_6, loyalty_7, loyalty_8, loyalty_9, loyalty_10}{Ordered factor. Loyalty items (1-7 Likert scale)}
#' }
#'
#' @details
#' This dataset represents a common scenario in survey research: multiple items
#' measuring similar constructs lead to redundancy and multicollinearity. Items
#' within each construct are correlated (satisfaction, engagement, loyalty), and
#' the constructs themselves are inter-correlated.
#'
#' **Use case**: Demonstrating `assocSelect()` for identifying redundant questionnaire
#' items in mixed-type data (ordered factors + numeric variables).
#'
#' @source Simulated data based on typical customer satisfaction survey structures
#'
#' @seealso [assocSelect()], [corrPrune()]
#'
#' @examples
#' data(survey_example)
#'
#' # This dataset has mixed types: numeric (age, overall_satisfaction),
#' # factors (gender, education), and ordered factors (Likert items)
#' str(survey_example[, 1:10])
#'
#' \donttest{
#' # Use assocSelect() for mixed-type data pruning
#' # This may take a few seconds with 34 variables
#' pruned <- assocSelect(survey_example[, -1],  # Exclude respondent_id
#'                       threshold = 0.8,
#'                       method_ord_ord = "spearman")
#' length(attr(pruned, "selected_vars"))
#' }
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
#'   \item{GENE001, GENE002, GENE003, GENE004, GENE005, GENE006, GENE007, GENE008, GENE009, GENE010, GENE011, GENE012, GENE013, GENE014, GENE015, GENE016, GENE017, GENE018, GENE019, GENE020, GENE021, GENE022, GENE023, GENE024, GENE025, GENE026, GENE027, GENE028, GENE029, GENE030, GENE031, GENE032, GENE033, GENE034, GENE035, GENE036, GENE037, GENE038, GENE039, GENE040, GENE041, GENE042, GENE043, GENE044, GENE045, GENE046, GENE047, GENE048, GENE049, GENE050, GENE051, GENE052, GENE053, GENE054, GENE055, GENE056, GENE057, GENE058, GENE059, GENE060, GENE061, GENE062, GENE063, GENE064, GENE065, GENE066, GENE067, GENE068, GENE069, GENE070, GENE071, GENE072, GENE073, GENE074, GENE075, GENE076, GENE077, GENE078, GENE079, GENE080, GENE081, GENE082, GENE083, GENE084, GENE085, GENE086, GENE087, GENE088, GENE089, GENE090, GENE091, GENE092, GENE093, GENE094, GENE095, GENE096, GENE097, GENE098, GENE099, GENE100, GENE101, GENE102, GENE103, GENE104, GENE105, GENE106, GENE107, GENE108, GENE109, GENE110, GENE111, GENE112, GENE113, GENE114, GENE115, GENE116, GENE117, GENE118, GENE119, GENE120, GENE121, GENE122, GENE123, GENE124, GENE125, GENE126, GENE127, GENE128, GENE129, GENE130, GENE131, GENE132, GENE133, GENE134, GENE135, GENE136, GENE137, GENE138, GENE139, GENE140, GENE141, GENE142, GENE143, GENE144, GENE145, GENE146, GENE147, GENE148, GENE149, GENE150, GENE151, GENE152, GENE153, GENE154, GENE155, GENE156, GENE157, GENE158, GENE159, GENE160, GENE161, GENE162, GENE163, GENE164, GENE165, GENE166, GENE167, GENE168, GENE169, GENE170, GENE171, GENE172, GENE173, GENE174, GENE175, GENE176, GENE177, GENE178, GENE179, GENE180, GENE181, GENE182, GENE183, GENE184, GENE185, GENE186, GENE187, GENE188, GENE189, GENE190, GENE191, GENE192, GENE193, GENE194, GENE195, GENE196, GENE197, GENE198, GENE199, GENE200}{Numeric. Gene expression values (log-transformed)}
#' }
#'
#' @details
#' This dataset simulates a high-dimensional, low-sample scenario common in genomics.
#' Genes are organized into four co-expression modules:
#' - Module 1 (GENE001-GENE050): Highly correlated (r ~= 0.80), disease-associated
#' - Module 2 (GENE051-GENE100): Moderately correlated (r ~= 0.60)
#' - Module 3 (GENE101-GENE150): Weakly correlated (r ~= 0.40)
#' - Module 4 (GENE151-GENE200): Independent (r ~= 0)
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
#'   \item{x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20}{Numeric. Correlated predictor variables}
#' }
#'
#' @details
#' This dataset represents a typical longitudinal study with repeated measures.
#' Predictors are correlated both within and between subjects:
#' - Predictors x1-x10: Highly correlated (r ~= 0.75)
#' - Predictors x11-x20: Moderately correlated (r ~= 0.50)
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
