pkgname <- "corrselect"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "corrselect-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('corrselect')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("CorrCombo")
### * CorrCombo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CorrCombo
### Title: CorrCombo S4 class
### Aliases: CorrCombo CorrCombo-class show,CorrCombo-method

### ** Examples

show(new("CorrCombo",
  subset_list = list(c("A", "B"), c("A", "C")),
  avg_corr = c(0.2, 0.3),
  min_corr = c(0.1, 0.2),
  max_corr = c(0.3, 0.4),
  names = c("A", "B", "C"),
  threshold = 0.5,
  forced_in = character(),
  search_type = "els",
  cor_method = "mixed",
  n_rows_used = as.integer(5)
))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CorrCombo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("MatSelect")
### * MatSelect

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MatSelect
### Title: Select Variable Subsets with Low Correlation or Association
###   (Matrix Interface)
### Aliases: MatSelect

### ** Examples

set.seed(42)
mat <- matrix(rnorm(100), ncol = 10)
colnames(mat) <- paste0("V", 1:10)
cmat <- cor(mat)

# Default method (Bron-Kerbosch)
res1 <- MatSelect(cmat, threshold = 0.5)

# Bron–Kerbosch without pivot
res2 <- MatSelect(cmat, threshold = 0.5, method = "bron-kerbosch", use_pivot = FALSE)

# Bron–Kerbosch with pivoting
res3 <- MatSelect(cmat, threshold = 0.5, method = "bron-kerbosch", use_pivot = TRUE)

# Force variable 1 into every subset (with warning if too correlated)
res4 <- MatSelect(cmat, threshold = 0.5, force_in = 1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MatSelect", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("as.data.frame.CorrCombo")
### * as.data.frame.CorrCombo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.data.frame.CorrCombo
### Title: Coerce CorrCombo to a Data Frame
### Aliases: as.data.frame.CorrCombo

### ** Examples

set.seed(1)
mat <- matrix(rnorm(100), ncol = 10)
colnames(mat) <- paste0("V", 1:10)
res <- corrSelect(cor(mat), threshold = 0.5)
as.data.frame(res)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.data.frame.CorrCombo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("assocSelect")
### * assocSelect

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: assocSelect
### Title: Select Variable Subsets with Low Association (Mixed-Type Data
###   Frame Interface)
### Aliases: assocSelect

### ** Examples

set.seed(42)
df <- data.frame(
  height = rnorm(15, 170, 10),
  weight = rnorm(15, 70, 12),
  group  = factor(rep(LETTERS[1:3], each = 5)),
  score  = ordered(sample(c("low","med","high"), 15, TRUE))
)

## keep every subset whose internal associations <= 0.6
assocSelect(df, threshold = 0.6)

## use Kendall for all rank-based comparisons and force 'height' to appear
assocSelect(df,
            threshold       = 0.5,
            method_num_num  = "kendall",
            method_num_ord  = "kendall",
            method_ord_ord  = "kendall",
            force_in        = "height")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("assocSelect", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bioclim_example")
### * bioclim_example

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bioclim_example
### Title: Example Bioclimatic Data for Ecological Modeling
### Aliases: bioclim_example
### Keywords: datasets

### ** Examples

data(bioclim_example)

# The 19 WorldClim bioclimatic variables (https://www.worldclim.org/data/bioclim.html)
# Many are highly correlated, making them ideal for pruning

# Remove highly correlated variables
pruned <- corrPrune(bioclim_example[, -1], threshold = 0.7)
ncol(pruned)  # Reduced from 19 to ~8 variables

# Model-based pruning with VIF
model_data <- modelPrune(species_richness ~ .,
                         data = bioclim_example,
                         limit = 5)
attr(model_data, "selected_vars")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bioclim_example", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cor_example")
### * cor_example

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cor_example
### Title: Example Correlation Matrix with Block Structure
### Aliases: cor_example
### Keywords: datasets

### ** Examples

data(cor_example)

# Matrix dimensions
dim(cor_example)

# Visualize structure
if (requireNamespace("corrplot", quietly = TRUE)) {
  corrplot::corrplot(cor_example, method = "color", type = "upper",
                     tl.col = "black", tl.cex = 0.7)
}

# Distribution of correlations
hist(cor_example[upper.tri(cor_example)],
     breaks = 30,
     main = "Distribution of Correlations in cor_example",
     xlab = "Correlation",
     col = "steelblue")

# Use with MatSelect
library(corrselect)
results <- MatSelect(cor_example, threshold = 0.7, method = "els")
show(results)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cor_example", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("corrPrune")
### * corrPrune

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: corrPrune
### Title: Association-Based Predictor Pruning
### Aliases: corrPrune

### ** Examples

# Basic numeric data pruning
data(mtcars)
pruned <- corrPrune(mtcars, threshold = 0.7)
names(pruned)

# Force certain variables to be included
pruned <- corrPrune(mtcars, threshold = 0.7, force_in = "mpg")

# Use greedy mode for faster computation
pruned <- corrPrune(mtcars, threshold = 0.7, mode = "greedy")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("corrPrune", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("corrSelect")
### * corrSelect

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: corrSelect
### Title: Select Variable Subsets with Low Correlation (Data Frame
###   Interface)
### Aliases: corrSelect

### ** Examples

set.seed(42)
n <- 100

# Create 20 variables: 5 blocks of correlated variables + some noise
block1 <- matrix(rnorm(n * 4), ncol = 4)
block2 <- matrix(rnorm(n), ncol = 1)
block2 <- matrix(rep(block2, 4), ncol = 4) + matrix(rnorm(n * 4, sd = 0.1), ncol = 4)
block3 <- matrix(rnorm(n * 4), ncol = 4)
block4 <- matrix(rnorm(n * 4), ncol = 4)
block5 <- matrix(rnorm(n * 4), ncol = 4)

df <- as.data.frame(cbind(block1, block2, block3, block4, block5))
colnames(df) <- paste0("V", 1:20)

# Add a non-numeric column to be ignored
df$label <- factor(sample(c("A", "B"), n, replace = TRUE))

# Basic usage
corrSelect(df, threshold = 0.8)

# Try Bron–Kerbosch with pivoting
corrSelect(df, threshold = 0.6, method = "bron-kerbosch", use_pivot = TRUE)

# Force in a specific variable and use Spearman correlation
corrSelect(df, threshold = 0.6, force_in = "V10", cor_method = "spearman")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("corrSelect", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("corrSubset")
### * corrSubset

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: corrSubset
### Title: Extract Variable Subsets from a CorrCombo Object
### Aliases: corrSubset

### ** Examples

# Simulate input data
set.seed(123)
df <- as.data.frame(matrix(rnorm(100), nrow = 10))
colnames(df) <- paste0("V", 1:10)

# Compute correlation matrix
cmat <- cor(df)

# Select subsets using corrSelect
res <- corrSelect(cmat, threshold = 0.5)

# Extract the best subset (default)
corrSubset(res, df)

# Extract the second-best subset
corrSubset(res, df, which = 2)

# Extract the first three subsets
corrSubset(res, df, which = 1:3)

# Extract all subsets
corrSubset(res, df, which = "all")

# Extract best subset and retain additional numeric column
df$CopyV1 <- df$V1
corrSubset(res, df, which = 1, keepExtra = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("corrSubset", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("genes_example")
### * genes_example

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: genes_example
### Title: Example Gene Expression Data for Bioinformatics
### Aliases: genes_example
### Keywords: datasets

### ** Examples

data(genes_example)

# Greedy pruning for high-dimensional data
gene_data <- genes_example[, -(1:2)]  # Exclude ID and outcome
pruned <- corrPrune(gene_data, threshold = 0.8, mode = "greedy")
ncol(pruned)  # Reduced from 200 to ~50 genes

# Use pruned genes for classification
pruned_with_outcome <- data.frame(
  disease_status = genes_example$disease_status,
  pruned
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("genes_example", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("longitudinal_example")
### * longitudinal_example

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: longitudinal_example
### Title: Example Longitudinal Data for Clinical Research
### Aliases: longitudinal_example
### Keywords: datasets

### ** Examples

data(longitudinal_example)

## Not run: 
##D # Prune fixed effects in mixed model (requires lme4)
##D if (requireNamespace("lme4", quietly = TRUE)) {
##D   pruned <- modelPrune(
##D     outcome ~ x1 + x2 + x3 + x4 + x5 + (1|subject) + (1|site),
##D     data = longitudinal_example,
##D     engine = "lme4",
##D     limit = 5
##D   )
##D 
##D   # Random effects preserved, only fixed effects pruned
##D   attr(pruned, "selected_vars")
##D }
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("longitudinal_example", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("modelPrune")
### * modelPrune

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: modelPrune
### Title: Model-Based Predictor Pruning
### Aliases: modelPrune

### ** Examples

# Linear model with VIF-based pruning
data(mtcars)
pruned <- modelPrune(mpg ~ ., data = mtcars, engine = "lm", limit = 5)
names(pruned)

# Force certain predictors to remain
pruned <- modelPrune(mpg ~ ., data = mtcars, force_in = "drat", limit = 20)

# GLM example (requires family argument)
pruned <- modelPrune(am ~ ., data = mtcars, engine = "glm",
                     family = binomial(), limit = 5)

## Not run: 
##D # Custom engine example (INLA)
##D inla_engine <- list(
##D   name = "inla",
##D   fit = function(formula, data, ...) {
##D     inla::inla(formula = formula, data = data,
##D                family = list(...)$family %||% "gaussian",
##D                control.compute = list(config = TRUE))
##D   },
##D   diagnostics = function(model, fixed_effects) {
##D     scores <- model$summary.fixed[, "sd"]
##D     names(scores) <- rownames(model$summary.fixed)
##D     scores[fixed_effects]
##D   }
##D )
##D 
##D pruned <- modelPrune(y ~ x1 + x2 + x3, data = df,
##D                      engine = inla_engine, limit = 0.5)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("modelPrune", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("survey_example")
### * survey_example

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: survey_example
### Title: Example Survey Data for Social Science Research
### Aliases: survey_example
### Keywords: datasets

### ** Examples

data(survey_example)

# This dataset has mixed types: numeric (age, overall_satisfaction),
# factors (gender, education), and ordered factors (Likert items)
str(survey_example[, 1:10])

## No test: 
# Use assocSelect() for mixed-type data pruning
# This may take a few seconds with 34 variables
pruned <- assocSelect(survey_example[, -1],  # Exclude respondent_id
                      threshold = 0.8,
                      method_ord_ord = "spearman")
length(attr(pruned, "selected_vars"))
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("survey_example", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
