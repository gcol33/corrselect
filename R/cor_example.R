#' Example Correlation Matrix with Block Structure
#'
#' A 20x20 correlation matrix with known block structure designed for
#' demonstrating threshold selection, algorithm comparison, and visualization
#' examples in vignettes.
#'
#' @format A 20x20 numeric correlation matrix with row and column names
#'   V1-V20. The matrix has four distinct correlation blocks (realized
#'   pairwise ranges, after the positive-definiteness correction described
#'   in \code{data-raw/create_cor_example.R} -- these can differ slightly
#'   from the \code{runif()} bounds used to generate each block):
#'   \describe{
#'     \item{Block 1 (V1-V5)}{High correlation: mean = 0.81,
#'       range = (0.74, 0.88)}
#'     \item{Block 2 (V6-V10)}{Moderate correlation: mean = 0.57,
#'       range = (0.50, 0.63)}
#'     \item{Block 3 (V11-V15)}{Low correlation: mean = 0.28,
#'       range = (0.20, 0.36)}
#'     \item{Block 4 (V16-V20)}{Minimal correlation: mean = 0.06,
#'       range = (0.01, 0.12)}
#'   }
#'   Between-block correlations are low (range = (0.0, 0.3)). The matrix
#'   is guaranteed to be positive definite.
#'
#' @details
#' This dataset provides a controlled correlation structure useful for:
#' \itemize{
#'   \item Threshold sensitivity analysis (comparing results at
#'     tau = 0.5, 0.7, 0.9)
#'   \item Algorithm comparison (exact vs greedy modes)
#'   \item Visualization examples (heatmaps, correlation distributions)
#'   \item Reproducible benchmarks across vignettes
#' }
#'
#' Expected behavior with different thresholds, verified via
#' \code{MatSelect(cor_example, threshold = tau)} on the full matrix:
#' \itemize{
#'   \item tau = 0.5: Blocks 1 and 2 both require pruning (largest subset
#'     keeps 12 of 20 variables)
#'   \item tau = 0.7: Block 1 requires pruning; Block 2 is retained intact
#'     (largest subset keeps 16 of 20 variables)
#'   \item tau = 0.9: No block requires pruning (largest subset keeps all
#'     20 variables)
#' }
#'
#' @source Generated with \code{data-raw/create_cor_example.R} using
#'   seed 20250125.
#'
#' @examples
#' data(cor_example)
#'
#' # Matrix dimensions
#' dim(cor_example)
#'
#' # Visualize structure
#' if (requireNamespace("corrplot", quietly = TRUE)) {
#'   corrplot::corrplot(cor_example, method = "color", type = "upper",
#'                      tl.col = "black", tl.cex = 0.7)
#' }
#'
#' # Distribution of correlations
#' hist(cor_example[upper.tri(cor_example)],
#'      breaks = 30,
#'      main = "Distribution of Correlations in cor_example",
#'      xlab = "Correlation",
#'      col = "steelblue")
#'
#' # Use with MatSelect
#' library(corrselect)
#' results <- MatSelect(cor_example, threshold = 0.7, method = "els")
#' show(results)
#'
#' @keywords datasets
"cor_example"
