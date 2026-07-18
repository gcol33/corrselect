#' corrselect: Correlation-Based and Model-Based Predictor Pruning
#'
#' Provides tools for reducing multicollinearity in predictor sets through
#' association-based and model-based approaches. The package offers both fast
#' greedy algorithms for quick pruning and exact graph-theoretic algorithms
#' for exhaustive subset enumeration.
#'
#' @section Association-Based Pruning:
#' These functions identify variable subsets where all pairwise correlations
#' or associations remain below a user-defined threshold:
#' \describe{
#'   \item{\code{\link{corrPrune}}}{Association-based pruning for numeric or mixed-type data, using exact search by default and falling back to a fast greedy algorithm for large predictor sets}
#'   \item{\code{\link{corrSelect}}}{Exhaustive enumeration for numeric data frames}
#'   \item{\code{\link{assocSelect}}}{Exhaustive enumeration for mixed-type data (numeric, factor, ordered)}
#'   \item{\code{\link{MatSelect}}}{Direct interface using a pre-computed correlation matrix}
#' }
#'
#' @section Model-Based Pruning:
#' These functions use variance inflation factors (VIF) to iteratively remove
#' collinear predictors from regression models:
#' \describe{
#'   \item{\code{\link{modelPrune}}}{VIF-based pruning for lm, glm, lme4, and glmmTMB models}
#' }
#'
#' @section Algorithms:
#' The exact enumeration functions (\code{corrSelect}, \code{assocSelect},
#' \code{MatSelect}) use two graph-theoretic algorithms:
#' \describe{
#'   \item{Eppstein-Loffler-Strash (ELS)}{Recommended when using \code{force_in} constraints}
#'   \item{Bron-Kerbosch}{Default algorithm, with optional pivoting for performance}
#' }
#'
#' @section Helpers:
#' \describe{
#'   \item{\code{\link{corrSubset}}}{Extract specific subsets from results}
#'   \item{\code{\link{CorrCombo}}}{Class holding enumeration results}
#' }
#'
#' @seealso
#' Vignettes: \code{vignette("quickstart", package = "corrselect")},
#' \code{vignette("advanced", package = "corrselect")}
#'
#' @docType package
#' @name corrselect-package
#' @aliases corrselect
"_PACKAGE"
