#include "utils.h"
#include <cmath>
#include <algorithm>

using namespace Rcpp;

// Compute mean absolute correlation for a subset
double meanAbsCorrelation(const NumericMatrix& corMatrix, const Combo& comb) {
  double sum = 0.0;
  int count = 0;
  for (size_t i = 0; i < comb.size(); ++i) {
    for (size_t j = i + 1; j < comb.size(); ++j) {
      sum += std::abs(corMatrix(comb[i], comb[j]));
      ++count;
    }
  }
  return count > 0 ? sum / count : 0.0;
}

// Matches the 1e-8 tolerance used by the R-level symmetry check in MatSelect().
static const double kSymmetryTolerance = 1e-8;

// Check if matrix is symmetric (within tolerance) or upper triangular,
// AND has a unit diagonal (required either way for a correlation/
// association matrix). NaN/NA entries never satisfy either check: an
// IEEE-754 comparison against NaN is always false, so a bare `> tolerance`
// comparison would let a NaN silently pass validation instead of tripping
// it -- `std::isnan()` guards are used explicitly wherever a comparison
// alone would otherwise be fooled.
bool validateMatrixStructure(const NumericMatrix& corMatrix) {
  int n = corMatrix.nrow();
  bool isSymmetric = true, isUpper = true, hasUnitDiagonal = true;

  for (int i = 0; i < n; ++i) {
    double diag = corMatrix(i, i);
    if (std::isnan(diag) || std::abs(diag - 1.0) > kSymmetryTolerance) hasUnitDiagonal = false;
    for (int j = 0; j < i; ++j) {
      double lower = corMatrix(i, j);
      double upper = corMatrix(j, i);
      if (!NumericMatrix::is_na(lower)) isUpper = false;
      if (std::isnan(lower) || std::isnan(upper) ||
          std::abs(lower - upper) > kSymmetryTolerance) {
        isSymmetric = false;
      }
    }
  }

  return hasUnitDiagonal && (isSymmetric || isUpper);
}

void validateCorMatrix(const NumericMatrix& corMatrix) {
  if (corMatrix.nrow() != corMatrix.ncol()) stop("Matrix must be square.");
  if (!validateMatrixStructure(corMatrix))
    stop("Matrix must have a unit diagonal and be symmetric or upper triangular.");
}

// Shared choke point for all `force_in`-accepting Rcpp entry points
// (findAllMaxSets, runELS, runBronKerbosch, greedyPruneBackend): validates
// bounds AND deduplicates, so a repeated index can never reach the search
// core of any of them (a duplicate would otherwise resurface as the same
// variable listed twice in a returned combo -- see #31).
void validateForcedIndices(Combo& forcedVec, int n) {
  for (size_t i = 0; i < forcedVec.size(); ++i) {
    if (forcedVec[i] < 0 || forcedVec[i] >= n)
      stop("`force_in` must be valid 0-based column indices");
  }
  std::sort(forcedVec.begin(), forcedVec.end());
  forcedVec.erase(std::unique(forcedVec.begin(), forcedVec.end()), forcedVec.end());
}
