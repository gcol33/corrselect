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
// association matrix).
bool validateMatrixStructure(const NumericMatrix& corMatrix) {
  int n = corMatrix.nrow();
  bool isSymmetric = true, isUpper = true, hasUnitDiagonal = true;

  for (int i = 0; i < n; ++i) {
    if (std::abs(corMatrix(i, i) - 1.0) > kSymmetryTolerance) hasUnitDiagonal = false;
    for (int j = 0; j < i; ++j) {
      if (!NumericMatrix::is_na(corMatrix(i, j))) isUpper = false;
      if (std::abs(corMatrix(i, j) - corMatrix(j, i)) > kSymmetryTolerance) isSymmetric = false;
    }
  }

  return hasUnitDiagonal && (isSymmetric || isUpper);
}

void validateCorMatrix(const NumericMatrix& corMatrix) {
  if (corMatrix.nrow() != corMatrix.ncol()) stop("Matrix must be square.");
  if (!validateMatrixStructure(corMatrix))
    stop("Matrix must have a unit diagonal and be symmetric or upper triangular.");
}

void validateForcedIndices(const Combo& forcedVec, int n) {
  for (size_t i = 0; i < forcedVec.size(); ++i) {
    if (forcedVec[i] < 0 || forcedVec[i] >= n)
      stop("`force_in` must be valid 0-based column indices");
  }
}
