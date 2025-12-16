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

// Check if matrix is symmetric or upper triangular
bool validateMatrixStructure(const NumericMatrix& corMatrix) {
  int n = corMatrix.nrow();
  bool isSymmetric = true, isUpper = true;

  for (int i = 0; i < n; ++i) {
    if (corMatrix(i, i) != 1) isUpper = false;
    for (int j = 0; j < i; ++j) {
      if (!NumericMatrix::is_na(corMatrix(i, j))) isUpper = false;
      if (corMatrix(i, j) != corMatrix(j, i)) isSymmetric = false;
    }
  }

  return isSymmetric || isUpper;
}
