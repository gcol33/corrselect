#ifndef CORRSELECT_UTILS_H
#define CORRSELECT_UTILS_H

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <numeric>

#include "corrselect_types.h"

double meanAbsCorrelation(const Rcpp::NumericMatrix& corMatrix, const Combo& comb);
bool validateMatrixStructure(const Rcpp::NumericMatrix& corMatrix);

// Shared entry-point validation for the four Rcpp-exported backends
// (findAllMaxSets, runELS, runBronKerbosch, greedyPruneBackend): square +
// symmetric/upper-triangular matrix, and in-bounds, deduplicated 0-based
// forced indices. Each throws via Rcpp::stop() on failure.
void validateCorMatrix(const Rcpp::NumericMatrix& corMatrix);
void validateForcedIndices(Combo& forcedVec, int n);

#endif
