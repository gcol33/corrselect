#ifndef CORRSELECT_UTILS_H
#define CORRSELECT_UTILS_H

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <numeric>

#include "corrselect_types.h"

double meanAbsCorrelation(const Rcpp::NumericMatrix& corMatrix, const Combo& comb);
bool validateMatrixStructure(const Rcpp::NumericMatrix& corMatrix);

// Shared entry-point validation for the three Rcpp-exported backends
// (findAllMaxSets, runELS, runBronKerbosch): square + symmetric/upper-
// triangular matrix, and in-bounds 0-based forced indices. Each throws via
// Rcpp::stop() on failure.
void validateCorMatrix(const Rcpp::NumericMatrix& corMatrix);
void validateForcedIndices(const Combo& forcedVec, int n);

#endif
