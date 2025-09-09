#ifndef CORRSELECT_UTILS_H
#define CORRSELECT_UTILS_H

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <numeric>

#include "corrselect_types.h"

bool isValidAddition(const Rcpp::NumericMatrix& corMatrix, const Combo& current, int newIdx, double threshold);
double meanAbsCorrelation(const Rcpp::NumericMatrix& corMatrix, const Combo& comb);
bool validateMatrixStructure(const Rcpp::NumericMatrix& corMatrix);
bool isValidCombination(const Rcpp::NumericMatrix& corMatrix, const Combo& comb, double threshold);

#endif
