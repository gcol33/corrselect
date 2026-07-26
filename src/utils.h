#ifndef CORRSELECT_UTILS_H
#define CORRSELECT_UTILS_H

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <numeric>

#include "corrselect_types.h"

[[nodiscard]] double meanAbsCorrelation(const Rcpp::NumericMatrix& corMatrix, const Combo& comb);
[[nodiscard]] bool validateMatrixStructure(const Rcpp::NumericMatrix& corMatrix);

// Shared entry-point validation for the four Rcpp-exported backends
// (findAllMaxSets, runELS, runBronKerbosch, greedyPruneBackend): square +
// symmetric/upper-triangular matrix, and in-bounds, deduplicated 0-based
// forced indices. Each throws via Rcpp::stop() on failure.
void validateCorMatrix(const Rcpp::NumericMatrix& corMatrix);
void validateForcedIndices(Combo& forcedVec, int n);

// Converts a `force_in` argument as received from R (already 0-based, or
// R_NilValue when absent) into a Combo, expanding to an empty Combo when
// null. Shared by findAllMaxSets() (corrselect_main.cpp) and
// greedyPruneBackend() (method_greedy.cpp) so this conversion has exactly
// one implementation.
Combo forceInToCombo(Rcpp::Nullable<Rcpp::IntegerVector> force_in);

// Warns, without stopping, when two or more `force_in` indices are
// themselves mutually incompatible under threshold; `force_in` is still
// honored and forced into every returned subset regardless. Shared by
// runELS() and runBronKerbosch() (and so, transitively, by
// findAllMaxSets(), which dispatches to them) so every entry point that
// forces such variables in gives this signal, not only MatSelect() (#111).
void warnIfForcedMutuallyIncompatible(const Rcpp::NumericMatrix& corMatrix,
                                      double threshold,
                                      const Combo& forcedVec);

#endif
