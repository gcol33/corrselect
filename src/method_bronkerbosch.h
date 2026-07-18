#ifndef CORRSELECT_METHOD_BRONKERBOSCH_H
#define CORRSELECT_METHOD_BRONKERBOSCH_H

#include <Rcpp.h>
#include "corrselect_types.h"

// Bron–Kerbosch enumeration of all maximal valid subsets, with optional
// pivoting. forcedVec is taken by value (not const&) because
// validateForcedIndices() deduplicates it in place.
ComboList runBronKerbosch(const Rcpp::NumericMatrix& corMatrix,
                          double threshold,
                          Combo forcedVec,
                          bool usePivot);

#endif
