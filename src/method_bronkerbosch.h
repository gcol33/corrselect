#ifndef CORRSELECT_METHOD_BRONKERBOSCH_H
#define CORRSELECT_METHOD_BRONKERBOSCH_H

#include <Rcpp.h>
#include "corrselect_types.h"

// Bron-Kerbosch enumeration of all maximal valid subsets, with optional
// pivoting. forcedVec is taken by value (not const&) because
// validateForcedIndices() deduplicates it in place. If two or more forced
// indices are themselves mutually incompatible under threshold, they are
// still forced into every returned subset, after a warning (via
// warnIfForcedMutuallyIncompatible() in utils.cpp) naming the offending pair
// -- the same signal MatSelect() gives, whether or not the caller goes
// through MatSelect() (#111).
ComboList runBronKerbosch(const Rcpp::NumericMatrix& corMatrix,
                          double threshold,
                          Combo forcedVec,
                          bool usePivot);

#endif
