#ifndef CORRSELECT_METHOD_ELS_H
#define CORRSELECT_METHOD_ELS_H

#include <Rcpp.h>
#include "corrselect_types.h"

// Eppstein-Loffler-Strash enumeration of all maximal valid subsets.
// forcedVec is taken by value (not const&) because validateForcedIndices()
// deduplicates it in place. If two or more forced indices are themselves
// mutually incompatible under threshold, they are still forced into every
// returned subset, after a warning (via warnIfForcedMutuallyIncompatible() in
// utils.cpp) naming the offending pair -- the same signal MatSelect() gives,
// whether or not the caller goes through MatSelect() (#111).
ComboList runELS(const Rcpp::NumericMatrix& corMatrix,
                 double threshold,
                 Combo forcedVec);

#endif
