#ifndef CORRSELECT_METHOD_ELS_H
#define CORRSELECT_METHOD_ELS_H

#include <Rcpp.h>
#include "corrselect_types.h"

// Eppstein-Loffler-Strash enumeration of all maximal valid subsets.
// forcedVec is taken by value (not const&) because validateForcedIndices()
// deduplicates it in place. Forced indices that are mutually incompatible
// under threshold are still forced into every returned subset, after the
// warning documented on warnIfForcedMutuallyIncompatible() in utils.h.
ComboList runELS(const Rcpp::NumericMatrix& corMatrix,
                 double threshold,
                 Combo forcedVec);

#endif
