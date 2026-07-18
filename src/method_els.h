#ifndef CORRSELECT_METHOD_ELS_H
#define CORRSELECT_METHOD_ELS_H

#include <Rcpp.h>
#include "corrselect_types.h"

// Eppstein–Löffler–Strash enumeration of all maximal valid subsets.
// forcedVec is taken by value (not const&) because validateForcedIndices()
// deduplicates it in place.
ComboList runELS(const Rcpp::NumericMatrix& corMatrix,
                 double threshold,
                 Combo forcedVec);

#endif
