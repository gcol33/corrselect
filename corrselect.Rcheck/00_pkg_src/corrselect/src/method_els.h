#ifndef CORRSELECT_METHOD_ELS_H
#define CORRSELECT_METHOD_ELS_H

#include <Rcpp.h>
#include "corrselect_types.h"

// Eppstein–Löffler–Strash enumeration of all maximal valid subsets
ComboList runELS(const Rcpp::NumericMatrix& corMatrix,
                 double threshold,
                 const Combo& forcedVec);

#endif
