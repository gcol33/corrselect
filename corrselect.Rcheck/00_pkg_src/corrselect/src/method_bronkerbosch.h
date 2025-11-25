#ifndef CORRSELECT_METHOD_BRONKERBOSCH_H
#define CORRSELECT_METHOD_BRONKERBOSCH_H

#include <Rcpp.h>
#include "corrselect_types.h"

// Bron–Kerbosch enumeration of all maximal valid subsets,
// with optional pivoting
ComboList runBronKerbosch(const Rcpp::NumericMatrix& corMatrix,
                          double threshold,
                          const Combo& forcedVec,
                          bool usePivot);

#endif
