#ifndef CORRSELECT_METHOD_SPECTRAL_H
#define CORRSELECT_METHOD_SPECTRAL_H

#include <Rcpp.h>
#include "corrselect_types.h"
using namespace Rcpp;

ComboList runSpectral(const NumericMatrix& corMatrix,
                      double threshold,
                      const Combo& forcedVec,
                      int lengthLimit,
                      Nullable<int> k_param);

#endif
