#ifndef METHOD_GREEDY_H
#define METHOD_GREEDY_H

#include <Rcpp.h>
#include "corrselect_types.h"

// Greedy pruning algorithm
// Iteratively removes variables with highest "badness" score until
// all pairwise associations are below threshold
Combo greedyPrune(
    const Rcpp::NumericMatrix& assocMatrix,
    double threshold,
    const Combo& force_in
);

// Helper: compute maximum association between var and any other active variable
double computeMaxAssoc(
    const Rcpp::NumericMatrix& A,
    int var,
    const std::vector<bool>& active
);

// Helper: compute average association between var and all other active variables
double computeAvgAssoc(
    const Rcpp::NumericMatrix& A,
    int var,
    const std::vector<bool>& active
);

#endif
