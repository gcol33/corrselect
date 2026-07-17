#ifndef CORRSELECT_CLIQUE_CORE_H
#define CORRSELECT_CLIQUE_CORE_H

#include <Rcpp.h>
#include <vector>
#include "corrselect_types.h"

// Boolean adjacency/compatibility matrix: edge (i,j) exists iff variables i
// and j may coexist in a subset, i.e. abs(corMatrix(i,j)) <= threshold.
typedef std::vector<std::vector<bool>> AdjMatrix;

AdjMatrix buildCompatibilityMatrix(const Rcpp::NumericMatrix& corMatrix, double threshold);

// Standard Bron-Kerbosch maximal-clique enumeration with optional pivoting
// (Tomita et al. pivot rule: maximize |P intersect N(pivot)|). Appends each
// maximal clique found (vertex indices local to `adj`) to `out`. Correctness
// does not depend on how R/P/X are seeded; callers determine which named
// algorithm this instantiates via that seeding: plain Bron-Kerbosch seeds
// R/P/X once over the whole graph, while Eppstein-Loffler-Strash calls this
// once per vertex in degeneracy order with P/X split into later/earlier
// neighbors.
void bronKerboschPivot(
    const AdjMatrix& adj,
    std::vector<int> R,
    std::vector<int> P,
    std::vector<int> X,
    bool usePivot,
    ComboList& out
);

#endif
