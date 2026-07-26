#ifndef CORRSELECT_CLIQUE_CORE_H
#define CORRSELECT_CLIQUE_CORE_H

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include "corrselect_types.h"

// Compatibility predicate: true iff variables a and b may coexist in a
// subset, i.e. abs(corMatrix(a,b)) <= threshold. corMatrix may be stored
// upper-triangular-only (lower triangle = NA, see validateMatrixStructure()
// in utils.cpp), so the pair is always read as (min, max) rather than
// trusting a < b. Shared by buildCompatibilityMatrix() below and by
// method_els.cpp's induced-subgraph build, so the compatibility definition
// has exactly one implementation.
inline bool isCompatible(const Rcpp::NumericMatrix& corMatrix, double threshold, int a, int b) {
  return std::abs(corMatrix(std::min(a, b), std::max(a, b))) <= threshold;
}

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
//
// R is taken by reference and backtracked in place (push_back before each
// recursive call, pop_back after) rather than copied per call, since it is
// purely a growing/shrinking stack; by the time this function returns, R is
// restored to its original contents. P and X are taken by value because
// each recursive branch genuinely needs its own filtered subset.
void bronKerboschPivot(
    const AdjMatrix& adj,
    std::vector<int>& R,
    std::vector<int> P,
    std::vector<int> X,
    bool usePivot,
    ComboList& out
);

#endif
