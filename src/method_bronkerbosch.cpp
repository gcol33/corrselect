#include "method_bronkerbosch.h"
#include "clique_core.h"
#include "utils.h"
#include <unordered_set>
#include <algorithm>
#include <numeric>

using namespace Rcpp;

// [[Rcpp::export]]
ComboList runBronKerbosch(const NumericMatrix& corMatrix,
                          double threshold,
                          Combo forcedVec,
                          bool usePivot) {
  int n = corMatrix.nrow();
  validateCorMatrix(corMatrix);
  validateForcedIndices(forcedVec, n);

  AdjMatrix adj = buildCompatibilityMatrix(corMatrix, threshold);

  // Initial P = neighbors common to all forcedVec (or all nodes if none forced)
  std::vector<int> P;
  if (forcedVec.empty()) {
    P.resize(n);
    std::iota(P.begin(), P.end(), 0);
  } else {
    std::unordered_set<int> common;
    for (int u = 0; u < n; ++u) if (adj[forcedVec[0]][u]) common.insert(u);
    for (size_t k = 1; k < forcedVec.size(); ++k) {
      std::unordered_set<int> next;
      for (int u : common) if (adj[forcedVec[k]][u]) next.insert(u);
      common.swap(next);
    }
    for (int f : forcedVec) common.erase(f);
    P.assign(common.begin(), common.end());
    std::sort(P.begin(), P.end());
  }

  std::vector<int> R = forcedVec;
  std::vector<int> X;

  ComboList allMaxCliques;
  bronKerboschPivot(adj, R, P, X, usePivot, allMaxCliques);

  return allMaxCliques;
}
