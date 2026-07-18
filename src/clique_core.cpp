#include "clique_core.h"
#include <cmath>
#include <algorithm>

using namespace Rcpp;

AdjMatrix buildCompatibilityMatrix(const NumericMatrix& corMatrix, double threshold) {
  int n = corMatrix.nrow();
  AdjMatrix adj(n, std::vector<bool>(n, false));
  for (int i = 0; i < n - 1; ++i) {
    for (int j = i + 1; j < n; ++j) {
      bool ok = std::abs(corMatrix(i, j)) <= threshold;
      adj[i][j] = adj[j][i] = ok;
    }
  }
  return adj;
}

void bronKerboschPivot(
    const AdjMatrix& adj,
    std::vector<int>& R,
    std::vector<int> P,
    std::vector<int> X,
    bool usePivot,
    ComboList& out
) {
  if (P.empty() && X.empty()) {
    // R is built in whatever order the recursion visits vertices, not
    // ascending index order (the pivot rule can leave un-visited, still-P
    // vertices smaller than an already-chosen v, which then surface deeper
    // in the recursion) -- sort a copy before it's used as a combo, since
    // callers (e.g. corrPrune()/corrSubset()) index data-frame columns
    // directly with it and expect ascending order like every other search
    // path (ELS re-sorts its own results after remapping local indices).
    Combo combo = R;
    std::sort(combo.begin(), combo.end());
    out.push_back(std::move(combo));
    return;
  }

  std::vector<int> candidates;
  if (usePivot) {
    int pivot = -1;
    int bestCount = -1;
    auto consider = [&](int u) {
      int count = 0;
      for (int w : P) if (adj[u][w]) ++count;
      if (count > bestCount) { bestCount = count; pivot = u; }
    };
    for (int u : P) consider(u);
    for (int u : X) consider(u);

    for (int u : P) {
      if (pivot == -1 || !adj[pivot][u]) candidates.push_back(u);
    }
  } else {
    candidates = P;
  }

  for (int v : candidates) {
    std::vector<int> newP, newX;
    for (int u : P) if (u != v && adj[v][u]) newP.push_back(u);
    for (int u : X) if (adj[v][u]) newX.push_back(u);

    R.push_back(v);
    bronKerboschPivot(adj, R, std::move(newP), std::move(newX), usePivot, out);
    R.pop_back();

    P.erase(std::remove(P.begin(), P.end(), v), P.end());
    X.push_back(v);
  }
}
