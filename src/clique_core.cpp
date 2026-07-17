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
    std::vector<int> R,
    std::vector<int> P,
    std::vector<int> X,
    bool usePivot,
    ComboList& out
) {
  if (P.empty() && X.empty()) {
    out.push_back(R);
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
    std::vector<int> newR = R;
    newR.push_back(v);

    std::vector<int> newP, newX;
    for (int u : P) if (u != v && adj[v][u]) newP.push_back(u);
    for (int u : X) if (adj[v][u]) newX.push_back(u);

    bronKerboschPivot(adj, newR, newP, newX, usePivot, out);

    P.erase(std::remove(P.begin(), P.end(), v), P.end());
    X.push_back(v);
  }
}
