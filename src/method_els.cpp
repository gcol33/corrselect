#include "method_els.h"
#include "clique_core.h"
#include "utils.h"
#include <unordered_set>
#include <algorithm>

using namespace Rcpp;

// Eppstein-Loffler-Strash (2010): for each vertex v of a graph, ordered by a
// degeneracy ordering, expand a single Bron-Kerbosch-with-pivot call seeded
// with R={v}, P=neighbors of v later in the order, X=neighbors earlier in
// the order. Every maximal clique has a unique earliest-ordered member, so
// this produces each maximal clique exactly once with no deduplication, and
// each per-vertex call is bounded by the graph's degeneracy d (size of P),
// giving the algorithm's near-optimal time bound on sparse graphs.
static void runELSOnSubgraph(const AdjMatrix& sub, ComboList& out) {
  int m = sub.size();
  if (m == 0) return;

  std::vector<int> degree(m, 0);
  for (int i = 0; i < m; ++i)
    for (int j = 0; j < m; ++j)
      if (sub[i][j]) ++degree[i];

  // Degeneracy ordering: repeatedly remove the minimum-degree remaining
  // vertex. order[k] = local vertex removed k-th.
  std::vector<bool> removed(m, false);
  std::vector<int> order;
  order.reserve(m);
  for (int step = 0; step < m; ++step) {
    int best = -1, bestDeg = -1;
    for (int i = 0; i < m; ++i) {
      if (removed[i]) continue;
      if (best == -1 || degree[i] < bestDeg) { best = i; bestDeg = degree[i]; }
    }
    removed[best] = true;
    order.push_back(best);
    for (int j = 0; j < m; ++j) {
      if (!removed[j] && sub[best][j]) degree[j]--;
    }
  }

  std::vector<int> orderPos(m);
  for (int k = 0; k < m; ++k) orderPos[order[k]] = k;

  for (int k = 0; k < m; ++k) {
    int v = order[k];
    std::vector<int> P, X;
    for (int u = 0; u < m; ++u) {
      if (u == v || !sub[v][u]) continue;
      if (orderPos[u] > orderPos[v]) P.push_back(u); else X.push_back(u);
    }
    bronKerboschPivot(sub, {v}, P, X, /*usePivot=*/true, out);
  }
}

// [[Rcpp::export]]
ComboList runELS(const NumericMatrix& corMatrix,
                 double threshold,
                 const Combo& forcedVec) {
  int n = corMatrix.nrow();
  validateCorMatrix(corMatrix);
  validateForcedIndices(forcedVec, n);

  AdjMatrix adj = buildCompatibilityMatrix(corMatrix, threshold);
  std::unordered_set<int> forcedSet(forcedVec.begin(), forcedVec.end());

  // H = vertices compatible with every forced vertex, excluding forcedVec
  // itself. A set forcedVec u S is a clique of the full graph iff S is a
  // clique of H (every member of H is, by construction, compatible with all
  // of forcedVec), and it is maximal iff S is maximal within H (any vertex
  // that could extend forcedVec u S must itself be in H, since it must be
  // compatible with every forced vertex). So enumerating maximal cliques
  // containing forcedVec reduces to running ELS on H and prepending
  // forcedVec to each result.
  std::vector<int> universe;
  for (int v = 0; v < n; ++v) {
    if (forcedSet.count(v)) continue;
    bool ok = true;
    for (int f : forcedVec) {
      if (!adj[v][f]) { ok = false; break; }
    }
    if (ok) universe.push_back(v);
  }

  int m = universe.size();
  ComboList cliquesInH;

  if (m == 0) {
    // No candidates to extend forcedVec with: the only maximal clique of the
    // (empty) graph H is the empty set.
    cliquesInH.push_back(Combo());
  } else {
    AdjMatrix sub(m, std::vector<bool>(m, false));
    for (int i = 0; i < m; ++i)
      for (int j = 0; j < m; ++j)
        if (i != j) sub[i][j] = adj[universe[i]][universe[j]];

    runELSOnSubgraph(sub, cliquesInH);
  }

  ComboList results;
  results.reserve(cliquesInH.size());
  for (auto& localClique : cliquesInH) {
    Combo combo = forcedVec;
    for (int localV : localClique) combo.push_back(universe[localV]);
    std::sort(combo.begin(), combo.end());
    results.push_back(std::move(combo));
  }

  return results;
}
