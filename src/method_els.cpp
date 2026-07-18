#include "method_els.h"
#include "clique_core.h"
#include "utils.h"
#include <unordered_set>
#include <algorithm>

using namespace Rcpp;

// Mirrors buildCompatibilityMatrix()'s convention (clique_core.cpp): corMatrix
// may be stored upper-triangular-only, so a pair is always read as (min, max).
static inline bool isCompatible(const NumericMatrix& corMatrix, double threshold, int a, int b) {
  return std::abs(corMatrix(std::min(a, b), std::max(a, b))) <= threshold;
}

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

  // Adjacency lists (built once, same O(m^2) cost as the dense matrix
  // itself) let the degeneracy peel below run in O(m + E) instead of
  // repeatedly rescanning all m vertices per step.
  std::vector<std::vector<int>> neighbors(m);
  std::vector<int> degree(m, 0);
  for (int i = 0; i < m; ++i) {
    for (int j = 0; j < m; ++j) {
      if (i != j && sub[i][j]) neighbors[i].push_back(j);
    }
    degree[i] = static_cast<int>(neighbors[i].size());
  }

  // Bucket-queue degeneracy ordering (Matula-Beck): vertices are bucketed by
  // current degree; each step pops a minimum-degree vertex and decrements
  // its remaining neighbors' buckets in place. order[k] = local vertex
  // removed k-th. Any valid degeneracy ordering preserves ELS's correctness
  // guarantees regardless of how same-degree ties are broken.
  int maxDeg = 0;
  for (int d : degree) maxDeg = std::max(maxDeg, d);
  std::vector<std::vector<int>> bucket(maxDeg + 1);
  std::vector<int> bucketPos(m);
  std::vector<int> curDeg = degree;
  for (int i = 0; i < m; ++i) {
    bucket[curDeg[i]].push_back(i);
    bucketPos[i] = static_cast<int>(bucket[curDeg[i]].size()) - 1;
  }

  std::vector<bool> removed(m, false);
  std::vector<int> order;
  order.reserve(m);
  int curMinDeg = 0;
  for (int step = 0; step < m; ++step) {
    while (curMinDeg <= maxDeg && bucket[curMinDeg].empty()) ++curMinDeg;
    int v = bucket[curMinDeg].back();
    bucket[curMinDeg].pop_back();
    removed[v] = true;
    order.push_back(v);
    for (int u : neighbors[v]) {
      if (removed[u]) continue;
      int d = curDeg[u];
      int pos = bucketPos[u];
      int lastVertex = bucket[d].back();
      bucket[d][pos] = lastVertex;
      bucketPos[lastVertex] = pos;
      bucket[d].pop_back();
      curDeg[u] = d - 1;
      bucket[d - 1].push_back(u);
      bucketPos[u] = static_cast<int>(bucket[d - 1].size()) - 1;
      if (d - 1 < curMinDeg) curMinDeg = d - 1;
    }
  }

  std::vector<int> orderPos(m);
  for (int k = 0; k < m; ++k) orderPos[order[k]] = k;

  for (int k = 0; k < m; ++k) {
    int v = order[k];
    std::vector<int> P, X;
    for (int u : neighbors[v]) {
      if (orderPos[u] > orderPos[v]) P.push_back(u); else X.push_back(u);
    }
    std::vector<int> R{v};
    bronKerboschPivot(sub, R, P, X, /*usePivot=*/true, out);
  }
}

// [[Rcpp::export]]
ComboList runELS(const NumericMatrix& corMatrix,
                 double threshold,
                 Combo forcedVec) {
  int n = corMatrix.nrow();
  validateCorMatrix(corMatrix);
  validateForcedIndices(forcedVec, n);

  std::unordered_set<int> forcedSet(forcedVec.begin(), forcedVec.end());

  // H = vertices compatible with every forced vertex, excluding forcedVec
  // itself. A set forcedVec u S is a clique of the full graph iff S is a
  // clique of H (every member of H is, by construction, compatible with all
  // of forcedVec), and it is maximal iff S is maximal within H (any vertex
  // that could extend forcedVec u S must itself be in H, since it must be
  // compatible with every forced vertex). So enumerating maximal cliques
  // containing forcedVec reduces to running ELS on H and prepending
  // forcedVec to each result.
  //
  // Compatibility is checked directly against corMatrix (O(n * |forcedVec|)
  // here, O(m^2) for the induced submatrix below) rather than materializing
  // the full n x n compatibility matrix first -- when force_in restricts H
  // to a small subgraph, that full O(n^2) build would otherwise dominate the
  // cost of the rest of this function, which is exactly the case ELS is
  // recommended for (CLAUDE.md: "Recommended when using force_in").
  std::vector<int> universe;
  for (int v = 0; v < n; ++v) {
    if (forcedSet.count(v)) continue;
    bool ok = true;
    for (int f : forcedVec) {
      if (!isCompatible(corMatrix, threshold, v, f)) { ok = false; break; }
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
        if (i != j) sub[i][j] = isCompatible(corMatrix, threshold, universe[i], universe[j]);

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
