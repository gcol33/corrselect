#include "method_bronkerbosch.h"
#include "utils.h"
#include <algorithm>
#include <unordered_set>

using namespace Rcpp;

// Recursive Bron–Kerbosch algorithm with optional pivoting
void bronKerboschRecursive(
    const std::vector<std::vector<int>>& graph,
    std::vector<int>& R,
    std::vector<int> P,
    std::vector<int> X,
    std::vector<Combo>& output,
    bool usePivot
) {
  if (P.empty() && X.empty()) {
    output.push_back(R);
    return;
  }

  std::vector<int> pivot_neighbors;
  if (usePivot && (!P.empty() || !X.empty())) {
    int pivot = !P.empty() ? P[0] : X[0];
    pivot_neighbors = graph[pivot];
  }

  std::vector<int> candidates;
  if (usePivot && !pivot_neighbors.empty()) {
    // only consider P \ N(pivot)
    for (int v : P) {
      if (std::find(pivot_neighbors.begin(), pivot_neighbors.end(), v) == pivot_neighbors.end()) {
        candidates.push_back(v);
      }
    }
  } else {
    candidates = P;
  }

  for (int v : candidates) {
    std::vector<int> newR = R;
    newR.push_back(v);

    // Build newP = P ∩ N(v), newX = X ∩ N(v)
    std::vector<int> newP, newX;
    for (int u : P) {
      if (std::find(graph[v].begin(), graph[v].end(), u) != graph[v].end()) {
        newP.push_back(u);
      }
    }
    for (int u : X) {
      if (std::find(graph[v].begin(), graph[v].end(), u) != graph[v].end()) {
        newX.push_back(u);
      }
    }

    bronKerboschRecursive(graph, newR, newP, newX, output, usePivot);

    // Move v from P to X
    P.erase(std::remove(P.begin(), P.end(), v), P.end());
    X.push_back(v);
  }
}

// [[Rcpp::export]]
ComboList runBronKerbosch(const NumericMatrix& corMatrix,
                          double threshold,
                          const Combo& forcedVec,
                          bool usePivot) {
  int n = corMatrix.nrow();
  if (n != corMatrix.ncol()) stop("Matrix must be square.");
  if (!validateMatrixStructure(corMatrix))
    stop("Matrix must be symmetric or upper triangular.");

  // 1) Build adjacency: edge if abs(corr) <= threshold
  std::vector<std::vector<int>> graph(n);
  for (int i = 0; i < n - 1; ++i) {
    for (int j = i + 1; j < n; ++j) {
      if (std::abs(corMatrix(i, j)) <= threshold) {
        graph[i].push_back(j);
        graph[j].push_back(i);
      }
    }
  }

  // 2) Compute initial P = neighbors common to all forcedVec (or all nodes if none forced)
  std::vector<int> P;
  if (forcedVec.empty()) {
    P.resize(n);
    std::iota(P.begin(), P.end(), 0);
  } else {
    // intersect neighbor lists of each forced node
    std::unordered_set<int> common(graph[forcedVec[0]].begin(),
                                   graph[forcedVec[0]].end());
    for (size_t k = 1; k < forcedVec.size(); ++k) {
      std::unordered_set<int> next;
      for (int v : graph[forcedVec[k]]) {
        if (common.count(v)) next.insert(v);
      }
      common.swap(next);
    }
    // remove forced nodes themselves
    for (int f : forcedVec) common.erase(f);
    P.assign(common.begin(), common.end());
    std::sort(P.begin(), P.end());
  }

  // 3) Initialize R = forcedVec, X = {}
  std::vector<int> R = forcedVec;
  std::vector<int> X;

  // 4) Run recursion
  ComboList allMaxCliques;
  bronKerboschRecursive(graph, R, P, X, allMaxCliques, usePivot);

  return allMaxCliques;
}
