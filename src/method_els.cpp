#include "method_els.h"
#include "utils.h"
#include <unordered_set>
#include <algorithm>

// Internal degeneracy-ordering based ELS
namespace {

// Helper: Degeneracy ordering
std::vector<int> degeneracy_ordering(const std::vector<std::vector<int>>& adj) {
  int n = adj.size();
  std::vector<int> degree(n), order, bucket(n);
  std::vector<bool> used(n, false);

  for (int i = 0; i < n; ++i) {
    degree[i] = adj[i].size();
    bucket[degree[i]]++;
  }

  int max_deg = *std::max_element(degree.begin(), degree.end());
  std::vector<std::vector<int>> bins(max_deg + 1);
  for (int i = 0; i < n; ++i)
    bins[degree[i]].push_back(i);

  for (int k = 0; k <= max_deg; ++k) {
    while (!bins[k].empty()) {
      int v = bins[k].back();
      bins[k].pop_back();
      if (used[v]) continue;
      used[v] = true;
      order.push_back(v);
      for (int u : adj[v]) {
        if (!used[u] && degree[u] > 0) {
          bins[degree[u]].erase(std::remove(bins[degree[u]].begin(), bins[degree[u]].end(), u), bins[degree[u]].end());
          degree[u]--;
          bins[degree[u]].push_back(u);
        }
      }
    }
  }

  std::reverse(order.begin(), order.end()); // highest degeneracy first
  return order;
}

// ELS expansion with degeneracy ordering
void els_expand(const std::vector<std::vector<int>>& adj,
                const std::unordered_set<int>& forcedSet,
                Combo current,
                std::unordered_set<int> candidates,
                ComboList& results) {

  if (candidates.empty()) {
    // Check if current subset includes forced-in variables
    for (int f : forcedSet) {
      if (std::find(current.begin(), current.end(), f) == current.end())
        return;
    }
    if (!current.empty()) results.push_back(current);
    return;
  }

  // pick candidate with lowest degeneracy (smallest degree first)
  int v = *candidates.begin();

  // Exclude v and expand
  candidates.erase(v);
  els_expand(adj, forcedSet, current, candidates, results);

  // Include v if valid
  bool valid = true;
  for (int u : current) {
    if (std::find(adj[v].begin(), adj[v].end(), u) == adj[v].end()) {
      valid = false;
      break;
    }
  }
  if (!valid) return;

  // Include v and restrict candidates to neighbors of v
  current.push_back(v);
  std::unordered_set<int> new_candidates;
  for (int w : candidates) {
    if (std::find(adj[v].begin(), adj[v].end(), w) != adj[v].end())
      new_candidates.insert(w);
  }
  els_expand(adj, forcedSet, current, new_candidates, results);
}

void findMaximalSetsELS(
    const Rcpp::NumericMatrix& mat,
    double threshold,
    const Combo& forcedVec,
    ComboList& results
) {
  int n = mat.ncol();

  // Adjacency list: edge ⇔ abs(corr) ≤ threshold
  std::vector<std::vector<int>> adj(n);
  for (int i = 0; i < n; ++i) {
    for (int j = i + 1; j < n; ++j) {
      if (std::abs(mat(i, j)) <= threshold) {
        adj[i].push_back(j);
        adj[j].push_back(i);
      }
    }
  }

  // Degeneracy ordering
  std::vector<int> order = degeneracy_ordering(adj);

  // Quick forced-in lookup
  std::unordered_set<int> forcedSet(forcedVec.begin(), forcedVec.end());

  // Start recursive expansion
  std::unordered_set<int> candidates(order.begin(), order.end());
  Combo current;
  els_expand(adj, forcedSet, current, candidates, results);
}

} // namespace

// [[Rcpp::export]]
ComboList runELS(const Rcpp::NumericMatrix& corMatrix,
                 double threshold,
                 const Combo& forcedVec) {
  ComboList out;
  findMaximalSetsELS(corMatrix, threshold, forcedVec, out);
  return out;
}
