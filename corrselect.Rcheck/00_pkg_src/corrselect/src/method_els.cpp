// method_els.cpp

#include "method_els.h"
#include "utils.h"
#include <Rcpp.h>
#include <vector>
#include <unordered_set>
#include <string>
#include <algorithm>
#include <numeric>
#include <cmath>

using namespace Rcpp;

// Helper to serialize a sorted Combo into a comma-separated string key
static std::string comboKey(const std::vector<int>& combo) {
  std::string key;
  for (size_t i = 0; i < combo.size(); ++i) {
    if (i > 0) key += ",";
    key += std::to_string(combo[i]);
  }
  return key;
}

// —————————————————————————————————————————————————————————————————————————————
// [[Rcpp::export]]
ComboList runELS(const NumericMatrix& corMatrix,
                 double threshold,
                 const Combo& forcedVec) {
  int n = corMatrix.nrow();
  if (n != corMatrix.ncol()) stop("Matrix must be square.");
  if (!validateMatrixStructure(corMatrix))
    stop("Matrix must be symmetric or upper triangular.");

  // 1. Precompute binary compatibility matrix
  std::vector<std::vector<bool>> compatible(n, std::vector<bool>(n, false));
  for (int i = 0; i < n - 1; ++i) {
    for (int j = i + 1; j < n; ++j) {
      bool ok = std::abs(corMatrix(i, j)) <= threshold;
      compatible[i][j] = compatible[j][i] = ok;
    }
  }

  std::vector<int> allNodes(n);
  std::iota(allNodes.begin(), allNodes.end(), 0);
  std::unordered_set<int> forcedSet(forcedVec.begin(), forcedVec.end());

  std::unordered_set<std::string> seen;
  ComboList results;

  // ————————————————————————————————
  // CASE 1: forcedVec is non-empty → expand once, skip seed loop
  // ————————————————————————————————
if (!forcedVec.empty()) {
  std::unordered_set<std::string> seen_local;

  // Determine seeds: nodes compatible with all of forcedVec
  std::vector<int> seeds;
  for (int v : allNodes) {
    if (forcedSet.count(v)) continue;

    bool ok = true;
    for (int f : forcedVec) {
      if (!compatible[v][f]) {
        ok = false;
        break;
      }
    }
    if (ok) seeds.push_back(v);
  }

  // Expand from each compatible seed
  for (int v : seeds) {
    Combo current = forcedVec;
    current.push_back(v);

    for (int u : allNodes) {
      if (forcedSet.count(u) || u == v) continue;

      bool ok = true;
      for (int w : current) {
        if (!compatible[u][w]) {
          ok = false;
          break;
        }
      }
      if (ok) current.push_back(u);
    }

    // Check if current is maximal
    bool maximal = true;
    for (int u : allNodes) {
      if (std::find(current.begin(), current.end(), u) != current.end()) continue;

      bool ok = true;
      for (int w : current) {
        if (!compatible[u][w]) {
          ok = false;
          break;
        }
      }
      if (ok) {
        maximal = false;
        break;
      }
    }

    if (maximal) {
      std::sort(current.begin(), current.end());
      std::string key = comboKey(current);
      if (!seen.count(key)) {
        seen.insert(key);
        results.push_back(current);
      }
    }
  }

  return results;
}


  // ————————————————————————————————
  // CASE 2: forcedVec is empty → normal ELS with seeds
  // ————————————————————————————————

  // Determine valid seeds
  std::vector<int> seeds;
  for (int v : allNodes) {
    bool ok = true;
    for (int f : forcedVec) {
      if (!compatible[v][f]) {
        ok = false;
        break;
      }
    }
    if (ok) seeds.push_back(v);
  }

  // Main loop over seeds
  for (int v : seeds) {
    Combo current = forcedVec;
    current.push_back(v);

    // Greedily add compatible nodes
    for (int u : allNodes) {
      if (forcedSet.count(u) || u == v) continue;

      bool ok = true;
      for (int w : current) {
        if (!compatible[u][w]) {
          ok = false;
          break;
        }
      }
      if (ok) current.push_back(u);
    }

    // Check if current is maximal
    bool maximal = true;
    for (int u : allNodes) {
      if (std::find(current.begin(), current.end(), u) != current.end()) continue;

      bool ok = true;
      for (int w : current) {
        if (!compatible[u][w]) {
          ok = false;
          break;
        }
      }
      if (ok) {
        maximal = false;
        break;
      }
    }

    if (maximal) {
      std::sort(current.begin(), current.end());
      std::string key = comboKey(current);
      if (!seen.count(key)) {
        seen.insert(key);
        results.push_back(current);
      }
    }
  }

  return results;
}
