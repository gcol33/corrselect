#include <Rcpp.h>
#include "method_spectral.h"
#include "utils.h"
#include <algorithm>
#include <map>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
ComboList runSpectral(const NumericMatrix& corMatrix, double threshold,
                      const Combo& forcedVec,
                      int lengthLimit,
                      Rcpp::Nullable<int> k_param = R_NilValue) {
  int n = corMatrix.nrow();
  if (n != corMatrix.ncol()) stop("Matrix must be square.");

  // Step 0: Check for non-finite values in the correlation matrix
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      if (!std::isfinite(corMatrix(i, j))) {
        Rcpp::warning("Correlation matrix contains NA or infinite values. Skipping spectral method.");
        return ComboList(); // Return empty instead of throwing
      }
    }
  }

  // Step 1: Build similarity matrix
  NumericMatrix S(n, n);
  for (int i = 0; i < n; ++i) {
    S(i, i) = 0;
    for (int j = i + 1; j < n; ++j) {
      double sim = 1.0 - std::abs(corMatrix(i, j));
      S(i, j) = sim;
      S(j, i) = sim;
    }
  }

  // Step 2: Degree vector
  NumericVector D(n);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      D[i] += S(i, j);
    }
  }

  // Step 3: Validate degrees
  for (int i = 0; i < n; ++i) {
    if (!std::isfinite(D[i]) || D[i] <= 0.0) {
      Rcpp::warning("Spectral method failed: Laplacian has non-positive or non-finite degrees. Skipping.");
      return ComboList(); // Return empty instead of throwing
    }
  }

  // Step 4: Normalized Laplacian L = I - D^{-1/2} * S * D^{-1/2}
  NumericMatrix L(n, n);
  for (int i = 0; i < n; ++i) {
    double di_sqrt_inv = 1.0 / std::sqrt(D[i]);
    for (int j = 0; j < n; ++j) {
      double dj_sqrt_inv = 1.0 / std::sqrt(D[j]);
      double entry = (i == j ? 1.0 : 0.0) - di_sqrt_inv * S(i, j) * dj_sqrt_inv;
      L(i, j) = entry;
    }
  }

  // Step 5: Eigen decomposition using R
  Environment base("package:base");
  Function eigen_func = base["eigen"];
  List eig = eigen_func(_["x"] = L, _["symmetric"] = true);
  NumericMatrix vectors = eig["vectors"];

  // Step 6: Choose first k eigenvectors
  int k = k_param.isNotNull() ? Rcpp::as<int>(k_param) :
            std::max(2, (int)std::round(std::sqrt(n / 2.0)));
  if (k < 2) k = 2;
  if (k > vectors.ncol()) k = vectors.ncol();

  NumericMatrix features(n, k);
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < k; ++j)
      features(i, j) = vectors(i, j);

  // Step 7: K-means clustering in R
  Environment stats("package:stats");
  Function kmeans = stats["kmeans"];
  List kmeans_result = kmeans(features, _["centers"] = k, _["nstart"] = 10);
  IntegerVector cluster_assignment = kmeans_result["cluster"];
  cluster_assignment = cluster_assignment - 1;

  // Step 8: Form clusters
  std::map<int, Combo> clusters;
  for (int i = 0; i < n; ++i) {
    clusters[cluster_assignment[i]].push_back(i);
  }

  // Step 9: Add forced variables and filter
  ComboList out;
  for (const auto& kv : clusters) {
    Combo group = kv.second;

    // Add forced variables if not already present
    for (int idx : forcedVec) {
      if (std::find(group.begin(), group.end(), idx) == group.end())
        group.push_back(idx);
    }

    if ((int)group.size() < 2) continue;
    if ((int)group.size() > lengthLimit) continue;
    if (isValidCombination(corMatrix, group, threshold))
      out.push_back(group);
  }

  // Step 10: fallback — only forcedVec if valid
  if (out.empty() && forcedVec.size() >= 2 && isValidCombination(corMatrix, forcedVec, threshold)) {
    out.push_back(forcedVec);
  }

  return out;
}
