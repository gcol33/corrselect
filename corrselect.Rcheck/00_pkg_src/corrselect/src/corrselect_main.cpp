#include <Rcpp.h>
#include "corrselect_types.h"
#include "utils.h"
#include "method_els.h"
#include "method_bronkerbosch.h"

using namespace Rcpp;

// [[Rcpp::export]]
List findAllMaxSets(
    NumericMatrix corMatrix,
    double threshold,
    std::string method = "els",
    Nullable<IntegerVector> force_in = R_NilValue,
    Nullable<bool> use_pivot         = R_NilValue
) {
  // 1) Basic checks
  int n = corMatrix.nrow();
  if (n != corMatrix.ncol()) stop("Matrix must be square.");
  if (!validateMatrixStructure(corMatrix))
    stop("Matrix must be symmetric or upper triangular.");

  // 2) Build forcedVec (expecting 0-based indices from R)
  Combo forcedVec;
  if (force_in.isNotNull()) {
    IntegerVector f = force_in.get();
    for (int i = 0; i < f.size(); ++i) {
      if (f[i] < 0 || f[i] >= n)
        stop("`force_in` must be valid 0-based column indices");
      forcedVec.push_back(f[i]);
    }
  }

  // 3) Dispatch to selected algorithm
  ComboList results;
  if (method == "els") {
    results = runELS(corMatrix, threshold, forcedVec);

  } else if (method == "bron_kerbosch") {
    bool pivot = false;
    if (use_pivot.isNotNull()) {
      pivot = as<bool>(use_pivot);
    }
    results = runBronKerbosch(corMatrix, threshold, forcedVec, pivot);

  } else {
    stop("Unknown method. Use 'els' or 'bron_kerbosch'.");
  }

  // 4) Sort: size descending, then avg-correlation ascending
  std::sort(results.begin(), results.end(),
    [&](const Combo &a, const Combo &b) {
      if (a.size() != b.size()) return a.size() > b.size();
      return meanAbsCorrelation(corMatrix, a)
           < meanAbsCorrelation(corMatrix, b);
    });

  // 5) Format output into R list (convert back to 1-based)
  List out;
  for (const Combo &combo : results) {
    IntegerVector v(combo.begin(), combo.end());
    v = v + 1; // convert to 1-based for R
    out.push_back(List::create(
      Named("combo")    = v,
      Named("avg_corr") = meanAbsCorrelation(corMatrix, combo)
    ));
  }

  return out;
}
