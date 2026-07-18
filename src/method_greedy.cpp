#include "method_greedy.h"
#include "utils.h"
#include <Rcpp.h>
#include <vector>
#include <map>
#include <algorithm>
#include <cmath>
#include <limits>

using namespace Rcpp;

// Reads the association between variables i and j from the upper triangle
// only (i<j), mirroring buildCompatibilityMatrix()'s i<j convention
// (clique_core.cpp). validateMatrixStructure() permits a validly
// upper-triangular-only matrix (lower triangle = NA); reading raw A(i,j)
// for both i<j and i>j would misread such a matrix as having NA on roughly
// half of all pairs.
inline double assocAt(const NumericMatrix& A, int i, int j) {
    return (i < j) ? A(i, j) : A(j, i);
}

// Helper: compute maximum association between var and any other active variable.
// An undefined (NaN) association is treated as worse than any defined value, so a
// variable involved in an undefined association is never silently preferred for
// retention during tie-breaking.
double computeMaxAssoc(
    const NumericMatrix& A,
    int var,
    const std::vector<bool>& active
) {
    double max_val = 0.0;
    int n = A.nrow();
    for (int j = 0; j < n; j++) {
        if (j == var || !active[j]) continue;
        double val = assocAt(A, var, j);
        if (std::isnan(val)) return std::numeric_limits<double>::infinity();
        val = std::fabs(val);
        if (val > max_val) {
            max_val = val;
        }
    }
    return max_val;
}

// Helper: compute average association between var and all other active variables.
// Undefined (NaN) associations are excluded from the average (the badness-count
// and max-association criteria already ensure a NaN pair is never silently
// treated as compatible; see the violation scan in greedyPrune()).
double computeAvgAssoc(
    const NumericMatrix& A,
    int var,
    const std::vector<bool>& active
) {
    double sum = 0.0;
    int count = 0;
    int n = A.nrow();
    for (int j = 0; j < n; j++) {
        if (j == var || !active[j]) continue;
        double val = assocAt(A, var, j);
        if (std::isnan(val)) continue;
        sum += std::fabs(val);
        count++;
    }
    return (count > 0) ? (sum / count) : 0.0;
}

// Main greedy pruning algorithm
Combo greedyPrune(
    const NumericMatrix& A,
    double threshold,
    const Combo& force_in
) {
    int n = A.nrow();

    // Initialize with all variables active
    std::vector<bool> active(n, true);

    // Mark force_in variables as protected. Bounds are already checked by
    // validateForcedIndices() in the caller (greedyPruneBackend); force_in
    // is trusted here.
    std::vector<bool> protected_vars(n, false);
    for (int idx : force_in) {
        protected_vars[idx] = true;
    }

    // Incrementally maintained per-variable tie-break statistics, so each
    // outer iteration doesn't have to rescan every neighbor from scratch for
    // every candidate (see #60). sumAbs/cnt/nanCount track the running
    // average association exactly, updated in O(1) per neighbor whenever a
    // variable is removed. maxVal is a cached max association that is only
    // recomputed (via computeMaxAssoc's O(n) scan) for a variable whose
    // removed neighbor could plausibly have been its argmax -- typically far
    // fewer than all n variables per removal.
    std::vector<double> sumAbs(n, 0.0);
    std::vector<int> cnt(n, 0);
    std::vector<int> nanCount(n, 0);
    std::vector<double> maxVal(n, 0.0);
    std::vector<bool> maxDirty(n, true);

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (j == i) continue;
            double val = assocAt(A, i, j);
            if (std::isnan(val)) { nanCount[i]++; continue; }
            sumAbs[i] += std::fabs(val);
            cnt[i]++;
        }
    }

    auto avgOf = [&](int i) {
        return cnt[i] > 0 ? sumAbs[i] / cnt[i] : 0.0;
    };
    auto maxOf = [&](int i) {
        if (nanCount[i] > 0) return std::numeric_limits<double>::infinity();
        if (maxDirty[i]) {
            maxVal[i] = computeMaxAssoc(A, i, active);
            maxDirty[i] = false;
        }
        return maxVal[i];
    };

    // Iteratively remove worst variable until no violations
    bool changed = true;
    while (changed) {
        changed = false;

        // Find all violations (pairs where association > threshold)
        std::vector<std::pair<int, int>> violations;
        for (int i = 0; i < n; i++) {
            if (!active[i]) continue;
            for (int j = i + 1; j < n; j++) {
                if (!active[j]) continue;
                double val = A(i, j);
                if (std::isnan(val) || std::fabs(val) > threshold) {
                    violations.push_back({i, j});
                }
            }
        }

        // If no violations, we're done
        if (violations.empty()) {
            break;
        }

        // Compute badness scores (count of violations per variable)
        std::map<int, int> badness;
        for (const auto& violation : violations) {
            badness[violation.first]++;
            badness[violation.second]++;
        }

        // Find worst variable using tie-breaking rules
        int worst_idx = -1;
        int max_badness = -1;
        double tie_max_assoc = -1.0;
        double tie_avg_assoc = -1.0;

        for (const auto& entry : badness) {
            int var = entry.first;
            int bad_count = entry.second;

            // Skip protected variables
            if (protected_vars[var]) {
                continue;
            }

            // Primary criterion: highest badness count
            if (bad_count > max_badness) {
                worst_idx = var;
                max_badness = bad_count;
                tie_max_assoc = maxOf(var);
                tie_avg_assoc = avgOf(var);
            }
            // Tie-breaking
            else if (bad_count == max_badness) {
                double this_max = maxOf(var);
                double this_avg = avgOf(var);

                bool should_replace = false;

                // First tie-breaker: highest max association
                if (this_max > tie_max_assoc) {
                    should_replace = true;
                }
                // Second tie-breaker: highest average association
                else if (std::fabs(this_max - tie_max_assoc) < 1e-10 && this_avg > tie_avg_assoc) {
                    should_replace = true;
                }
                // Third tie-breaker (smallest column index wins): `badness`
                // is a std::map, which the standard guarantees iterates keys
                // in ascending order, so on a full tie `worst_idx` already
                // holds the smallest index and `should_replace` correctly
                // stays false here.

                if (should_replace) {
                    worst_idx = var;
                    tie_max_assoc = this_max;
                    tie_avg_assoc = this_avg;
                }
            }
        }

        // Check if we found a variable to remove
        if (worst_idx == -1) {
            // All violating variables are protected - cannot satisfy constraint
            stop("Cannot satisfy threshold: force_in variables violate the constraint");
        }

        // Remove the worst variable, updating every remaining active
        // neighbor's cached stats in O(1) amortized instead of recomputing
        // them from scratch on the next iteration.
        active[worst_idx] = false;
        for (int i = 0; i < n; i++) {
            if (!active[i] || i == worst_idx) continue;
            double val = assocAt(A, i, worst_idx);
            if (std::isnan(val)) {
                nanCount[i]--;
                if (nanCount[i] == 0) maxDirty[i] = true;  // may no longer be infinity
            } else {
                double av = std::fabs(val);
                sumAbs[i] -= av;
                cnt[i]--;
                if (nanCount[i] == 0 && av >= maxVal[i] - 1e-12) {
                    // removed neighbor may have been the argmax; recompute lazily
                    maxDirty[i] = true;
                }
            }
        }
        changed = true;
    }

    // Collect remaining active variables
    Combo result;
    for (int i = 0; i < n; i++) {
        if (active[i]) {
            result.push_back(i);
        }
    }

    return result;
}

// Rcpp export for use in R
// [[Rcpp::export]]
IntegerVector greedyPruneBackend(
    NumericMatrix assoc_matrix,
    double threshold,
    Nullable<IntegerVector> force_in = R_NilValue
) {
    // Validate input via the same shared checks used by the other three
    // Rcpp-exported backends (findAllMaxSets, runELS, runBronKerbosch).
    validateCorMatrix(assoc_matrix);
    int n = assoc_matrix.nrow();

    // Convert force_in to 0-based indices
    Combo forcedVec;
    if (force_in.isNotNull()) {
        IntegerVector f = force_in.get();
        for (int i = 0; i < f.size(); ++i) {
            // R passes 0-based indices (already converted in R layer)
            forcedVec.push_back(f[i]);
        }
    }
    validateForcedIndices(forcedVec, n);

    // Run greedy algorithm
    Combo result = greedyPrune(assoc_matrix, threshold, forcedVec);

    // Convert to 1-based indices for R
    IntegerVector out(result.begin(), result.end());
    out = out + 1;

    return out;
}
