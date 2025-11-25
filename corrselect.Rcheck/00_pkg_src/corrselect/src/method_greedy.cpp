#include "method_greedy.h"
#include <Rcpp.h>
#include <vector>
#include <map>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

// Helper: compute maximum association between var and any other active variable
double computeMaxAssoc(
    const NumericMatrix& A,
    int var,
    const std::vector<bool>& active
) {
    double max_val = 0.0;
    int n = A.nrow();
    for (int j = 0; j < n; j++) {
        if (j == var || !active[j]) continue;
        double val = std::fabs(A(var, j));
        if (val > max_val) {
            max_val = val;
        }
    }
    return max_val;
}

// Helper: compute average association between var and all other active variables
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
        sum += std::fabs(A(var, j));
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

    // Mark force_in variables as protected
    std::vector<bool> protected_vars(n, false);
    for (int idx : force_in) {
        if (idx < 0 || idx >= n) {
            stop("Invalid force_in index");
        }
        protected_vars[idx] = true;
    }

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
                if (std::fabs(A(i, j)) > threshold) {
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
                tie_max_assoc = computeMaxAssoc(A, var, active);
                tie_avg_assoc = computeAvgAssoc(A, var, active);
            }
            // Tie-breaking
            else if (bad_count == max_badness) {
                double this_max = computeMaxAssoc(A, var, active);
                double this_avg = computeAvgAssoc(A, var, active);

                bool should_replace = false;

                // First tie-breaker: highest max association
                if (this_max > tie_max_assoc) {
                    should_replace = true;
                }
                // Second tie-breaker: highest average association
                else if (std::fabs(this_max - tie_max_assoc) < 1e-10 && this_avg > tie_avg_assoc) {
                    should_replace = true;
                }
                // Third tie-breaker: lexicographically first (smallest index)
                else if (std::fabs(this_max - tie_max_assoc) < 1e-10 &&
                         std::fabs(this_avg - tie_avg_assoc) < 1e-10 &&
                         var < worst_idx) {
                    should_replace = true;
                }

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

        // Remove the worst variable
        active[worst_idx] = false;
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
    // Validate input
    int n = assoc_matrix.nrow();
    if (n != assoc_matrix.ncol()) {
        stop("Association matrix must be square");
    }

    // Convert force_in to 0-based indices
    Combo forcedVec;
    if (force_in.isNotNull()) {
        IntegerVector f = force_in.get();
        for (int i = 0; i < f.size(); ++i) {
            // R passes 0-based indices (already converted in R layer)
            int idx = f[i];
            if (idx < 0 || idx >= n) {
                stop("force_in indices out of bounds");
            }
            forcedVec.push_back(idx);
        }
    }

    // Run greedy algorithm
    Combo result = greedyPrune(assoc_matrix, threshold, forcedVec);

    // Convert to 1-based indices for R
    IntegerVector out(result.begin(), result.end());
    out = out + 1;

    return out;
}
