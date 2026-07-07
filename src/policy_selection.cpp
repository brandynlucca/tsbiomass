#include <Rcpp.h>
#include <R_ext/Random.h>
#include <algorithm>
#include <cmath>
#include <string>
#include <vector>

using namespace Rcpp;

namespace {

inline bool finite_value(double x) {
  return R_finite(x);
}

double quantile_type8(std::vector<double> values, double probability) {
  values.erase(
    std::remove_if(values.begin(), values.end(),
                   [](double x) { return !finite_value(x); }),
    values.end()
  );
  if (values.empty()) return NA_REAL;
  std::sort(values.begin(), values.end());
  int n = static_cast<int>(values.size());
  double h = (static_cast<double>(n) + 1.0 / 3.0) * probability + 1.0 / 3.0;
  if (h <= 1.0) return values.front();
  if (h >= static_cast<double>(n)) return values.back();
  int j = static_cast<int>(std::floor(h));
  double gamma = h - static_cast<double>(j);
  return (1.0 - gamma) * values[j - 1] + gamma * values[j];
}

double median_value(std::vector<double> values) {
  if (values.empty()) return NA_REAL;
  std::sort(values.begin(), values.end());
  std::size_t n = values.size();
  if (n % 2 == 1) return values[n / 2];
  return (values[n / 2 - 1] + values[n / 2]) / 2.0;
}

int ascii_code_sum(const std::string& value) {
  int out = 0;
  for (unsigned char ch : value) out += static_cast<int>(ch);
  return out;
}

}  // namespace

//' Compute every paired policy-equivalence bootstrap in one compiled call
//'
//' @param policy_matrix Species-by-policy error matrix ordered like the policy
//'   metadata vectors.
//' @param policy Policy names.
//' @param branch Equation-branch labels.
//' @param policy_key Stable policy-and-branch keys.
//' @param tolerance Practical equivalence tolerance.
//' @param n_boot Number of paired bootstrap resamples.
//' @param seed Base integer seed.
//'
//' @return Pairwise policy-equivalence data frame.
//'
//' @keywords internal
// [[Rcpp::export]]
SEXP cpp_policy_equivalence_pairs(NumericMatrix policy_matrix,
                                  CharacterVector policy,
                                  CharacterVector branch,
                                  CharacterVector policy_key,
                                  double tolerance,
                                  int n_boot,
                                  int seed) {
  int n_species = policy_matrix.nrow();
  int n_policy = policy_matrix.ncol();
  if (policy.size() != n_policy || branch.size() != n_policy ||
      policy_key.size() != n_policy) {
    stop("Policy-equivalence metadata must match the policy-matrix columns.");
  }
  if (n_boot < 1) stop("'n_boot' must be at least 1.");
  if (!finite_value(tolerance) || tolerance < 0.0) {
    stop("'tolerance' must be finite and non-negative.");
  }

  double n_pairs_double = static_cast<double>(n_policy) *
    static_cast<double>(n_policy - 1) / 2.0;
  if (n_pairs_double > static_cast<double>(R_XLEN_T_MAX)) {
    stop("Too many policy pairs for one result table.");
  }
  std::size_t n_pairs = static_cast<std::size_t>(n_pairs_double);

  std::vector<std::string> policy_a, branch_a, policy_b, branch_b;
  std::vector<int> n_common;
  std::vector<double> mean_diff, median_diff, q025, q975;
  std::vector<int> equivalent;
  std::vector<std::string> decision;
  std::vector<std::string> better;
  std::vector<int> better_missing;
  policy_a.reserve(n_pairs);
  branch_a.reserve(n_pairs);
  policy_b.reserve(n_pairs);
  branch_b.reserve(n_pairs);
  n_common.reserve(n_pairs);
  mean_diff.reserve(n_pairs);
  median_diff.reserve(n_pairs);
  q025.reserve(n_pairs);
  q975.reserve(n_pairs);
  equivalent.reserve(n_pairs);
  decision.reserve(n_pairs);
  better.reserve(n_pairs);
  better_missing.reserve(n_pairs);

  Function set_seed = Environment::base_env()["set.seed"];

  // This order matches utils::combn(policy_keys, 2): exhaust every right-hand
  // partner for the current left-hand policy before advancing the left side.
  for (int lhs = 0; lhs < n_policy - 1; ++lhs) {
    for (int rhs = lhs + 1; rhs < n_policy; ++rhs) {
      std::string lhs_name = as<std::string>(policy[lhs]);
      std::string lhs_branch = as<std::string>(branch[lhs]);
      std::string rhs_name = as<std::string>(policy[rhs]);
      std::string rhs_branch = as<std::string>(branch[rhs]);
      std::string lhs_key = as<std::string>(policy_key[lhs]);
      std::string rhs_key = as<std::string>(policy_key[rhs]);

      std::vector<double> differences;
      differences.reserve(n_species);
      for (int row = 0; row < n_species; ++row) {
        double lhs_value = policy_matrix(row, lhs);
        double rhs_value = policy_matrix(row, rhs);
        if (finite_value(lhs_value) && finite_value(rhs_value)) {
          differences.push_back(lhs_value - rhs_value);
        }
      }

      policy_a.push_back(lhs_name);
      branch_a.push_back(lhs_branch);
      policy_b.push_back(rhs_name);
      branch_b.push_back(rhs_branch);
      n_common.push_back(static_cast<int>(differences.size()));

      if (differences.empty()) {
        mean_diff.push_back(NA_REAL);
        median_diff.push_back(NA_REAL);
        q025.push_back(NA_REAL);
        q975.push_back(NA_REAL);
        equivalent.push_back(FALSE);
        decision.push_back("inconclusive");
        better.push_back("");
        better_missing.push_back(TRUE);
        continue;
      }

      int pair_seed = seed + ascii_code_sum(lhs_key + "|" + rhs_key);
      set_seed(pair_seed);
      int n_difference = static_cast<int>(differences.size());
      std::vector<double> bootstrap_means(n_boot);
      for (int bootstrap = 0; bootstrap < n_boot; ++bootstrap) {
        double sum = 0.0;
        for (int draw = 0; draw < n_difference; ++draw) {
          int index = static_cast<int>(R_unif_index(n_difference));
          sum += differences[index];
        }
        bootstrap_means[bootstrap] = sum / static_cast<double>(n_difference);
      }

      double sum_difference = 0.0;
      for (double value : differences) {
        sum_difference += value;
      }
      double mean = sum_difference / static_cast<double>(differences.size());
      double median = median_value(differences);
      double lower = quantile_type8(bootstrap_means, 0.025);
      double upper = quantile_type8(bootstrap_means, 0.975);
      bool is_equivalent = finite_value(lower) && finite_value(upper) &&
        lower >= -tolerance && upper <= tolerance;
      bool lhs_better = finite_value(upper) && upper < -tolerance;
      bool rhs_better = finite_value(lower) && lower > tolerance;

      mean_diff.push_back(mean);
      median_diff.push_back(median);
      q025.push_back(lower);
      q975.push_back(upper);
      equivalent.push_back(is_equivalent ? TRUE : FALSE);
      if (is_equivalent) {
        decision.push_back("equivalent");
        better.push_back("");
        better_missing.push_back(TRUE);
      } else if (lhs_better) {
        decision.push_back("lhs_better");
        better.push_back(lhs_name);
        better_missing.push_back(FALSE);
      } else if (rhs_better) {
        decision.push_back("rhs_better");
        better.push_back(rhs_name);
        better_missing.push_back(FALSE);
      } else {
        decision.push_back("inconclusive");
        better.push_back("");
        better_missing.push_back(TRUE);
      }
    }
  }

  CharacterVector better_output(better.size());
  LogicalVector equivalent_output(equivalent.size());
  for (std::size_t i = 0; i < better.size(); ++i) {
    better_output[i] = better_missing[i] ? NA_STRING : String(better[i]);
    equivalent_output[i] = equivalent[i] ? TRUE : FALSE;
  }

  return DataFrame::create(
    _["policy_a"] = policy_a,
    _["equation_branch_filter_a"] = branch_a,
    _["policy_b"] = policy_b,
    _["equation_branch_filter_b"] = branch_b,
    _["n_species_common"] = n_common,
    _["paired_mean_diff"] = mean_diff,
    _["paired_median_diff"] = median_diff,
    _["paired_boot_q025"] = q025,
    _["paired_boot_q975"] = q975,
    _["equivalent_pair"] = equivalent_output,
    _["pair_decision"] = decision,
    _["better_policy"] = better_output
  );
}
