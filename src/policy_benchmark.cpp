#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <set>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace Rcpp;

namespace {

inline bool finite_num(double x) {
  return R_finite(x);
}

double finite_min(const NumericVector& x, const std::vector<int>& idx) {
  double out = R_PosInf;
  bool found = false;
  for (int i : idx) {
    double value = x[i];
    if (finite_num(value) && value < out) {
      out = value;
      found = true;
    }
  }
  return found ? out : NA_REAL;
}

double finite_median(const NumericVector& x, const std::vector<int>& idx) {
  std::vector<double> values;
  values.reserve(idx.size());
  for (int i : idx) {
    if (finite_num(x[i])) values.push_back(x[i]);
  }
  if (values.empty()) return NA_REAL;
  std::sort(values.begin(), values.end());
  std::size_t n = values.size();
  if (n % 2 == 1) return values[n / 2];
  return (values[n / 2 - 1] + values[n / 2]) / 2.0;
}

double weighted_mean(const NumericVector& x,
                     const NumericVector& raw_weight,
                     const std::vector<int>& idx) {
  double numerator = 0.0;
  double denominator = 0.0;
  for (int i : idx) {
    double value = x[i];
    double weight = raw_weight[i];
    if (finite_num(value) && finite_num(weight) && weight > 0.0) {
      numerator += value * weight;
      denominator += weight;
    }
  }
  return denominator > 0.0 ? numerator / denominator : NA_REAL;
}

std::vector<double> normalized_weights(const NumericVector& raw_weight,
                                       const std::vector<int>& idx,
                                       bool equal_weight) {
  std::vector<double> out(idx.size(), 0.0);
  if (idx.empty()) return out;
  if (equal_weight) {
    std::fill(out.begin(), out.end(), 1.0 / static_cast<double>(idx.size()));
    return out;
  }
  double total = 0.0;
  for (std::size_t j = 0; j < idx.size(); ++j) {
    double value = raw_weight[idx[j]];
    if (finite_num(value) && value > 0.0) {
      out[j] = value;
      total += value;
    }
  }
  if (total <= 0.0) return std::vector<double>();
  for (double& value : out) value /= total;
  return out;
}

double weighted_quantile_step(const std::vector<double>& values,
                              const std::vector<double>& weights,
                              double probability) {
  std::vector<std::pair<double, double> > pairs;
  pairs.reserve(values.size());
  double total = 0.0;
  for (std::size_t i = 0; i < values.size() && i < weights.size(); ++i) {
    if (finite_num(values[i]) && finite_num(weights[i]) && weights[i] >= 0.0) {
      pairs.push_back(std::make_pair(values[i], weights[i]));
      total += weights[i];
    }
  }
  if (pairs.empty() || total <= 0.0) return NA_REAL;
  std::stable_sort(pairs.begin(), pairs.end(),
                   [](const std::pair<double, double>& a,
                      const std::pair<double, double>& b) {
                     return a.first < b.first;
                   });
  double cumulative = 0.0;
  for (const auto& pair : pairs) {
    cumulative += pair.second / total;
    if (cumulative >= probability) return pair.first;
  }
  return pairs.back().first;
}

double quantile_type8(std::vector<double> values, double probability) {
  values.erase(
    std::remove_if(values.begin(), values.end(),
                   [](double x) { return !finite_num(x); }),
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

double finite_sd(const NumericVector& x, const std::vector<int>& idx) {
  std::vector<double> values;
  values.reserve(idx.size());
  for (int i : idx) if (finite_num(x[i])) values.push_back(x[i]);
  if (values.size() <= 1) return 0.0;
  double mean = 0.0;
  for (double value : values) mean += value;
  mean /= static_cast<double>(values.size());
  double ss = 0.0;
  for (double value : values) {
    double delta = value - mean;
    ss += delta * delta;
  }
  return std::sqrt(ss / static_cast<double>(values.size() - 1));
}

double finite_iqr(const NumericVector& x, const std::vector<int>& idx) {
  std::vector<double> values;
  values.reserve(idx.size());
  for (int i : idx) if (finite_num(x[i])) values.push_back(x[i]);
  if (values.size() <= 1) return 0.0;
  return quantile_type8(values, 0.75) - quantile_type8(values, 0.25);
}

int nearest_lexicographic(const std::vector<int>& idx,
                          const NumericVector& primary,
                          const NumericVector& secondary,
                          bool require_primary) {
  int best = -1;
  double best_primary = R_PosInf;
  double best_secondary = R_PosInf;
  for (int i : idx) {
    double p = primary[i];
    double s = secondary[i];
    if (require_primary && !finite_num(p)) continue;
    if (!finite_num(p)) p = R_PosInf;
    if (!finite_num(s)) s = R_PosInf;
    if (best < 0 || p < best_primary || (p == best_primary && s < best_secondary)) {
      best = i;
      best_primary = p;
      best_secondary = s;
    }
  }
  return best;
}

int nearest_combined_summary(const std::vector<int>& idx,
                             const NumericVector& combined,
                             const NumericVector& taxonomic) {
  int best = -1;
  double best_combined = R_PosInf;
  double best_tax = R_PosInf;
  for (int i : idx) {
    double c = combined[i];
    if (!finite_num(c)) c = R_PosInf;
    double t = finite_num(taxonomic[i]) ? taxonomic[i] : R_PosInf;
    if (best < 0 || c < best_combined || (c == best_combined && t < best_tax)) {
      best = i;
      best_combined = c;
      best_tax = t;
    }
  }
  return best;
}

int nearest_single(const std::vector<int>& idx,
                   const NumericVector& distance) {
  int best = -1;
  double best_distance = R_PosInf;
  for (int i : idx) {
    double value = finite_num(distance[i]) ? distance[i] : R_PosInf;
    if (best < 0 || value < best_distance) {
      best = i;
      best_distance = value;
    }
  }
  return best;
}

double sigma_mean(double slope,
                  double intercept,
                  const NumericVector& log_length,
                  const NumericVector& pdf_weight) {
  if (!finite_num(slope) || !finite_num(intercept) || log_length.size() == 0) {
    return NA_REAL;
  }
  double out = 0.0;
  for (R_xlen_t i = 0; i < log_length.size(); ++i) {
    out += std::pow(10.0, (slope * log_length[i] + intercept) / 10.0) * pdf_weight[i];
  }
  return out;
}

std::string donor_fingerprint(const CharacterVector& donor_id,
                              const std::vector<int>& idx) {
  std::set<std::string> ids;
  for (int i : idx) {
    if (CharacterVector::is_na(donor_id[i])) continue;
    std::string value = as<std::string>(donor_id[i]);
    if (!value.empty()) ids.insert(value);
  }
  std::string out;
  for (const std::string& value : ids) {
    if (!out.empty()) out += "|";
    out += value;
  }
  return out;
}

} // namespace

//' Evaluate a complete policy plan for one anchor
//'
//' @param donors Flat donor-vector payload.
//' @param pool_masks Logical matrix: unique pools by donor rows.
//' @param plan Flat compiled policy plan.
//' @param length_cm Anchor PDF length grid.
//' @param pdf_weight Anchor PDF weights.
//' @param anchor_sigma Anchor truth mean backscatter.
//'
//' @return A data frame of core policy predictions and diagnostics.
//'
//' @keywords internal
// [[Rcpp::export]]
SEXP cpp_evaluate_policy_plan(List donors,
                              LogicalMatrix pool_masks,
                              List plan,
                              NumericVector length_cm,
                              NumericVector pdf_weight,
                              double anchor_sigma) {
  NumericVector slope = donors["slope"];
  NumericVector intercept = donors["intercept"];
  NumericVector weight = donors["weight"];
  NumericVector combined = donors["combined_distance"];
  NumericVector trait = donors["trait_distance"];
  NumericVector taxonomic = donors["taxonomic_distance"];
  NumericVector species = donors["species_distance"];
  NumericVector learned_disagreement = donors["learned_distance_disagreement"];
  LogicalVector learned_diagnostic_available = donors["learned_distance_diagnostic_available"];
  bool has_trait = as<bool>(donors["has_trait_distance"]);
  bool has_taxonomic = as<bool>(donors["has_taxonomic_distance"]);
  bool has_species = as<bool>(donors["has_species_distance"]);
  bool has_learned_diagnostic = as<bool>(donors["has_learned_distance_diagnostic"]);
  NumericVector length_overlap = donors["length_overlap"];
  NumericVector depth_overlap = donors["depth_overlap"];
  NumericVector donor_multiplier = donors["donor_multiplier"];
  CharacterVector donor_id = donors["donor_id"];
  LogicalMatrix overlap = donors["overlap"];

  IntegerVector pool_id = plan["pool_id"];
  IntegerVector aggregation_code = plan["aggregation_code"];
  int n_plan = pool_id.size();
  int n_donor = slope.size();
  if (pool_masks.ncol() != n_donor || aggregation_code.size() != n_plan) {
    stop("Compiled policy-plan dimensions do not match donor payload.");
  }

  // Normalize the PDF once and precompute log-length moments used by every
  // policy and structural donor summary.
  std::vector<double> log_values;
  std::vector<double> pdf_values;
  double pdf_total = 0.0;
  for (R_xlen_t i = 0; i < length_cm.size() && i < pdf_weight.size(); ++i) {
    if (finite_num(length_cm[i]) && length_cm[i] > 0.0 &&
        finite_num(pdf_weight[i]) && pdf_weight[i] >= 0.0) {
      log_values.push_back(std::log10(length_cm[i]));
      pdf_values.push_back(pdf_weight[i]);
      pdf_total += pdf_weight[i];
    }
  }
  NumericVector log_length(log_values.size());
  NumericVector normalized_pdf(pdf_values.size());
  double log_mean = NA_REAL;
  double log_second = NA_REAL;
  if (pdf_total > 0.0) {
    log_mean = 0.0;
    log_second = 0.0;
    for (std::size_t i = 0; i < log_values.size(); ++i) {
      log_length[i] = log_values[i];
      normalized_pdf[i] = pdf_values[i] / pdf_total;
      log_mean += normalized_pdf[i] * log_length[i];
      log_second += normalized_pdf[i] * log_length[i] * log_length[i];
    }
  } else {
    log_length = NumericVector(0);
    normalized_pdf = NumericVector(0);
  }

  NumericVector donor_sigma(n_donor, NA_REAL);
  for (int i = 0; i < n_donor; ++i) {
    donor_sigma[i] = sigma_mean(slope[i], intercept[i], log_length, normalized_pdf);
  }

  IntegerVector n_models_pool(n_plan);
  NumericVector policy_slope(n_plan, NA_REAL);
  NumericVector policy_intercept(n_plan, NA_REAL);
  NumericVector policy_sigma(n_plan, NA_REAL);
  NumericVector multiplier(n_plan, NA_REAL);
  IntegerVector n_valid_models(n_plan);
  NumericVector min_combined(n_plan, NA_REAL), median_combined(n_plan, NA_REAL), weighted_combined(n_plan, NA_REAL);
  NumericVector min_trait(n_plan, NA_REAL), weighted_trait(n_plan, NA_REAL);
  NumericVector min_species(n_plan, NA_REAL), weighted_species(n_plan, NA_REAL);
  NumericVector weighted_learned_disagreement(n_plan, NA_REAL), max_learned_disagreement(n_plan, NA_REAL);
  LogicalVector learned_diagnostic_available_out(n_plan, false);
  NumericVector mean_length_overlap(n_plan, NA_REAL), mean_depth_overlap(n_plan, NA_REAL);
  NumericVector effective_support(n_plan, NA_REAL), max_weight(n_plan, NA_REAL);
  IntegerVector unique_donors(n_plan), n_slope20(n_plan), n_non_slope20(n_plan);
  CharacterVector donor_fingerprints(n_plan);
  LogicalVector constructed(n_plan);
  NumericVector slope_sd(n_plan, NA_REAL), intercept_sd(n_plan, NA_REAL);
  NumericVector slope_iqr(n_plan, NA_REAL), intercept_iqr(n_plan, NA_REAL);
  NumericVector multiplier_dev_median(n_plan, NA_REAL), multiplier_dev_q90(n_plan, NA_REAL);
  NumericVector sigma_dev_median(n_plan, NA_REAL), sigma_dev_q90(n_plan, NA_REAL);
  NumericVector curve_rmse_median(n_plan, NA_REAL), curve_rmse_q90(n_plan, NA_REAL);
  NumericVector structural_q(n_plan, NA_REAL);
  List overlap_counts(overlap.ncol());
  CharacterVector overlap_names = colnames(overlap);
  for (int j = 0; j < overlap.ncol(); ++j) overlap_counts[j] = IntegerVector(n_plan);

  for (int p = 0; p < n_plan; ++p) {
    int pool = pool_id[p] - 1;
    if (pool < 0 || pool >= pool_masks.nrow()) stop("Compiled plan referenced an invalid pool ID.");
    int method = aggregation_code[p];
    std::vector<int> valid;
    valid.reserve(n_donor);
    for (int i = 0; i < n_donor; ++i) {
      if (pool_masks(pool, i) == TRUE && finite_num(slope[i]) && finite_num(intercept[i])) {
        valid.push_back(i);
      }
    }
    n_models_pool[p] = valid.size();

    int equation_index = -1;
    int summary_index = -1;
    if (method == 1) {
      summary_index = nearest_combined_summary(valid, combined, taxonomic);
      // When the taxonomic column exists, R uses it as a tiny tiebreak and
      // arithmetic NA propagation requires both fields to be finite. When it
      // is absent, R ranks on combined distance alone.
      double best_score = R_PosInf;
      for (int i : valid) {
        if (!finite_num(combined[i]) || (has_taxonomic && !finite_num(taxonomic[i]))) continue;
        double score = combined[i] + (has_taxonomic ? taxonomic[i] * 1e-9 : 0.0);
        if (score < best_score) { best_score = score; equation_index = i; }
      }
    } else if (method >= 2 && method <= 4) {
      const NumericVector* primary = method == 2 ? &trait : (method == 3 ? &taxonomic : &species);
      bool has_primary = method == 2 ? has_trait : (method == 3 ? has_taxonomic : has_species);
      bool any_primary = false;
      if (has_primary) {
        for (int i : valid) if (finite_num((*primary)[i])) { any_primary = true; break; }
      }
      summary_index = any_primary ? nearest_lexicographic(valid, *primary, combined, true)
                                  : nearest_single(valid, combined);
      double best_score = R_PosInf;
      for (int i : valid) {
        if (!finite_num(combined[i]) || (has_primary && !finite_num((*primary)[i]))) continue;
        double score = has_primary ? (*primary)[i] + combined[i] * 1e-9 : combined[i];
        if (score < best_score) { best_score = score; equation_index = i; }
      }
    }

    if (method >= 1 && method <= 4) {
      if (equation_index >= 0) {
        policy_slope[p] = slope[equation_index];
        policy_intercept[p] = intercept[equation_index];
      }
      constructed[p] = false;
    } else if (method == 5) {
      double sw = 0.0, ss = 0.0, si = 0.0;
      for (int i : valid) {
        if (finite_num(weight[i]) && weight[i] > 0.0) {
          sw += weight[i];
          ss += weight[i] * slope[i];
          si += weight[i] * intercept[i];
        }
      }
      if (sw > 0.0) {
        policy_slope[p] = ss / sw;
        policy_intercept[p] = si / sw;
      }
      constructed[p] = true;
    } else if (method == 6) {
      if (!valid.empty()) {
        double ss = 0.0, si = 0.0;
        for (int i : valid) { ss += slope[i]; si += intercept[i]; }
        policy_slope[p] = ss / valid.size();
        policy_intercept[p] = si / valid.size();
      }
      constructed[p] = true;
    }

    policy_sigma[p] = sigma_mean(policy_slope[p], policy_intercept[p], log_length, normalized_pdf);
    if (finite_num(anchor_sigma) && anchor_sigma > 0.0 &&
        finite_num(policy_sigma[p]) && policy_sigma[p] > 0.0) {
      multiplier[p] = anchor_sigma / policy_sigma[p];
    }

    std::vector<int> summary_idx;
    if (method >= 1 && method <= 4) {
      if (summary_index >= 0) summary_idx.push_back(summary_index);
    } else {
      summary_idx = valid;
    }
    n_valid_models[p] = summary_idx.size();
    min_combined[p] = finite_min(combined, summary_idx);
    median_combined[p] = finite_median(combined, summary_idx);
    weighted_combined[p] = weighted_mean(combined, weight, summary_idx);
    min_trait[p] = finite_min(trait, summary_idx);
    weighted_trait[p] = weighted_mean(trait, weight, summary_idx);
    min_species[p] = finite_min(species, summary_idx);
    weighted_species[p] = weighted_mean(species, weight, summary_idx);
    weighted_learned_disagreement[p] = weighted_mean(learned_disagreement, weight, summary_idx);
    double max_disagreement = R_NegInf;
    bool has_disagreement = false;
    for (int i : summary_idx) {
      if (finite_num(learned_disagreement[i])) {
        max_disagreement = std::max(max_disagreement, learned_disagreement[i]);
        has_disagreement = true;
      }
    }
    max_learned_disagreement[p] = has_disagreement ? max_disagreement : NA_REAL;
    if (has_learned_diagnostic) {
      bool all_available = true;
      for (int i : summary_idx) {
        if (learned_diagnostic_available[i] != TRUE) {
          all_available = false;
          break;
        }
      }
      learned_diagnostic_available_out[p] = all_available;
    }
    mean_length_overlap[p] = weighted_mean(length_overlap, weight, summary_idx);
    mean_depth_overlap[p] = weighted_mean(depth_overlap, weight, summary_idx);

    std::vector<double> support_weights = normalized_weights(weight, summary_idx, false);
    if (!support_weights.empty()) {
      double sum_sq = 0.0;
      double max_now = 0.0;
      for (double value : support_weights) { sum_sq += value * value; max_now = std::max(max_now, value); }
      effective_support[p] = sum_sq > 0.0 ? 1.0 / sum_sq : NA_REAL;
      max_weight[p] = max_now;
    }
    std::set<std::string> unique_id_set;
    bool has_missing_id = false;
    for (int i : summary_idx) {
      if (CharacterVector::is_na(donor_id[i])) has_missing_id = true;
      else unique_id_set.insert(as<std::string>(donor_id[i]));
      if (finite_num(slope[i]) && std::abs(slope[i] - 20.0) <= 1e-8) n_slope20[p]++;
      else n_non_slope20[p]++;
    }
    // R's length(unique(as.character(x))) counts one missing value.
    unique_donors[p] = unique_id_set.size() + (has_missing_id ? 1 : 0);
    std::string fingerprint = donor_fingerprint(donor_id, summary_idx);
    if (fingerprint.empty()) donor_fingerprints[p] = NA_STRING;
    else donor_fingerprints[p] = fingerprint;
    for (int j = 0; j < overlap.ncol(); ++j) {
      IntegerVector counts = overlap_counts[j];
      int count = 0;
      for (int i : summary_idx) if (overlap(i, j) == TRUE) count++;
      counts[p] = count;
    }

    // Structural rows are the selected nearest donor or the complete valid
    // pool. Their weighting follows policy_structural_rows().
    std::vector<int> structural_idx;
    if (method >= 1 && method <= 4) {
      if (summary_index >= 0) structural_idx.push_back(summary_index);
    } else {
      structural_idx = valid;
    }
    if (!structural_idx.empty() && finite_num(policy_slope[p]) && finite_num(policy_intercept[p])) {
      // The non-empty R structural-summary path currently flags taxonomic-
      // and species-distance nearest policies as constructed ensembles.
      if (method == 3 || method == 4) constructed[p] = true;
      std::vector<double> structural_weights = normalized_weights(
        weight,
        structural_idx,
        method == 6 || (method >= 1 && method <= 4)
      );
      slope_sd[p] = finite_sd(slope, structural_idx);
      intercept_sd[p] = finite_sd(intercept, structural_idx);
      slope_iqr[p] = finite_iqr(slope, structural_idx);
      intercept_iqr[p] = finite_iqr(intercept, structural_idx);

      std::vector<double> multiplier_dev, sigma_dev, curve_dev;
      multiplier_dev.reserve(structural_idx.size());
      sigma_dev.reserve(structural_idx.size());
      curve_dev.reserve(structural_idx.size());
      for (int i : structural_idx) {
        double ds = donor_sigma[i];
        // Structural uncertainty is donor disagreement around the policy
        // equation on the anchor's length distribution, not error against the
        // anchor truth. This matches the kappa definition used by the R path.
        double donor_policy_log_deviation =
          finite_num(ds) && ds > 0.0 && finite_num(policy_sigma[p]) && policy_sigma[p] > 0.0
            ? std::abs(std::log(ds) - std::log(policy_sigma[p])) : NA_REAL;
        multiplier_dev.push_back(donor_policy_log_deviation);
        sigma_dev.push_back(donor_policy_log_deviation);
        if (finite_num(log_mean) && finite_num(log_second)) {
          double delta_s = slope[i] - policy_slope[p];
          double delta_b = intercept[i] - policy_intercept[p];
          double mse = delta_s * delta_s * log_second +
            2.0 * delta_s * delta_b * log_mean + delta_b * delta_b;
          curve_dev.push_back(std::sqrt(std::max(0.0, mse)));
        } else {
          curve_dev.push_back(NA_REAL);
        }
      }
      multiplier_dev_median[p] = weighted_quantile_step(multiplier_dev, structural_weights, 0.50);
      multiplier_dev_q90[p] = weighted_quantile_step(multiplier_dev, structural_weights, 0.90);
      sigma_dev_median[p] = weighted_quantile_step(sigma_dev, structural_weights, 0.50);
      sigma_dev_q90[p] = weighted_quantile_step(sigma_dev, structural_weights, 0.90);
      curve_rmse_median[p] = weighted_quantile_step(curve_dev, structural_weights, 0.50);
      curve_rmse_q90[p] = weighted_quantile_step(curve_dev, structural_weights, 0.90);
      if (finite_num(multiplier_dev_q90[p])) structural_q[p] = multiplier_dev_q90[p];
      else if (finite_num(sigma_dev_q90[p])) structural_q[p] = sigma_dev_q90[p];
      else if (finite_num(curve_rmse_q90[p])) structural_q[p] = curve_rmse_q90[p] * std::log(10.0) / 10.0;
    }
  }

  List out = List::create(
    _["n_models_pool"] = n_models_pool,
    _["policy_slope_len"] = policy_slope,
    _["policy_intercept_len"] = policy_intercept,
    _["policy_sigma_bs_mean"] = policy_sigma,
    _["multiplier_pred"] = multiplier,
    _["n_valid_models"] = n_valid_models,
    _["local_min_combined_distance"] = min_combined,
    _["local_median_combined_distance"] = median_combined,
    _["local_weighted_mean_combined_distance"] = weighted_combined,
    _["local_weighted_mean_learned_distance_disagreement"] = weighted_learned_disagreement,
    _["local_max_learned_distance_disagreement"] = max_learned_disagreement,
    _["learned_distance_diagnostic_available"] = learned_diagnostic_available_out,
    _["local_min_trait_gower_distance"] = min_trait,
    _["local_weighted_mean_trait_gower_distance"] = weighted_trait,
    _["local_min_species_distance"] = min_species,
    _["local_weighted_mean_species_distance"] = weighted_species,
    _["local_mean_length_overlap"] = mean_length_overlap,
    _["local_mean_depth_overlap"] = mean_depth_overlap,
    _["local_effective_support"] = effective_support,
    _["local_max_weight"] = max_weight,
    _["realized_n_unique_donors"] = unique_donors,
    _["realized_donor_fingerprint"] = donor_fingerprints,
    _["local_n_slope20"] = n_slope20,
    _["local_n_non_slope20"] = n_non_slope20,
    _["policy_is_constructed_ensemble"] = constructed,
    _["donor_slope_sd"] = slope_sd,
    _["donor_intercept_sd"] = intercept_sd,
    _["donor_slope_iqr"] = slope_iqr,
    _["donor_intercept_iqr"] = intercept_iqr,
    _["donor_log_multiplier_abs_dev_median"] = multiplier_dev_median,
    _["donor_log_multiplier_abs_dev_q90"] = multiplier_dev_q90,
    _["donor_log_sigma_abs_dev_median"] = sigma_dev_median,
    _["donor_log_sigma_abs_dev_q90"] = sigma_dev_q90,
    _["donor_curve_rmse_median"] = curve_rmse_median,
    _["donor_curve_rmse_q90"] = curve_rmse_q90,
    _["local_structural_q_abs_log"] = structural_q
  );
  for (int j = 0; j < overlap.ncol(); ++j) {
    std::string name = overlap_names.size() > j && !CharacterVector::is_na(overlap_names[j])
      ? as<std::string>(overlap_names[j])
      : std::string("local_n_overlap_") + std::to_string(j + 1);
    out[name] = overlap_counts[j];
  }
  out.attr("class") = "data.frame";
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -n_plan);
  return out;
}

//' Build TS-error rows for a complete pseudo-anchor benchmark
//'
//' @param anchors Flat anchor vectors containing IDs, species, standardized
//'   coefficients, and study-length bounds.
//' @param policies Flat policy-performance vectors containing anchor IDs,
//'   policy metadata, standardized coefficients, and local diagnostics.
//' @param grid_size Number of equally spaced relative-length points.
//'
//' @return A data frame with one row per valid policy and relative-length point.
//'
//' @keywords internal
// [[Rcpp::export]]
SEXP cpp_build_benchmark_ts_errors(List anchors,
                                   List policies,
                                   int grid_size = 11) {
  if (grid_size < 2) stop("'grid_size' must be at least 2.");

  CharacterVector anchor_id = anchors["anchor_model_id"];
  CharacterVector anchor_species = anchors["anchor_species"];
  NumericVector anchor_slope = anchors["anchor_slope"];
  NumericVector anchor_intercept = anchors["anchor_intercept"];
  NumericVector anchor_lmin = anchors["study_length_min"];
  NumericVector anchor_lmax = anchors["study_length_max"];

  CharacterVector policy_anchor_id = policies["anchor_model_id"];
  CharacterVector policy_name = policies["policy"];
  CharacterVector branch_name = policies["equation_branch_filter"];
  NumericVector policy_slope = policies["policy_slope_len"];
  NumericVector policy_intercept = policies["policy_intercept_len"];
  NumericVector local_min_distance = policies["local_min_combined_distance"];
  NumericVector local_weighted_distance = policies["local_weighted_mean_combined_distance"];
  NumericVector local_effective_support = policies["local_effective_support"];
  NumericVector local_length_overlap = policies["local_mean_length_overlap"];
  NumericVector local_depth_overlap = policies["local_mean_depth_overlap"];

  R_xlen_t n_anchor = anchor_id.size();
  R_xlen_t n_policy = policy_anchor_id.size();
  if (anchor_species.size() != n_anchor || anchor_slope.size() != n_anchor ||
      anchor_intercept.size() != n_anchor || anchor_lmin.size() != n_anchor ||
      anchor_lmax.size() != n_anchor) {
    stop("Anchor TS-error payload columns have inconsistent lengths.");
  }
  if (policy_name.size() != n_policy || branch_name.size() != n_policy ||
      policy_slope.size() != n_policy || policy_intercept.size() != n_policy ||
      local_min_distance.size() != n_policy || local_weighted_distance.size() != n_policy ||
      local_effective_support.size() != n_policy || local_length_overlap.size() != n_policy ||
      local_depth_overlap.size() != n_policy) {
    stop("Policy TS-error payload columns have inconsistent lengths.");
  }

  // Match R's first-row anchor lookup when model IDs are duplicated.
  std::unordered_map<std::string, R_xlen_t> anchor_lookup;
  anchor_lookup.reserve(static_cast<std::size_t>(n_anchor));
  for (R_xlen_t i = 0; i < n_anchor; ++i) {
    if (CharacterVector::is_na(anchor_id[i])) continue;
    std::string id = as<std::string>(anchor_id[i]);
    if (!id.empty() && anchor_lookup.find(id) == anchor_lookup.end()) {
      anchor_lookup[id] = i;
    }
  }

  std::size_t reserve_n = static_cast<std::size_t>(n_policy) *
    static_cast<std::size_t>(grid_size);
  std::vector<std::string> out_anchor_id, out_anchor_species, out_policy, out_branch;
  std::vector<double> out_policy_slope, out_policy_intercept;
  std::vector<double> out_min_distance, out_weighted_distance, out_effective_support;
  std::vector<double> out_length_overlap, out_depth_overlap;
  std::vector<double> out_u, out_length, out_ts_obs, out_ts_pred, out_ts_error;
  std::vector<double> out_abs_ts_error, out_sigma_obs, out_sigma_pred, out_log_sigma_residual;
  out_anchor_id.reserve(reserve_n);
  out_anchor_species.reserve(reserve_n);
  out_policy.reserve(reserve_n);
  out_branch.reserve(reserve_n);
  out_policy_slope.reserve(reserve_n);
  out_policy_intercept.reserve(reserve_n);
  out_min_distance.reserve(reserve_n);
  out_weighted_distance.reserve(reserve_n);
  out_effective_support.reserve(reserve_n);
  out_length_overlap.reserve(reserve_n);
  out_depth_overlap.reserve(reserve_n);
  out_u.reserve(reserve_n);
  out_length.reserve(reserve_n);
  out_ts_obs.reserve(reserve_n);
  out_ts_pred.reserve(reserve_n);
  out_ts_error.reserve(reserve_n);
  out_abs_ts_error.reserve(reserve_n);
  out_sigma_obs.reserve(reserve_n);
  out_sigma_pred.reserve(reserve_n);
  out_log_sigma_residual.reserve(reserve_n);

  // build_ts_errors() keeps the first row for each anchor-policy-branch tuple.
  std::set<std::string> seen;
  for (R_xlen_t i = 0; i < n_policy; ++i) {
    if (CharacterVector::is_na(policy_anchor_id[i]) ||
        CharacterVector::is_na(policy_name[i]) ||
        CharacterVector::is_na(branch_name[i])) {
      continue;
    }
    std::string id = as<std::string>(policy_anchor_id[i]);
    std::string policy = as<std::string>(policy_name[i]);
    std::string branch = as<std::string>(branch_name[i]);
    std::string distinct_key = id + "\x1f" + policy + "\x1f" + branch;
    if (!seen.insert(distinct_key).second) continue;

    auto anchor_it = anchor_lookup.find(id);
    if (anchor_it == anchor_lookup.end()) continue;
    R_xlen_t ai = anchor_it->second;
    double observed_slope = anchor_slope[ai];
    double observed_intercept = anchor_intercept[ai];
    double lmin = anchor_lmin[ai];
    double lmax = anchor_lmax[ai];
    double predicted_slope = policy_slope[i];
    double predicted_intercept = policy_intercept[i];
    if (!finite_num(observed_slope) || !finite_num(observed_intercept) ||
        !finite_num(lmin) || !finite_num(lmax) || lmax <= lmin ||
        !finite_num(predicted_slope) || !finite_num(predicted_intercept)) {
      continue;
    }

    std::vector<double> lengths(grid_size), observed(grid_size), predicted(grid_size);
    bool valid = true;
    for (int j = 0; j < grid_size; ++j) {
      double u = static_cast<double>(j) / static_cast<double>(grid_size - 1);
      double length = lmin + u * (lmax - lmin);
      double log_length = std::log10(length);
      double obs = observed_slope * log_length + observed_intercept;
      double pred = predicted_slope * log_length + predicted_intercept;
      if (!finite_num(length) || length <= 0.0 || !finite_num(obs) || !finite_num(pred) ||
          obs >= 0.0 || pred >= 0.0) {
        valid = false;
        break;
      }
      lengths[j] = length;
      observed[j] = obs;
      predicted[j] = pred;
    }
    if (!valid) continue;

    std::string species = CharacterVector::is_na(anchor_species[ai])
      ? std::string() : as<std::string>(anchor_species[ai]);
    for (int j = 0; j < grid_size; ++j) {
      double u = static_cast<double>(j) / static_cast<double>(grid_size - 1);
      double sigma_obs = std::pow(10.0, observed[j] / 10.0);
      double sigma_pred = std::pow(10.0, predicted[j] / 10.0);
      double error = predicted[j] - observed[j];
      double log_residual = std::log(sigma_obs / sigma_pred);
      if (!finite_num(error) || !finite_num(log_residual)) continue;

      out_anchor_id.push_back(id);
      out_anchor_species.push_back(species);
      out_policy.push_back(policy);
      out_branch.push_back(branch);
      out_policy_slope.push_back(predicted_slope);
      out_policy_intercept.push_back(predicted_intercept);
      out_min_distance.push_back(local_min_distance[i]);
      out_weighted_distance.push_back(local_weighted_distance[i]);
      out_effective_support.push_back(local_effective_support[i]);
      out_length_overlap.push_back(local_length_overlap[i]);
      out_depth_overlap.push_back(local_depth_overlap[i]);
      out_u.push_back(u);
      out_length.push_back(lengths[j]);
      out_ts_obs.push_back(observed[j]);
      out_ts_pred.push_back(predicted[j]);
      out_ts_error.push_back(error);
      out_abs_ts_error.push_back(std::abs(error));
      out_sigma_obs.push_back(sigma_obs);
      out_sigma_pred.push_back(sigma_pred);
      out_log_sigma_residual.push_back(log_residual);
    }
  }

  return DataFrame::create(
    _["anchor_model_id"] = out_anchor_id,
    _["anchor_species"] = out_anchor_species,
    _["policy"] = out_policy,
    _["equation_branch_filter"] = out_branch,
    _["policy_slope_len"] = out_policy_slope,
    _["policy_intercept_len"] = out_policy_intercept,
    _["local_min_combined_distance"] = out_min_distance,
    _["local_weighted_mean_combined_distance"] = out_weighted_distance,
    _["local_effective_support"] = out_effective_support,
    _["local_mean_length_overlap"] = out_length_overlap,
    _["local_mean_depth_overlap"] = out_depth_overlap,
    _["u"] = out_u,
    _["length_cm"] = out_length,
    _["ts_obs"] = out_ts_obs,
    _["ts_pred"] = out_ts_pred,
    _["ts_error"] = out_ts_error,
    _["abs_ts_error"] = out_abs_ts_error,
    _["sigma_obs"] = out_sigma_obs,
    _["sigma_pred"] = out_sigma_pred,
    _["log_sigma_residual"] = out_log_sigma_residual
  );
}
