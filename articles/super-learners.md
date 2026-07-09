# Super Learners

`tsbiomass` uses a Super Learner (stacked ensemble) in three distinct
places. Each learner has a different outcome, a different feature set,
and different practical constraints. Understanding which learner is
which, and what it is actually predicting, is the most important thing
before you start tuning.

``` r

library(tsbiomass)

# See what method names are valid in any context
list_learners()
```

Each learner section below includes a collapsible table of its valid
methods. The Alchemist accepts a restricted set. The Uncertainty and
Selection learners accept the full catalog.

------------------------------------------------------------------------

## The Super Learners

![](figures/super-learner-contexts.png)

------------------------------------------------------------------------

## Alchemist

**What it learns.** Given a table of per-trait pairwise distances (one
per donor–anchor pair), it predicts the absolute log-ratio of the
donor’s expected $`\sigma_\mathrm{bs}`$ to the anchor’s
$`\sigma_\mathrm{bs}`$, integrated over the anchor’s length
distribution. The resulting $`N\ \times N`$ distance matrix is the
backbone of all downstream similarity weighting, ordination, and
admissibility scoring.

**Available methods**

(Penalized) linear

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Generalized linear model | `glm` | The linear component of trait distance predicting differences in $`\sigma_\mathrm{bs}`$ is real. Plain OLS almost always receives meaningful NNLS weight. Coefficients show which trait dimensions matter most and are therefore interpretable. Fast. | Cannot capture nonlinear or interaction effects. If the difference in $`\sigma_\mathrm{bs}`$ is driven by threshold crossings rather than distance magnitude, GLM will be consistently biased toward the mean. |
| L2 regularization | `glm_ridge` | Handles multicollinearity between traits (e.g., genus and family). Trait distances move together by construction, and ridge shrinks them proportionally rather than picking one arbitrarily. Stable when feature count approaches pair count. | Does not zero out irrelevant features. No advantage over Elastic-net when sparsity is desired. Rarely competitive as the sole penalized choice. |
| L1 regularization | `glm_lasso` | Produces a sparse model identifying a few trait dimensions that actually drive $`\sigma_\mathrm{bs}`$ diveragence. Useful for diagnosing whether a small subset of traits dominates the trait distances. | Unstable when features are highly correlated, resulting in LASSO to arbitrarily pick among correlated traits. |
| Elastic-net | `glm_elastic` | Combines the collinearity hadling from ridge regressions with LASSO’s feature selection. The recommended default penalized choice for this context. Stable coefficient paths. Robust to overfitting on pairwise training data. | The $`\alpha`$ hyperparameter must be chosen before fitting. At the default $`\alpha = 0.25`$ (ridge-dominant) it may not select aggressively enough when many features are genuinely irrelevant. |

Smooth and piecewise

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Generalized additive models | `gam` | Captures smooth nonlinear relationships between individual trait distances. Each trait gets its own smooth term, so the shape of each dimension’s contribution is interpretable. Well-calibrated on medium-size pair tables. | Assumes independence of trait contibutions with no $`\text{trait} \times \text{trait}`$ interactions in the default setup. If differences in $`\sigma_\mathrm{bs}`$ is driven by joint combinations of traits, the GAM will miss them entirely. |
| Multivariate adaptive regression splines | `mars` | Detects sharp transitions in the trait distances (e.g., taxonomic divides). Handles threshold structure without requiring f ull interaction modeling. Faster than `rf` and `xgboost` on small pair tables. | With interaction hinge terms (`degree = 2`) the model can overfit in small pair tables. The backward pruning pass needs appropriate penalty tuning. Interpretation requires inspecting hinge knot positions. |

Tree-based

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Random forest | `rf` | Captures nonlinear interactions between multiple ttrait dimensions. Robust to irrelevant or redundant features. Strong default across a wide range of pair counts. | Memory-intensive when the pair tabel is large. Requires enough pairs per fold for reliable OOB error estimation. Sensitive to the minimum number of training observations allowed and decision trees. |
| eXtreme Gradient Boosting | `xgboost` | Highest capcity for capturing higher-order interactions between trait features. Each tree corrects residuals of the previous, allowing precise modeling of complex interactions. Best when many traits interact non-additively. | Needs sufficient pair counts to regularize effectively. In small candidate pools the pair count is too low and overfitting is severe. More hyperparameters to tune than `rf`. |
| Recursive partitioning and regression trees | `rpart` | Produces an explicitly interpretable decision tree that is computationally fast. When NNLS assigns `rpart` a substantial weight, this serves as a diagnostic that indicates the trait distances have hard thresholds that all continuous models are smoothing over. | Almost always dominated by `rf` or `mars` in an ensemble. Single tree has high variance across folds. Not competitive as a standalone predictor. |

Conditional quantile

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Quantile regression | `qreg` | Models a conditional quantile of distance rather than the mean. At $`\tau > 0.5`$ it targets the tail-ends (pessimistic) of the distance distribution. Useful when some trait combinations produce high-variance $`\sigma_\mathrm{bs}`$ outcomes and one wants the ensemble to account for the tail. | Linear quantile regression shares GLM’s inability to capture nonlinear effects. Numerically unstable with veyr few pairs. The $`\tau`$ parameter must be chosen deliberately. The default $`\tau = 0.5`$ collapses to median regression with no tail benefit. |

**Recommended default**

``` yaml
alchemist:
  learner:
    methods: [glm_elastic, rf, xgboost]
    inner_folds: 5
    outcome_transform: log1p
    lambda_rule: lambda.1se
```

The default three-method ensemble balances regularized linearity
(`glm_elastic`), nonlinear main effects (`rf`), and interaction capture
(`xgboost`). The NNLS combiner then determines how much weight each
method gets per fold.

Before tuning the learner library, check `feature_type`. The default
`feature_type = “gower”` is unsigned, which means nonlinear methods like
`rf` and `xgboost` must discover asymmetries themselves. Switching to
`feature_type = “difference”` encodes direction explicitly and often
improves RF and XGBoost generalization without any other change.

If your candidate pool has good phylogenetic coverage, setting
`taxonomic_distance = TRUE` replaces the separate species, genus,
family, etc., traits with a single continuous distance from the Open
Tree of Life. This reduces feature collinearity and tends to improve
out-of-fold accuracy for methods with limited regularization (e.g.,
`gam`, `rpart`).

**Growing or shrinking the library**

The Alchemist is always a stacked ensemble: the `methods:` list *is* the
library, and the NNLS combiner reweights it per fold. There is no
separate `super_learner` switch here (that toggle exists only for the
Uncertainty and Selection learners). Expand the library (add `mars`,
`gam`, `rpart`, or a tail-targeting `qreg`) when the pair table is large
and you suspect threshold or tail structure the default three miss.
Shrink toward `[glm_elastic, rf]` when the candidate pool is small,
since few models means few pairs and the richer learners overfit.

**Method notes**

`glm`

Unpenalized OLS. No hyperparameters. The fast linear reference member of
the library.

`glm_ridge`

`glmnet` with `alpha = 0` (pure L2). `lambda` is chosen by inner
cross-validation (`lambda_rule`). Features are standardized and the CV
loss is MAE.

`glm_lasso`

`glmnet` with `alpha = 1` (pure L1). `lambda` by inner CV.

**Watch out:** Picks arbitrarily among correlated trait columns. In the
Alchemist, where trait distances are correlated by construction, prefer
`glm_elastic`.

`glm_elastic`

`glmnet` with configurable `alpha` (default `0.25`, ridge-leaning).
Increase toward `1.0` for sparser feature selection. `lambda` by
`lambda_rule`.

**Watch out:** In very high-dimensional feature spaces (many trait
expansions, `feature_type = "difference"` with set-type traits),
`lambda_rule = "lambda.min"` may overfit the training pairs, so prefer
`lambda.1se` here.

`gam`

`mgcv`, one smooth term per feature. Fitted with `fit_method = "REML"`
and `select_terms = TRUE`, so the extra penalty can shrink an
uninformative smooth to zero. Benefits from `taxonomic_distance = TRUE`,
which cuts feature collinearity.

`mars`

Requires the `earth` package. Defaults: `degree = 2` (pairwise hinge
interactions, appropriate for the Alchemist), `penalty = 3`,
`pmethod = "backward"`, `nprune = NULL` (GCV chooses the retained term
count).

`rpart`

Single tree. Defaults `cp = 0.01`, `minsplit = 20`, `minbucket = 7`,
`maxdepth = 30`. Mostly a diagnostic member. Substantial NNLS weight
flags hard thresholds in the distance surface. Benefits from
`taxonomic_distance = TRUE`.

`rf`

`ranger`. Defaults `num_trees = 500`, `min_node_size = 5`, `mtry = NULL`
(auto), `sample_fraction = 1`, `replace = TRUE`.

**Watch out:** Memory-intensive with many pairs. If memory is tight,
reduce `num_trees` to 200–300 before reducing `sample_fraction`.

`xgboost`

Defaults `nrounds = 100`, `eta = 0.30`, `max_depth = 6`,
`min_child_weight = 1`, `lambda = 1`, `alpha = 0`. For small pair tables
use the `conservative` variant (`eta = 0.05`, `max_depth = 3`,
`min_child_weight = 5`, `lambda = 2`).

**Watch out:** In small candidate pools (N \< 50 models) the pair count
may be too low to regularize, so drop `xgboost` and rely on
`glm_elastic` and `rf` alone.

`qreg`

`quantreg`, `fit_method = "fn"`. The default `tau = 0.50` is median
regression (no tail benefit). Use the `q75`/`q90` variants
(`tau = 0.75`/`0.90`) to target the pessimistic tail of the distance
distribution.

**Watch out:** Numerically unstable with very few pairs.

------------------------------------------------------------------------

## Learner 2: Uncertainty (`uncertainty`)

**What it learns.** Cross-fitted regression that predicts the expected
absolute log-transfer-error for each anchor policy combination. The
fitted predictions are used to calibrate conformal prediction intervals:
once you know where the error tends to be large, the intervals can be
widened there and tightened elsewhere.

**Available methods**

(Penalized) linear

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Generalized linear model | `glm` | Linear baseline for error prediction. Often earns real NNLS weight because error scales with features like support size or nearest-anchor distance. Anchors the ensemble and is interpretable. | Cannot capture nonlinear or threshold effects. If error spikes sharply beyond a similarity threshold, GLM underestimates the high-error region and produces under-wide intervals there. |
| Ridge (L2) regression | `glm_ridge` | Handles correlated benchmark statistics. Policy score and interval width move together, and ridge shrinks them proportionally rather than picking one. Stable when the benchmark feature table has many columns. | Does not zero out uninformative features. Marginal benefit over `glm_elastic` in most settings. Not recommended as the sole penalized member. |
| LASSO (L1) regression | `glm_lasso` | Selects the few benchmark features most predictive of error magnitude. Useful for trimming a large benchmark feature table and for seeing which covariates actually drive error. | Unstable when features are correlated. LASSO picks arbitrarily among correlated columns. In practice `glm_elastic` is almost always preferred. |
| Elastic-net | `glm_elastic` | Balances selection and shrinkage when benchmark features are numerous and correlated. Works well with `lambda.min` here because the downstream conformal calibration adds regularization. | Requires `alpha` and `lambda` tuning. At the default `alpha = 0.25` (ridge-dominant) it may not select aggressively enough in high-dimensional benchmark tables. |

Smooth and piecewise

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Generalized additive models | `gam` | Captures smooth nonlinear error-covariate relationships, such as error accelerating as support distance increases. Each benchmark feature gets its own smooth term, so each contribution is interpretable. Well-calibrated on medium-size tables. | Assumes additive, independent contributions from each benchmark feature. If error is driven by interactions (low support and poor frequency match together), the GAM misses them entirely. |
| Multivariate adaptive regression splines | `mars` | Models threshold behavior in error, e.g. error spiking once the nearest anchor falls outside a similarity radius. Handles discontinuities that GAM and GLM smooth over. Faster than `rf` on small benchmark tables. | With interaction hinge terms (`degree = 2`) and many features it can overfit small benchmark tables. The backward pruning pass needs tuning to avoid retaining spurious terms. |

Tree-based

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Random forest | `rf` | Captures nonlinear interactions between benchmark features and error magnitude (high error only when both support is low and frequency match is poor). Robust to irrelevant features. Recommended in nearly every uncertainty ensemble. | Mean-regression forest targets the expected error, not the upper tail that determines interval width. Always pair `rf` with `qreg` or `qrf`, which directly target the tail. |
| eXtreme Gradient Boosting | `xgboost` | Higher-capacity interaction learning for error prediction. Sequential boosting corrects residuals left by simpler models. Strong when the benchmark training set is large. | Small anchor-policy training sets are the norm. Without conservative settings (`eta ≤ 0.05`, `min_child_weight ≥ 5`) XGBoost overfits the benchmark table severely. |
| Recursive partitioning and regression trees | `rpart` | Produces an interpretable stratification of error by benchmark strata. Fast. Meaningful NNLS weight signals that error concentrates in specific strata (a policy family, a species group) that continuous models average over. | Almost always dominated by `rf` or `qrf` in the ensemble. Single tree has high variance. Not competitive alone. |

Bayesian additive trees

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Bayesian additive regression trees | `bart` | Pure-MCMC BART. Well-calibrated probabilistic error predictions that integrate smoothly with the conformal calibration step. Handles nonlinear interactions and regularizes automatically through its prior. | Computationally expensive. Convergence depends on the number of MCMC samples. Requires the `stochtree` package. |
| Accelerated BART | `xbart` | Grow-from-root only (`num_mcmc = 0`). Fastest BART variant with no MCMC burn-in. Produces a point estimate of the error surface. Good when training speed is the bottleneck and other posterior-aware members are present. | No posterior uncertainty, effectively a deterministic approximation. May underestimate tail behavior relative to full BART. Always pair with `qreg` or `qrf`. |
| Warm-start BART | `wsbart` | A short grow-from-root initialization followed by full MCMC. Reaches posterior convergence faster than pure `bart` for the same draw budget. A middle ground between `xbart` speed and `bart` posterior quality. | Still requires MCMC. Slower than `xbart`. Both the grow-from-root budget and MCMC draw count must be configured. Improvement over plain `bart` is often marginal. |
| Variance-forest BART | `vfbart` | Heteroskedastic BART with a separate variance forest that adapts the error scale to each region of the covariate space, making it the most natural BART variant where error magnitude is explicitly non-constant. Produces a mean and a conditional variance. | Most complex and expensive BART variant. Requires `variance_forest_num_trees` tuning. Can overfit the variance surface with small benchmark training sets. |
| Random-effects BART | `rebart` | BART with an additive group random effect (lmm-style): absorbs systematic per-species error shifts rather than forcing the tree ensemble to explain them. Set the group via `method_settings.bart.random_effects_group`. | Requires `random_effects_group` to be set. Random intercept and tree ensemble are estimated jointly, slower than standalone `bart`. May not converge with very few anchor species per group. |

Conditional quantile

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Quantile regression | `qreg` | Directly targets the upper quantile of the error distribution. At $`\tau = 0.75`$ or $`0.90`$ it models exactly the tail that determines where intervals must be wide. Typically earns substantial NNLS weight and reduces coverage error. | Linear quantile regression cannot capture nonlinear or interaction effects. Numerically unstable with fewer than ~100 anchor-policy pairs. The $`\tau`$ value must be chosen deliberately. |
| Quantile regression forest | `qrf` | Combines the nonlinear flexibility of `rf` with direct quantile targeting. Captures tail-error behavior driven by feature interactions that the linear `qreg` misses. Natural complement to `qreg`. | Slower than standard `rf`. The quantile target must be set in `method_settings.qrf` (default 0.9). Provides no mean prediction, so NNLS needs mean-regression members alongside it. |

Kernel and instance-based

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Gaussian process regression | `gpr` | Provides a native predictive variance alongside the mean, a qualitatively different shape of uncertainty that often improves ensemble coverage. Well-calibrated in small-to-medium samples. | $`O(N^3)`$ cost. Avoid in large training sets. Fails or degenerates when features are nearly collinear. The nugget (`method_settings.gpr.var`) must be raised if fitting warnings appear. |
| Support vector regression | `svr` | Non-parametric kernel regression useful when the error surface is complex but smooth and other nonlinear methods overfit. The RBF kernel captures local structure without hard boundaries. | Slow to fit and tune (kernel width and cost). Provides no probabilistic prediction. Rarely the highest-weight member of the ensemble. |
| k-nearest neighbors | `knn` | Predicts error by local averaging over similar benchmark cases. Captures patchy local structure (taxonomic pockets with consistently higher error) that global parametric models smooth over. No distributional assumptions. | Sensitive to `k` and the distance metric. Does not extrapolate beyond observed cases. Slow at prediction time in large tables. |

Rule-based

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Cubist rule-based model | `cubist` | Rule-based stratification of error by benchmark features. Produces an interpretable error model and adds structural diversity distinct from tree-based members. | Requires the `Cubist` package. Sensitive to `committees` and `neighbors`. Rules can be sparse and unstable in small benchmark tables. |

Mixed effects

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Linear mixed model | `lmm` | Accounts for species-level clustering in error. If some species are systematically harder to transfer regardless of policy, LMM captures that baseline shift as a random intercept rather than forcing other learners to explain it through features. | Requires `group_col` to be set in the uncertainty config. The random intercept is estimated from training anchors only and is unreliable with few species per group. No nonlinear effects. |

Baseline and ensemble

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Grand mean baseline | `mean` | Zero hyperparameters. Fast. Useful as a sanity check in the ensemble. | Ignores all benchmark features. Meaningful NNLS weight signals that features are uninformative for error prediction and intervals will be marginal rather than conditional. |
| Super Learner (stacked ensemble) | `super_learner` | Enables full ensemble mode within the uncertainty stage: the NNLS combiner learns optimal weights from cross-validated fold predictions rather than equal weighting. | Adds a nested cross-fitting loop (`inner_folds` within each outer fold), substantially increasing compute time. The `super_methods` library must be chosen deliberately. |

**Recommended default**

``` yaml
uncertainty:
  method: super_learner
  super_methods: [glm, glm_elastic, rf, xgboost, qreg, qrf, gpr]
  n_folds: 5
  inner_folds: 3
  outcome_col: error_abs_log
  outcome_transform: log1p
  lambda_rule: lambda.min
```

The full library is appropriate here because the outcome (absolute
log-error) is well-behaved (non-negative, roughly log-normal) and
quantile-targeting methods add diversity that helps the NNLS combiner.

Adding `qreg` and `qrf` alongside mean-regression methods gives the NNLS
combiner access to upper-tail predictions. Because prediction intervals
are set at the upper tail of the error distribution, these quantile
learners often receive substantial weight and meaningfully reduce
coverage error.

For the uncertainty learner, prefer `lambda.min` over `lambda.1se`. The
uncertainty stage is already regularized by the conformal calibration
step that follows. Aggressive penalization in the learner can leave
systematic variance unexplained and produce poorly calibrated intervals.

**When to use `super_learner`**

The uncertainty learner ships with `method: super_learner` because the
error outcome is well-behaved and the quantile members add real coverage
value. Keep the stacked ensemble unless the anchor-policy table is very
small (fewer than ~100 pairs), where the nested inner folds leave too
little data per fit, so drop to a single robust `method:` (`rf` or
`glm_elastic`) and skip the quantile members. When you do keep
`super_learner`, always pair at least one mean-regression member
(`glm`/`rf`) with the tail-targeting members (`qreg`/`qrf`) so the
combiner has both.

**Method notes**

`glm`

Unpenalized OLS. No hyperparameters. The linear reference member.

`glm_ridge`

`glmnet` with `alpha = 0` (pure L2). `lambda` by inner CV
(`lambda_rule`). Features standardized, CV loss MAE.

`glm_lasso`

`glmnet` with `alpha = 1` (pure L1). `lambda` by inner CV. Picks
arbitrarily among correlated benchmark columns, so prefer `glm_elastic`.

`glm_elastic`

`glmnet` with configurable `alpha` (default `0.25`). `lambda` by
`lambda_rule`. Prefer `lambda.min` here, since the downstream conformal
step adds its own regularization.

`gam`

`mgcv`, one smooth per benchmark feature, with `fit_method = "REML"`,
`select_terms = TRUE` (an uninformative smooth can be shrunk to zero).

`mars`

Requires the `earth` package. Defaults `degree = 2`, `penalty = 3`,
`pmethod = "backward"`.

**Watch out:** With many benchmark features, `degree = 2` can overfit a
small benchmark table, so drop to `degree = 1` if the backward pass
retains spurious terms.

`rpart`

Single tree. Defaults `cp = 0.01`, `minsplit = 20`, `minbucket = 7`,
`maxdepth = 30`. A diagnostic stratifier. Substantial weight signals
error concentrated in specific strata.

`rf`

`ranger` with defaults `num_trees = 500`, `min_node_size = 5`,
`sample_fraction = 1`.

**Watch out:** Mean-regression forest. It targets the expected error,
not the tail. Always pair it with `qreg` or `qrf`.

`xgboost`

Defaults `nrounds = 100`, `eta = 0.30`, `max_depth = 6`,
`min_child_weight = 1`, `lambda = 1`. Use the `conservative` variant
(`eta = 0.05`, `max_depth = 3`, `min_child_weight = 5`, `lambda = 2`)
because the anchor-policy table is small.

`bart`

`stochtree`. Shared priors `num_trees = 75`, `alpha = 0.95`, `beta = 2`,
`min_samples_leaf = 5`, `max_depth = 10`. Sampler `num_gfr = 0`,
`num_burnin = 100`, `num_mcmc = 200`. Requires the `stochtree` package.

`xbart`

Accelerated BART: sampler `num_gfr = 40`, `num_mcmc = 0` (grow-from-root
only, `keep_gfr = TRUE`). Point estimate only, so pair with `qreg`/`qrf`
for the tail.

`wsbart`

Warm-start BART: sampler `num_gfr = 20`, `num_mcmc = 200`. Shares the
BART priors above.

`vfbart`

Heteroskedastic BART: `variance_forest = TRUE`,
`variance_forest_num_trees = 50`. Sampler `num_burnin = 100`,
`num_mcmc = 200`. The most natural BART variant here, since error
magnitude is explicitly non-constant.

`rebart`

BART with a group random effect: `random_effects = TRUE`, group set via
`method_settings.bart.random_effects_group` (default `.split_group`).
Absorbs systematic per-species error.

`qreg`

`quantreg`, `fit_method = "fn"`. The default `tau = 0.50` is median
regression. Use the `q75`/`q90` variants (`tau = 0.75`/`0.90`) to model
the upper tail that sets interval width.

**Watch out:** Numerically unstable with very few rows. With fewer than
~100 anchor-policy pairs, drop `qreg` and rely on `rf` and
`glm_elastic`.

`qrf`

`ranger` with `quantreg = TRUE` with defaults `num_trees = 500`,
`min_node_size = 10`, `quantile = 0.9`. Adjust `quantile` if your target
coverage differs from 90%.

`gpr`

[`kernlab::gausspr`](https://rdrr.io/pkg/kernlab/man/gausspr.html)
(RBF). Kernel width estimated automatically, `var = 0.001` noise nugget.
Computationally $`O(N^3)`$. Avoid in very large training sets.

**Watch out:** Can fail or degenerate when features are nearly
collinear. Raise `method_settings.gpr.var` if you see fitting warnings.

`svr`

[`kernlab::ksvm`](https://rdrr.io/pkg/kernlab/man/ksvm.html) (eps-SVR,
RBF) with defaults `C = 1`, `epsilon = 0.1`, kernel width auto. Provides
no probabilistic prediction.

`knn`

[`FNN::knn.reg`](https://rdrr.io/pkg/FNN/man/knn.reg.html) on the
standardized feature matrix with default `k = 10`. Purely local. Does
not extrapolate beyond observed cases.

`cubist`

Requires the `Cubist` package with defaults `committees = 1`,
`neighbors = 0` (`neighbors` up to 9 adds an instance-based correction
at prediction time).

`lmm`

`lme4`, `fit_method = "REML"`, `random_intercept = ".split_group"`.
Requires the `lme4` package and a populated `group_col`.

`mean`

Grand-mean baseline. No hyperparameters. Meaningful NNLS weight is a
warning that the benchmark features are uninformative.

`super_learner`

Nested stacking: set the base library in `super_methods` and the
nested-fit count in `inner_folds`. The NNLS combiner weights the base
learners from their inner out-of-fold predictions.

------------------------------------------------------------------------

## Learner 3: Selection / PolicyLearner (`selection`)

**What it learns.** A cross-fitted meta-learner that predicts
transfer-error rank for each policy, given the context of the target
species (benchmark features, anchor availability, similarity scores).
This is the model that `PolicyLearner` uses to recommend the best policy
per target at prediction time.

**Available methods**

(Penalized) linear

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Generalized linear model | `glm` | Linear scoring of policy performance from benchmark context. Fast, stable, interpretable. The right starting point before anything richer. Rarely catastrophically wrong even when the selection surface is mildly nonlinear. | Cannot capture nonlinear or threshold effects in policy ranking. If policy performance switches sharply at a covariate threshold (e.g. a species-based policy dominates only above a support cutoff), GLM smooths over it and makes suboptimal selections. |
| Ridge (L2) regression | `glm_ridge` | Handles correlated benchmark statistics. Policy scores across similar policies move together and ridge shrinks them proportionally. More stable than plain GLM when many policy-performance features are included. | Does not zero out irrelevant features. Marginal benefit over `glm_elastic` in most settings. Not recommended as the sole penalized member. |
| LASSO (L1) regression | `glm_lasso` | Selects the few context features most predictive of policy rank. Useful for understanding which target properties (support size, nearest-anchor similarity) actually drive policy choice. | Unstable when features are correlated. LASSO picks arbitrarily among correlated columns. `glm_elastic` is almost always preferred in practice. |
| Elastic-net | `glm_elastic` | Balances selection and shrinkage. Good when the benchmark feature set is large and correlated. Stable coefficient paths. | Requires `alpha` and `lambda` tuning. At `alpha = 0.25` (default, ridge-dominant) it may not select aggressively enough when many features are genuinely irrelevant. |

Smooth and piecewise

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Generalized additive models | `gam` | Captures smooth nonlinear relationships between target context and policy performance rank, e.g. performance degrading at an accelerating rate as taxonomic distance from the anchor pool increases. Each feature gets its own smooth term, so the relationship is interpretable. | Assumes independent, additive contributions from context features. If policy ranking depends on interactions between features (support and frequency match jointly), the GAM misses them entirely. |
| Multivariate adaptive regression splines | `mars` | Models threshold-like selection rules: a species-based policy wins when taxonomic support exceeds a threshold, a generalized policy wins otherwise. Adds structural diversity that GLM misses without the data demands of `rf`. Handles small-to-medium anchor counts well. | With `degree = 2` and few anchors, interaction hinge terms can be unstable. The backward pruning pass must be tuned. Interpretation requires inspecting hinge knot positions. |

Tree-based

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Random forest | `rf` | Captures complex interactions between context features and policy performance. Robust to irrelevant features. Appropriate when anchor count is large enough for reliable out-of-species generalization (≥ 30 species). | Tends to overfit the benchmark table with fewer than ~30 anchor species. If Sentinel outer folds show high variance in which policy is selected across folds, `rf` is likely the culprit. Switch to `glm_elastic` or `mars`. |
| eXtreme Gradient Boosting | `xgboost` | Highest capacity for interaction learning in policy scoring. Sequential boosting corrects residuals of simpler models. Strong when many context features interact non-additively to determine policy rank. | The training table for selection is small by default. Without conservative settings (`eta ≤ 0.05`, `min_child_weight ≥ 5`) XGBoost overfits severely. Reserve for large, taxonomically diverse anchor sets. |
| Recursive partitioning and regression trees | `rpart` | Produces an explicitly interpretable selection rule. Fast. Meaningful NNLS weight signals a dominant hard threshold in the selection logic that all smooth models are missing. | Almost always dominated by `rf` or `mars` in an ensemble. Single tree has high variance across holdout folds. Not competitive as a standalone predictor. |

Bayesian additive trees

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Bayesian additive regression trees | `bart` | Pure-MCMC BART. Provides posterior uncertainty over which policy is best, informative when no single policy is clearly superior across Sentinel folds. Regularizes automatically through its prior. | Computationally expensive. Convergence depends on the number of MCMC samples. Requires the `stochtree` package. |
| Accelerated BART | `xbart` | Grow-from-root only (`num_mcmc = 0`). Fastest BART variant with no MCMC burn-in. Produces a deterministic point estimate of policy scores. Good when training speed is the bottleneck and posterior uncertainty is not needed here. | No posterior uncertainty quantification. May underperform full `bart` in distinguishing closely ranked policies. Best used alongside members that provide richer uncertainty information. |
| Warm-start BART | `wsbart` | Grow-from-root initialization followed by full MCMC. Reaches posterior convergence faster than pure `bart` for the same draw budget. A practical middle ground between `xbart` speed and `bart` full posterior quality. | Still requires MCMC. Slower than `xbart`. Both the grow-from-root and MCMC draw budgets must be configured. Improvement over plain `bart` can be marginal. |
| Variance-forest BART | `vfbart` | Heteroskedastic BART with a separate variance forest. Models non-constant variance in policy scores across the covariate space, adding a conditional variance prediction. Useful when score dispersion varies systematically with context (e.g. high variance only for sparsely supported species). | Most complex and expensive BART variant. Requires `variance_forest_num_trees` tuning. Can overfit the variance surface with few anchor species. |
| Random-effects BART | `rebart` | BART with an additive species-level random effect (lmm-style): absorbs baseline per-species variation in policy performance, combined with BART’s nonlinear mean model. Set the group via `method_settings.bart.random_effects_group`. | Requires `random_effects_group` to be set. Joint estimation of random effects and tree ensemble is slower than standalone `bart`. Unreliable with very few anchor species per group. |

Conditional quantile

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Quantile regression | `qreg` | Models a conditional quantile of policy scores rather than the mean. Useful for conservative policy selection targeting the upper tail of expected error rather than average performance. Adds quantile-loss diversity to the ensemble. | Linear quantile regression cannot capture nonlinear ranking effects. Numerically unstable with few training rows. The $`\tau`$ value must be chosen deliberately. |
| Quantile regression forest | `qrf` | Combines the nonlinear flexibility of `rf` with direct quantile targeting for policy scoring. Captures quantile-specific interaction effects that the linear `qreg` misses. | Requires enough anchors for the forest to generalize (same ~30 species threshold as `rf`). The quantile target must be set in `method_settings.qrf`. |

Kernel and instance-based

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Gaussian process regression | `gpr` | Well-calibrated in small anchor sets where tree-based methods overfit. Contributes a qualitatively different error-surface shape via its native predictive variance. Useful precisely when `rf` and `xgboost` cannot be used safely. | $`O(N^3)`$ computational cost. Collinear features cause numerical issues. Rarely competitive when anchor count exceeds ~100 species. |
| Support vector regression | `svr` | Non-parametric kernel regression for policy scoring. Useful when the performance surface is complex but smooth and other nonlinear methods overfit. | Slow to fit and tune. Provides no probabilistic prediction. Rarely the highest-weight member of the selection ensemble. |
| k-nearest neighbors | `knn` | Policy selection by local similarity to past anchor contexts. Useful when the policy landscape is patchy, so similar target contexts should get similar recommendations regardless of global trends. No distributional assumptions. | Sensitive to `k` and the distance metric. Does not extrapolate. Slow at prediction time in large benchmark tables. |

Rule-based

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Cubist rule-based model | `cubist` | Rule-based policy scoring producing interpretable stratified selection rules by target context. Adds structural diversity distinct from tree-based members. Locally linear predictions within each rule. | Requires the `Cubist` package. Sensitive to `committees` and `neighbors`. Rules can be sparse and unstable with few anchors. |

Mixed effects

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Linear mixed model | `lmm` | Accounts for baseline species-level variation in policy performance independent of context features. If some species are universally better or worse served by particular policy families, LMM captures that as a random intercept rather than forcing other learners to explain it through covariates. | Requires `lme4` and `group_col` to be set in the selection config. The random intercept is estimated from training anchors only and is unreliable with few species per group. No nonlinear effects. |

Baseline and ensemble

| Method | Arg. | Strengths | Weaknesses |
|----|----|----|----|
| Grand mean baseline | `mean` | Constant predictor. Zero hyperparameters. Fast. Useful as a sanity check. | Ignores all benchmark features. Meaningful NNLS weight signals that features are not informative for policy selection and any policy is as good as any other. |
| Super Learner (stacked ensemble) | `super_learner` | Enables full ensemble mode within the selection stage: the NNLS combiner learns optimal weights from cross-validated fold predictions rather than equal weighting. | Adds nested cross-fitting (`inner_folds` within each outer fold), substantially increasing compute time. The `super_methods` library must be chosen deliberately. Adding similar methods without diversity does not help. |

**Recommended default for most analyses**

``` yaml
selection:
  method: glm
  n_folds: 5
  inner_folds: 5
  outcome_transform: log1p
  lambda_rule: lambda.1se
```

A regularized GLM is the right starting point. The policy meta-learner
trains on a relatively small cross-validated benchmark table (one row
per anchor × policy), and complex methods tend to overfit without
substantial gains in holdout performance.

**When to use `super_learner`**

Switch to `method: super_learner` when:

- You have many anchors (≥ 30–40 species across diverse ecological
  contexts).
- Your benchmark shows evidence that policy performance varies
  nonlinearly with target covariates (inspect `plot(learner)` for
  feature-importance diagnostics).
- Outer-fold holdout validation (Sentinel) shows that the GLM is
  consistently selecting the wrong policy family.

``` yaml
selection:
  method: super_learner
  super_methods: [glm, glm_elastic, rf, mars]
  n_folds: 5
  inner_folds: 5
```

For the selection learner, a 3-4 method library
(e.g. `[glm, glm_elastic, rf, mars]`) usually out-performs a large
library. Adding many similar methods (multiple XGBoost variants,
multiple RF variants) without diversity rarely improves the NNLS
combiner and inflates cross-fitting time.

Set `outcome_clip_quantile` to 0.95-0.99 (default 0.99). A handful of
extreme benchmark errors can dominate the learner target and prevent the
model from distinguishing between policies in the moderate-performance
range where most selection decisions happen.

**Method notes**

`glm`

Unpenalized OLS. No hyperparameters. The recommended starting point. If
learner residuals show a clear pattern against outcome quantiles, move
to a richer family.

`glm_ridge`

`glmnet` with `alpha = 0` (pure L2). `lambda` by inner CV
(`lambda_rule`). Features standardized, CV loss MAE.

`glm_lasso`

`glmnet` with `alpha = 1` (pure L1). `lambda` by inner CV. Picks
arbitrarily among correlated context features, so prefer `glm_elastic`.

`glm_elastic`

`glmnet` with configurable `alpha` (default `0.25`). `lambda` by
`lambda_rule` (default `lambda.1se` for selection). Shrinks irrelevant
policy-context features toward zero.

`gam`

`mgcv`, one smooth per context feature, with `fit_method = "REML"`,
`select_terms = TRUE`.

`mars`

Requires the `earth` package. Defaults `degree = 2`, `penalty = 3`,
`pmethod = "backward"`.

**Watch out:** In the selection learner keep `degree = 1` (additive
hinges only) unless you have many anchors. `degree = 2` is highly
unstable with fewer than ~20 species.

`rpart`

Single tree. Defaults `cp = 0.01`, `minsplit = 20`, `minbucket = 7`,
`maxdepth = 30`. Yields an interpretable selection rule, mostly
diagnostic.

`rf`

`ranger` with defaults `num_trees = 500`, `min_node_size = 5`.
Appropriate at ≥ 30 anchor species; with fewer it overfits and ranks
policies inconsistently.

**Watch out:** If Sentinel outer folds show high variance in which
policy is selected, `rf` is unstable. Switch to `glm_elastic` or `mars`
before tuning hyperparameters.

`xgboost`

Defaults `nrounds = 100`, `eta = 0.30`, `max_depth = 6`,
`min_child_weight = 1`, `lambda = 1`. Reserve for large anchor sets and
use the `conservative` variant (`eta = 0.05`, `max_depth = 3`,
`min_child_weight = 5`, `lambda = 2`). The selection table is small.

`bart`

`stochtree`. Shared priors `num_trees = 75`, `alpha = 0.95`, `beta = 2`,
`min_samples_leaf = 5`, `max_depth = 10`. Sampler `num_gfr = 0`,
`num_burnin = 100`, `num_mcmc = 200`. Requires the `stochtree` package.

`xbart`

Accelerated BART: sampler `num_gfr = 40`, `num_mcmc = 0` (grow-from-root
only). Deterministic point estimate of policy scores.

`wsbart`

Warm-start BART: sampler `num_gfr = 20`, `num_mcmc = 200`. Shares the
BART priors above.

`vfbart`

Heteroskedastic BART: `variance_forest = TRUE`,
`variance_forest_num_trees = 50`. Sampler `num_burnin = 100`,
`num_mcmc = 200`. Use only when policy-score dispersion clearly varies
with context.

`rebart`

BART with a group random effect: `random_effects = TRUE`, group set via
`method_settings.bart.random_effects_group` (default `.split_group`).
Absorbs baseline per-species variation in policy performance.

`qreg`

`quantreg`, `fit_method = "fn"`. Default `tau = 0.50`. Use the
`q75`/`q90` variants for conservative selection targeting the upper tail
of expected error.

`qrf`

`ranger` with `quantreg = TRUE` with defaults `num_trees = 500`,
`min_node_size = 10`, `quantile = 0.9`. Same ~30-species threshold as
`rf`.

`gpr`

[`kernlab::gausspr`](https://rdrr.io/pkg/kernlab/man/gausspr.html)
(RBF). Kernel width auto, `var = 0.001` noise nugget. $`O(N^3)`$.
Well-calibrated in small anchor sets where trees overfit, rarely
competitive above ~100 species.

`svr`

[`kernlab::ksvm`](https://rdrr.io/pkg/kernlab/man/ksvm.html) (eps-SVR,
RBF) with defaults `C = 1`, `epsilon = 0.1`, kernel width auto. No
probabilistic prediction.

`knn`

[`FNN::knn.reg`](https://rdrr.io/pkg/FNN/man/knn.reg.html) on the
standardized feature matrix with default `k = 10`. Local similarity to
past anchor contexts. Does not extrapolate.

`cubist`

Requires the `Cubist` package with defaults `committees = 1`,
`neighbors = 0` (`neighbors` up to 9 adds an instance-based correction
at prediction time).

`lmm`

`lme4`, `fit_method = "REML"`, `random_intercept = ".split_group"`.

**Watch out:** Requires the `lme4` package and a populated group column.
Make sure `group_col` in the selection config resolves to a valid
species identifier in the benchmark table.

`mean`

Grand-mean baseline. No hyperparameters. Meaningful NNLS weight means no
policy is distinguishable from any other, so revisit the feature set or
benchmark.

`super_learner`

Nested stacking: set the base library in `super_methods` and the
nested-fit count in `inner_folds`. See *When to use `super_learner`*
above for when to switch the selection learner into this mode.

------------------------------------------------------------------------

## Shared settings across all three contexts

All three Super Learner slots share the same cross-fitting machinery.

| Setting | What it controls |
|----|----|
| `n_folds` | Number of outer folds. Larger = more stable OOF predictions, more compute. |
| `inner_folds` | Inner folds for lambda selection in penalized methods. 3–5 is usually enough. |
| `outcome_transform` | Applied before fitting. `"log1p"` is the default and right for error outcomes. |
| `lambda_rule` | `"lambda.1se"` is more conservative, while `"lambda.min"` fits more aggressively. |
| `seed` | Set for reproducibility. Especially important for Sentinel outer folds. |
| `workers` | Parallel processes. One worker per base learner per fold is the ceiling. |

The NNLS metalearner loss must always be `"squared_error"`. Do not
change this. It is required for non-negativity constraints to be
meaningful.
