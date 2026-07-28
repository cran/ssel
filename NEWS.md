# ssel 0.3.1

## Bug fixes

- `trainRegressionModel()` now restores the exact named `R2` and `RMSE`
  score lists for each response--dataset cell before unseen-row prediction.
  Ensemble weights and method pruning are therefore cell-local and no longer
  inherit state from the last OOF cell visited. Weight formulas, equal-weight
  fallbacks, output schemas, and selectors are unchanged.

## Documentation

- The ensemble, iterative multi-response, and semi-supervised articles now
  define their symbols, equations, API boundaries, package policies, and
  limitations directly from the implemented public contracts.
- Retired design and unsupported terminology were removed, including claims
  of predictive quantiles, calibrated uncertainty, Boruta, scale-free
  convergence, tri-training, held-out validation, and guaranteed predictive
  gain.

# ssel 0.3.0

## CRAN normalization

- Progress, diagnostics, warnings, and fatal failures now use R condition
  channels instead of raw console output. Quiet mode suppresses progress and
  structural messages without hiding warnings or errors.
- Package-created parallel backends are limited to two workers and are reset
  on exit, including failure paths.
- Prediction contract failures no longer enter an interactive debugger.
- `auditOverfit()` now discovers mixed-case caret method names consistently
  with `oofEnsemble()`.
- Source builds exclude internal `dev/` and `TITO/` material. DESCRIPTION and
  README metadata now match the implemented public API and include verified
  method references.

## Semi-supervised pipeline

- New exported stage: `semiSupervisedPipeline()`. Package-defined range-ratio
  pseudo-label promotion with an out-of-fold squared-correlation gauge over
  original labelled rows, a key budget, absolute-tolerance reversion with an
  accepted-state refit, and final serial cell-file re-emission. With
  `.path.iter` pointing to a `chainPipeline()` iteration directory, the
  baseline can use one selected augmented dataset snapshot.

# ssel 0.2.0

## Bug fixes

* `trainRegressionModel()` now promotes integer predictor columns to numeric
  after reading train/test split CSVs in the training, metrics, and prediction
  paths. This keeps caret model frames type-stable when `data.table::fread()`
  infers integer in one split and double in another, avoiding prediction-time
  class mismatch failures without changing the numeric values.

## Development notes

* The semi-supervised prototype remains quarantined under `inst/dev/sot/` and
  is not part of the public API in this release. The redesign items are tracked
  as planned work before any exported semi-supervised entry point is added.

# ssel 0.1.2

## Bug fixes

* `buildDataset()`, `modelPipeline()`, and `chainPipeline()` now accept an
  optional exact `features` vector for base predictors. When supplied,
  train/test splits use only those predictor columns plus chain-generated
  prior-iteration columns governed by `ITER_SUFFIX`, and fail clearly if a
  requested predictor is missing or if a response/identifier column is listed
  as a predictor. The default `features = NULL` preserves legacy predictor
  inference.

# ssel 0.1.1

## Bug fixes

* `chainPipeline(resume = TRUE)` now fails before attempting to read metrics
  when the resume state is incomplete. If `.path.iter/convergence.csv` and
  `.path.iter/Y.csv` exist but `.path.summary/metrics.csv` is missing, the
  error names the missing artifact and tells the caller to start a fresh run
  with `resume = FALSE` or restore the summary metrics artifact.
* `trainRegressionModel()` now skips response/dataset cells whose training
  response has no variance before calling `caret::train()`. The warning names
  the affected cell and reports the row count, unique response count, minimum,
  and maximum, avoiding low-level errors such as `invalid number of intervals`
  for unsupported response/dataset intersections.
* `chainPipeline()` now snapshots and stitches all summary artifacts
  consistently in the final per-response best-iteration pass. `metrics.csv`,
  `response_long.csv`, `residuals_oof.csv`, `prediction_quantiles.csv`, and
  `overfit.csv` are all taken from the same selected iteration per response,
  avoiding reports that mixed stitched metrics with last-iteration residuals.

# ssel 0.1.0

Initial release.

## Exported pipeline entry points

* `dataPipeline()` --- assembles per-dataset CSVs from the
  manifest-declared domain CSVs.
* `modelPipeline()` --- single-response weighted-ensemble fit with
  5-fold cross-validation, OOF residuals, signed residual-offset
  summaries, and an in-sample-vs-CV optimism audit.
* `chainPipeline()` --- multi-response iterative refinement with
  two sweep orders (`"jacobi"` and `"gauss-seidel"`), package-defined
  active-set gates, a positional change heuristic, and per-response
  iteration stitching.

## Exported helpers

Dataset assembly, training, prediction, residual audit, and chain
gating helpers: `buildDatasets`, `buildDataset`, `trainModel`,
`trainRegressionModel`, `predictModel`, `aggregateResponses`,
`oofEnsemble`, `auditQuantiles`, `auditOverfit`, `detectOutliers`,
`extractChainImportance`, `activeByImportance`, `activeByShadow`,
`computeActiveByImportance`, `removeOutliersIQR`, `toNumeric`,
`which.nonnum`.

## Documentation

* `vignette("ssel-quickstart", package = "ssel")` --- minimal usage
  template.
* `vignette("ensemble-theory", package = "ssel")` --- single-response
  estimators: weighted-ensemble construction, OOF residual
  reconstruction, signed offsets, optimism diagnostic, and the two
  response/dataset selectors.
* `vignette("chain-regression", package = "ssel")` --- multi-response
  input expansion: row-class-specific Jacobi and asymmetric
  Gauss--Seidel updates, package-defined fixed and shadow gates,
  stopping policy, and per-response iteration stitch.
* The initial release also shipped a non-production semi-supervised design.
  Version 0.3.0 replaced it with the implemented range-ratio promotion policy.

The bibliography for all four vignettes lives in `inst/REFERENCES.bib`.
