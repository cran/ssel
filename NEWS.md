# ssel 0.5.1

## Bug fixes

- The OOF audits report what they could not audit. A model filename that the
  response pattern excludes is still filtered silently, because excluding
  responses is what the pattern is for, but a file the pattern matches that
  cannot be used -- absent TRAIN file, unreadable model, no saved out-of-fold
  predictions, no finite rows, failed prediction, prediction-length mismatch,
  or a missing resample table -- now raises a warning naming the file and the
  reason. `oofEnsemble()` also reports how many response-dataset cells it
  reconstructed when the ensemble rules drop some, and both helpers report an
  audit that produced nothing at all, distinguishing a pattern that matched no
  filename from matches that were all skipped. Products, return values and
  exit status are unchanged, and a complete audit warns about nothing.
- `auditQuantiles()` names what it observed when reconstruction yields no row:
  the model directory, how many `.Rds` files it holds and the response pattern
  in force. It previously reported that no models were found even when they
  were present and the pattern had excluded them.

## Behavior changes

- The warnings and errors that `chainPipeline()` raises while stitching the
  per-response best iterations begin with `chainPipeline:` instead of
  `chainPipeline R5:`, and the heading message of that stage reads
  `Per-response best iteration (stitched final pass)` instead of
  `R5 - per-response best iter (stitched final pass)`.

## Removals

- `pak` is removed from `Suggests`.

## Documentation

- `auditOverfit()` documented that an audit surviving no model creates a
  zero-byte `overfit.csv`. That happens only when no file was there before.
  `data.table::fwrite()` refuses a column-less table, so an existing
  `overfit.csv` survives untouched and still holds the previous run's verdict;
  the manual now states both cases and points at the new warnings as the
  signal that the file is not from this run.

- The repository moved to the kanameishi organization. The `URL` field of
  `DESCRIPTION`, the `ssel-package` help topic, the citation entry and the
  README name its documentation site, <https://kanameishi.github.io/ssel/>.

- The command-line interface article states that `ssel --version` prints
  `ssel <version>`, the version of the package actually loaded. The citation
  block of the README no longer carries a version number; `citation("ssel")`
  reports the installed version.

## Command-line interface

- The `ssel` command shares the package's version: `ssel --version` prints
  `ssel <version>`, and the command requires the installed API facts instead
  of a minimum package version of its own.
- `ssel --version` and the failure context of the command no longer name the
  directory of the installed package or R's library paths.
- `--root DIR` selects an existing project directory. Relative paths resolve
  from it: the specification, its CSV/RDS references, operation arguments and
  `--output`. Without `--root` they resolve from the working directory, as
  before.
- `install/install.sh` and `install/install.ps1` install the package and the
  command together; the `--component` and `-Component` options are removed.
  The generated launchers resolve R when they run instead of reusing the R
  recorded at installation.
- The command-line interface article lists the files each operation reads and
  writes and the directories each workflow clears, and states that `--output`
  saves the function's return after the operation runs without redirecting
  its working directories.

# ssel 0.5.0

## Product changes

- Keys keep leading zeros end to end. Every public entry point that reads
  CSV files (`buildDataset`, `trainModel`, `trainRegressionModel`,
  `predictModel`, the three pipelines, `oofEnsemble`, the two audits,
  `aggregateResponses`, `postProcess`) sets
  `options(datatable.keepLeadingZeros = TRUE)` for the duration of the call,
  restored on exit, and `buildDataset` no longer numeric-coerces the key or
  declared identifier columns. A sample key such as `031` was previously
  delivered as `31` in `Yo.csv` and the id sidecars; it is now delivered as
  `031`. Predictions, metrics and model files are unchanged. Projects whose
  keys are numeric without leading zeros see identical CSV text (the in-memory
  column type becomes character). The registered consumer projects were
  measured unaffected: their keys are alphanumeric and their inputs contain
  no leading-zero cells, so `fread` returns identical data with and without
  the option.

# ssel 0.4.6

## Bug fixes

- Input errors stop at the public boundary naming path, argument and cause.
  `buildDataset` refuses a missing or CSV-less `.path.input` (previously a
  silent no-op) and refuses dataset or response names containing `_`, which
  cannot round-trip through the `<response>_<dataset>_TRAIN.csv` split files
  (previously a downstream `data file not found`). `trainModel` and
  `predictModel` refuse a missing or split-less `.path.train` (previously
  `'x' must be atomic`) and a `.responses` filter that matches no split
  (previously `invalid subscript type 'list'`). Valid projects are
  unaffected.

# ssel 0.4.5

## Command-line interface

- The package installs its API facts (`inst/api/operations.json`): the
  operation register, compatibility aliases, vector/table argument kinds and
  the file-oriented operation set. CLI 0.3.0 reads these facts from the
  installed package instead of carrying its own literals and now requires
  library 0.4.5 or later; without the file, `ssel --help` prints the minimum
  version instead of failing on a missing object. A drift test keeps the
  facts identical to the exported surface. No modeling behavior changes.

# ssel 0.4.4

## Bug fixes

- The response-bound arguments `boundsMin`/`boundsMax` and `.min`/`.max` of the
  eleven modeling, audit and delivery entry points are validated on entry:
  each must be a single non-`NA` numeric value (`Inf` and `-Inf` remain
  allowed and are the defaults). A bound of the wrong type, such as a string
  in a JSON specification, now fails with an error naming the argument instead
  of silently clamping predictions. Valid bounds produce identical products.

## Command-line interface

- CLI 0.3.0: data-returning operations also accept a new directory path as
  `--output`, producing `result.json` (shape and types; `NA` as null; `NaN`,
  `Inf` and `-Inf` as their exact texts) plus one CSV per table, while `.rds`
  keeps the exact native value. `--seed N` seeds the R process before the
  operation, making independent invocations reproducible. `OPERATION --help`
  renders the installed R documentation. The JSON texts `"Inf"`, `"-Inf"` and
  `"NaN"` decode to numeric only where the argument default is numeric.
  Warnings are reported as they happen.

# ssel 0.4.3

## Bug fixes

- `modelPipeline()`, `chainPipeline()` and `semiSupervisedPipeline()` now refuse
  to wipe a declared output directory that coincides with, contains or is inside
  the datasets directory, that contains the working directory, or that coincides
  with another declared path; the error names both paths and nothing is deleted.
  Valid configurations produce identical products.

# ssel 0.4.2

## Bug fixes

- Products, feature hashes and fitting order no longer depend on the session
  collation. Names are ordered by folding ASCII case and sorting `_` and `.`
  first, and `trainRegressionModel()` fits under the C collation so that the
  cross-validation folds `caret` groups by response label are the same on
  every platform; runs on macOS with the default collation are unchanged,
  runs under `LC_ALL=C` now match them.

# ssel 0.4.1

## Bug fixes

- `semiSupervisedPipeline()` builds each final TEST split from the fitted
  TRAIN schema, so multiple target responses no longer cause an extra-key
  failure. Final summaries and exports include the original prediction pool,
  including promoted rows. Fitted models, promotion rules, history and OOF
  diagnostics retain their existing behavior.

## Command-line interface

- CLI 0.2.0 exposes all twenty public functions by their exact R names, with
  `model`, `chain` and `post` aliases retained. JSON and native RDS specifications,
  explicit CSV schemas and RDS results support standalone helpers as well as
  complete pipelines. The CLI manual describes the library boundary and
  executable examples.

# ssel 0.4.0

## Bug fixes

- `removeOutliersIQR()` applies the interquartile envelopes of the inspected
  predictors together instead of one at a time. The loop rebuilt the surviving
  set from the original data on each pass, so only the last inspected column
  ever narrowed it: with an outlier seeded in each of three columns the function
  removed one row of sixty. It now removes all three. A dataset whose ranked
  predictors are all non-numeric returns unchanged rather than failing in a C
  stack overflow, and a missing value in an inspected column no longer decides
  its row, because a value that cannot be compared to an envelope is not
  evidence of an outlier.

## Behavior changes

- Stage 3 prediction skips a cell whose `TEST` split holds no rows, with a
  warning, instead of aborting the run. `buildDataset()` can drop every test row
  of a cell when a predictor lacks coverage, which is routine in chain datasets;
  the run previously stopped on a type mismatch. The Stage 2 metrics and
  out-of-fold predictions of the skipped cell stand, and the cell contributes no
  delivery rows.

- The chain stitch excludes a declared response that produced no iteration-0
  metric row, with a warning, instead of aborting. Such a response has no fitted
  cell in any iteration — every cell was skipped for insufficient support — so
  it has no delivery rows to preserve.

## Removals

- The `resampling` argument and its temporal `timeslice` mode are removed from
  `modelPipeline()`, `chainPipeline()`, `trainModel()` and
  `trainRegressionModel()`, together with the internal `.resolveResampling()`
  helper, its short-series guard, its test file, and its vignette. The argument
  never reached a released version: it was added after 0.3.1 was published, and
  no consumer passed it. Fitting returns to the single k-fold generator it used
  before, and every artifact of a default run is unchanged.

  Time-respecting evaluation belongs to a separate forecast API whose unit is a
  series rather than a row, and which needs a horizon axis that none of the
  present artifacts carry. Threading a fold generator through the supervised
  path could not provide it.

## Documentation

- The help topics of `trainModel()` and `trainRegressionModel()` describe the
  layers of the training API as they behave: which stage owns each directory,
  what each mode reads and writes, and where the ensemble weights come from.
  The ensemble-theory article gains the matching derivation.

- `removeOutliersIQR()` states what it does. The topic previously described
  envelopes that the function did not apply, and did not mention that the
  response column must be named `y` or that the caret formula interface rejects
  missing values before any envelope is computed.

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
