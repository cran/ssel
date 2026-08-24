#' ssel: Semi-Supervised Ensemble Learning
#'
#' Weighted-ensemble regression over caret-supported base learners with
#' cross-validated hyperparameter selection, out-of-fold diagnostics, and
#' signed residual-offset estimates. Multi-response problems use iterative
#' input-space expansion with Jacobi or asymmetric Gauss-Seidel sweeps,
#' package-defined companion gates, and per-response iteration stitching.
#' A package-defined pseudo-label stage promotes prediction rows by a range
#' ratio and accepts rounds with an OOF squared-correlation gauge.
#'
#' @name ssel-package
#' @keywords internal
#' @import data.table
#' @importFrom caret train trainControl varImp
#' @importFrom crayon blue bold green red silver yellow
#' @importFrom dbscan lof
#' @importFrom digest digest
#' @importFrom doParallel registerDoParallel
#' @importFrom earth earth
#' @importFrom foreach %dopar% foreach registerDoSEQ
#' @importFrom gbm gbm
#' @importFrom jsonlite fromJSON
#' @importFrom parallel detectCores makePSOCKcluster stopCluster
#' @importFrom ranger ranger
#' @importFrom stats complete.cases cooks.distance cor lm mad median na.omit predict quantile rnorm sd setNames var
#' @importFrom utils capture.output head tail
"_PACKAGE"

utils::globalVariables(c(
  ".", "..COLS", "..COLS.keep", "..location", "..methods_here", "..responses",
  ".I", ".N", ".SD", ":=", ".outlier", ".is_train", ".row_total",
  "Overall", "SampleID", "Transformed", "b", "chainResp", "dataset",
  "feature", "i.q_50", "i.q_90", "i.q_95", "i.r2", "i.y_hat_oof",
  "importance", "iter", "meanImport", "method", "n", "nActive",
  "normImportance", "obs", "p", "pred", "q_50", "q_90", "q_95",
  "r", "r2", "residual", "response", "rmse", "rmse_k", "rowIndex",
  "set", "shadow", "shadowImportance", "value", "w", "y_hat_oof",
  "D", "action", "i.value", "nm", "spread", "y_obs"
))

#' Build per-response training and prediction files
#'
#' Read assembled dataset CSV files, choose response and predictor columns, and
#' write the per-response files consumed by the model-fitting helpers. Use this
#' low-level helper when the assembled CSVs already exist and their row roles
#' and columns are known.
#'
#' @param .path.input Path to an existing directory. Each non-recursive file
#'   whose name ends in `.csv` is treated as one assembled dataset.
#' @param .path.train Path to an existing writable output directory. The
#'   function does not create it.
#' @param .file.index Reserved for compatibility; currently ignored.
#' @param Y.PATTERN A non-empty regular expression used case-insensitively to
#'   select response column names. Required despite the `NULL` default.
#' @param KEY A single non-empty name of the row-identifier column. Required
#'   despite the `NULL` default.
#' @param COLS.id A character vector of additional identifier columns to
#'   exclude from inferred predictors. `SET_COL` is always excluded as well.
#'   Names absent from a given CSV are silently ignored.
#' @param features `NULL`, or a character vector giving the exact base
#'   predictors in output order. `NULL` infers predictors as every column that
#'   is not a response, `KEY`, or an identifier. Exact names must exist and
#'   cannot name responses or identifiers.
#' @param OUTLIER_REMOVAL A non-missing logical scalar. If `TRUE`,
#'   [detectOutliers()] is applied to each complete training split and rows it
#'   flags are removed.
#' @param SET_COL A single non-empty name of the row-role column. The exact
#'   value `"train"` marks training rows; other non-missing values mark
#'   prediction rows. The column must contain no missing values. This
#'   precondition is not proactively validated. Required despite the `NULL`
#'   default.
#' @param ITER_SUFFIX `NULL`, or a suffix identifying prior-iteration response
#'   columns. Available matching columns are added to an exact `features` set;
#'   each response's own suffixed column is excluded from its split.
#' @param .shadow A non-missing logical scalar. If `TRUE`, add a standard-normal
#'   column named `shadow` using the fixed seed `12345`. It is inferred as a
#'   predictor when `features = NULL`; with exact features it must be named.
#'
#' @section Input CSV schema:
#' The input filename without `.csv` becomes the dataset identifier used in
#' output filenames. Each CSV must contain `KEY`, `SET_COL`, the columns matched
#' by `Y.PATTERN`, and any exact `features`. Response columns are eligible only
#' when numeric after cleaning. Predictor and response units are not converted
#' or scaled; CSV unit metadata is neither read nor written, so callers must
#' supply consistent units.
#'
#' Before splitting, the helper reads `""`, `"N/I"`, `"N/A"`, and `"-"` as
#' missing, removes a column named `*`, removes duplicate rows, and deletes ASCII
#' spaces from every character value. It passes each character column through
#' [toNumeric()] and replaces the whole column only when every non-missing value
#' converts successfully. A partially convertible column remains character.
#'
#' @section Row and predictor filtering:
#' Predictors with at most one distinct non-missing training value are removed.
#' Training rows incomplete in any retained predictor or the current response
#' are omitted. Prediction rows are omitted when a retained predictor is
#' missing or a character predictor has a value not observed in training; one
#' warning reports the number omitted. No imputation or unit conversion is
#' performed.
#'
#' @section Files written:
#' For every numeric response `response` in dataset `dataset`, three CSV files
#' are written:
#'
#' * `response_dataset_TRAIN.csv`: retained predictors followed by the response
#'   renamed `y`;
#' * `response_dataset_TEST.csv`: retained predictors followed by the original
#'   `KEY` column;
#' * `response_dataset_TRAIN_ids.csv`: one column named `SampleID`, containing
#'   the training identifiers aligned with the filtered TRAIN rows.
#'
#' Existing files with those names are overwritten. Other files in
#' `.path.train` are not removed. The function writes no files for a CSV with no
#' numeric matched response, and an input directory with no matching CSV files
#' is a no-op.
#'
#' @section Validation and side effects:
#' Missing required selectors, required columns, or output directories produce
#' errors. Exact `features` also error when they contain missing, empty,
#' duplicated, response, or identifier names. `SET_COL` values must be
#' non-missing, but this is not checked before splitting; a missing row role can
#' therefore produce an indirect downstream error. `COLS.id` names are not
#' validated, and absent names are silently ignored. `KEY` uniqueness and
#' missingness are not checked. Shadow generation restores an existing global
#' random-number state; if no state existed, the call initializes one.
#'
#' @return Invisibly returns `NULL`. The result is the set of CSV files described
#'   in **Files written**.
#' @seealso [toNumeric()] for the supported numeric-string grammar and
#'   [detectOutliers()] for optional training-row filtering.
#' @examples
#' root <- tempfile("ssel-buildDataset-")
#' input_dir <- file.path(root, "assembled")
#' output_dir <- file.path(root, "splits")
#' dir.create(input_dir, recursive = TRUE)
#' dir.create(output_dir)
#'
#' assembled <- data.frame(
#'   SampleID = c("S1", "S2", "P1", "P2"),
#'   set = c("train", "train", "predict", "predict"),
#'   feature_a = c(1, 2, 3, 4),
#'   feature_b = c("1,5", "2,5", "3,5", "4,5"),
#'   target = c(10, 20, NA, NA)
#' )
#' utils::write.csv(assembled, file.path(input_dir, "demo.csv"),
#'                  row.names = FALSE, na = "")
#'
#' buildDataset(
#'   .path.input = input_dir,
#'   .path.train = output_dir,
#'   Y.PATTERN = "^target$",
#'   KEY = "SampleID",
#'   SET_COL = "set"
#' )
#'
#' sort(basename(list.files(output_dir)))
#' utils::read.csv(file.path(output_dir, "target_demo_TRAIN.csv"))
#' utils::read.csv(file.path(output_dir, "target_demo_TEST.csv"))
#'
#' unlink(root, recursive = TRUE)
#' @usage buildDataset(.path.input, .path.train, .file.index = NULL,
#'   Y.PATTERN = NULL, KEY = NULL, COLS.id = character(0),
#'   features = NULL, OUTLIER_REMOVAL = FALSE, SET_COL = NULL,
#'   ITER_SUFFIX = NULL, .shadow = FALSE)
#' @name buildDataset
NULL

#' Fit every discovered response--dataset training cell
#'
#' Training-only wrapper around [trainRegressionModel()]. It discovers split
#' files created by [buildDataset()], constructs the response--dataset grid,
#' and requests every learner under ssel's standard fitting defaults. It does
#' not evaluate fitted models, write metrics, or predict unseen rows.
#'
#' @param .path.train Path to an existing directory containing split files.
#'   Files are discovered non-recursively by the suffix `_TRAIN.csv`.
#' @param .path.model Path to the model-output directory. It is created
#'   non-recursively when absent, so its parent must exist and be writable.
#' @param .path.metrics Path to a metrics directory. It is created when absent
#'   but receives no metric file from this training-only wrapper.
#' @param .path.response Path to a prediction directory. It is created when
#'   absent but receives no prediction file from this training-only wrapper.
#' @param METHODS A character vector of method identifiers accepted by
#'   [caret::train()]. Duplicate identifiers are removed. Their execution order
#'   is randomized for each dataset. An empty vector is not rejected, but no
#'   learner is then fitted.
#' @param PPROC `NULL`, a character vector, or another preprocessing
#'   specification accepted by the `preProcess` argument of [caret::train()].
#' @param OVERRIDE A non-missing logical scalar. If `FALSE`, an existing model
#'   is reused only when its companion feature-name hash exists and matches.
#'   If `TRUE`, every discovered cell is fitted again.
#' @param .min,.max Numeric response bounds, in the response's units. They are
#'   forwarded to the cell engine for API consistency; this wrapper invokes
#'   training mode only, so the bounds do not alter fitting or training data.
#'   Their order and finiteness are not validated.
#' @param .responses `NULL`, or a character vector restricting the discovered
#'   response names. `NULL` fits every discovered response. Requested names
#'   absent from the discovered files are ignored.
#'
#' @section Choosing the API layer:
#' Use [modelPipeline()] when starting from assembled dataset CSVs and the
#' complete supervised workflow is required. Use `trainModel()` when TRAIN
#' split files already exist and the standard five-fold/RMSE fitting policy is
#' appropriate. Call [trainRegressionModel()] with `TRAIN = TRUE` only when the
#' requested response--dataset identifier grid, fold count, tuning granularity,
#' preprocessing, metric, or parallel mode must be controlled directly.
#' Fitted-model evaluation and delivery belong to [predictModel()]; this
#' wrapper performs no evaluation despite receiving metrics and response paths.
#'
#' @section Split-file contract:
#' A training filename must have the form
#' `response_dataset_TRAIN.csv`; suffix matching is case-sensitive. Discovery
#' interprets the first two
#' underscore-separated fields as `response` and `dataset`; those identifiers
#' therefore cannot contain underscores. This restriction is not proactively
#' validated. The parsed responses and datasets are deduplicated separately,
#' then their Cartesian product is attempted. In a sparse set of source files,
#' reconstructed pairs that were not present therefore warn and skip. Each CSV
#' that is found must satisfy the [trainRegressionModel()] training schema:
#' predictor columns followed by, or otherwise including, a numeric response
#' column named `y` with at least two distinct non-missing values.
#' If no filename matches, the output directories are still prepared and the
#' function returns without fitting a model or emitting a warning.
#'
#' Dataset order and the unique learner order are randomized with R's current
#' random-number generator. Call [set.seed()] immediately before this function
#' when repeatable ordering and caret resampling are required. Separate learner
#' fits do not receive a shared caret resampling index, so the function does not
#' promise identical folds across learners.
#'
#' @section Fitting and cache policy:
#' The wrapper fixes five-fold caret cross-validation, `tuneLength = 10`,
#' `metric = "RMSE"`, saved final-tuning holdout predictions, and the
#' `trainRegressionModel()` default parallel policy. `tuneLength` requests a
#' method-specific grid granularity; it is not ten values for every tuning
#' parameter.
#'
#' A successful learner fit writes a caret `train` object to
#' `method_response_dataset.Rds` and a companion `.Rds.hash` text file under
#' `.path.model`. The eight-character hash is derived only from the sorted
#' predictor names. With `OVERRIDE = FALSE`, equal predictor names reuse the
#' model even if response values, predictor values or classes, preprocessing,
#' resampling, tuning, or other fitting arguments changed. Set `OVERRIDE =
#' TRUE` whenever any of those inputs changed.
#'
#' @section Conditions and side effects:
#' A malformed discovered basename can be parsed into a reconstructed cell
#' filename that does not exist; that cell is skipped with a warning. A split
#' with no usable `y`, or with a constant `y`, is also skipped with a warning.
#' A learner-specific caret failure warns and skips that learner; successfully
#' fitted cells continue. The wrapper muffles caret's generic missing-resample-
#' performance warning because the cell engine emits method/cell counts for
#' missing tuning-grid or selected-fold RMSE values after a successful fit.
#' Unused-connection warnings are muffled without a replacement diagnostic.
#'
#' The fitting engine registers a PSOCK backend with at most two workers, then
#' stops it and leaves foreach registered sequentially. Progress is emitted as
#' messages unless `options(aR.quiet = TRUE)` is set. Existing model/hash files
#' may be reused or overwritten as described above; other files are preserved.
#'
#' @return Invisibly returns `NULL`. The fitted `.Rds` and `.Rds.hash` files
#'   are the result; `.path.metrics` and `.path.response` are only prepared for
#'   later evaluation calls.
#' @seealso [buildDataset()] for creating the split files and
#'   [trainRegressionModel()] for the complete cell-engine contract.
#' @references
#' Kuhn, M. (2008). Building predictive models in R using the caret package.
#' *Journal of Statistical Software*, 28(5), 1--26.
#' \doi{10.18637/jss.v028.i05}.
#' See also the caret documentation for [caret::train()] and
#' [caret::trainControl()].
#' @examples
#' \donttest{
#' root <- tempfile("ssel-trainModel-")
#' train_dir <- file.path(root, "train")
#' model_dir <- file.path(root, "model")
#' metrics_dir <- file.path(root, "metrics")
#' response_dir <- file.path(root, "response")
#' dir.create(train_dir, recursive = TRUE)
#'
#' split <- data.frame(
#'   x = seq_len(20),
#'   z = rep(c(0, 1), 10),
#'   y = seq_len(20) / 3 + rep(c(-1, 1), 10)
#' )
#' utils::write.csv(
#'   split,
#'   file.path(train_dir, "target_demo_TRAIN.csv"),
#'   row.names = FALSE
#' )
#'
#' set.seed(2026)
#' suppressMessages(trainModel(
#'   .path.train = train_dir,
#'   .path.model = model_dir,
#'   .path.metrics = metrics_dir,
#'   .path.response = response_dir,
#'   METHODS = "lm"
#' ))
#' sort(basename(list.files(model_dir)))
#' # The result is one caret model and its predictor-name hash.
#'
#' unlink(root, recursive = TRUE)
#' }
#' @usage trainModel(.path.train, .path.model, .path.metrics, .path.response,
#'   METHODS, PPROC = NULL, OVERRIDE = FALSE, .min = -Inf, .max = Inf,
#'   .responses = NULL)
#' @name trainModel
NULL

#' Fit or evaluate a requested response--dataset grid
#'
#' Low-level engine for a requested response--dataset--learner grid. Training
#' mode fits and caches caret learners. Evaluation mode first reconstructs
#' projected out-of-fold (OOF) metrics and weighted ensembles, then predicts
#' matching unseen rows. The higher-level [trainModel()] and [predictModel()]
#' wrappers call these two modes; direct use is intended for grid-level control.
#'
#' @param .path.data Path to an existing directory containing the split CSV
#'   files described in **Split files**.
#' @param .path.model Model directory. Training mode requires it to exist and
#'   be writable for `.Rds` files and their `.Rds.hash` companions. Evaluation
#'   mode requires it to exist and be readable; it reads `.Rds` files and does
#'   not use the hashes. This engine does not create the directory.
#' @param .path.metrics Metrics directory. Training mode does not touch this
#'   path, so it need not exist for a training-only call. Evaluation mode
#'   requires an existing writable directory, overwrites assigned cell-named
#'   CSVs, and reads every `.csv` already present; use a dedicated clean
#'   directory for one run.
#' @param .path.response Prediction directory. Training mode does not touch this
#'   path, so it need not exist for a training-only call. Evaluation mode
#'   requires an existing writable directory and overwrites one cell-named CSV
#'   per cell that reaches prediction. This engine does not create it.
#' @param OVERRIDE A non-missing logical scalar used only in training mode.
#'   `FALSE` reuses a model when both model and hash files exist and the sorted
#'   predictor-name hash matches; `TRUE` fits it again.
#' @param TRAIN A non-missing logical scalar. `TRUE` selects fitting and returns
#'   after model creation. `FALSE` runs both OOF metric reconstruction and
#'   unseen-row prediction; it is not a prediction-only switch.
#' @param PARALLEL A non-missing logical scalar. `TRUE` registers a PSOCK
#'   cluster capped at two logical cores; `FALSE` registers foreach sequential
#'   execution. The backend is left sequential after the call.
#' @param .min,.max Numeric lower and upper response bounds, in the response's
#'   units. In evaluation mode each base OOF and unseen prediction is projected
#'   onto this interval. The function does not validate finiteness or
#'   `.min <= .max`. The bounds are unused in training mode.
#' @param .numberCV A positive integer passed to the `number` argument of
#'   [caret::trainControl()] in training mode. The default is 10.
#' @param .responses A character vector of response identifiers. Despite the
#'   `NULL` default, a non-empty vector is required to process cells.
#' @param .methods A character vector of caret method identifiers. Despite the
#'   `NULL` default, at least one successfully fitted method is required for
#'   evaluation and ensemble output.
#' @param .datasets A character vector of dataset identifiers. Despite the
#'   `NULL` default, a non-empty vector is required to process cells.
#' @param .tuneLength A positive integer passed to [caret::train()] in training
#'   mode. It requests method-specific grid granularity and is not a count per
#'   tuning parameter. The default is 10.
#' @param .preProcess `NULL`, a character vector, or another preprocessing
#'   specification accepted by [caret::train()] in training mode. It is unused
#'   in evaluation mode.
#' @param .metric A single caret performance metric used to select tuning
#'   settings in training mode. The default is `"RMSE"`; it does not change
#'   the fixed metrics written by evaluation mode.
#'
#' @section Choosing the API layer:
#' This function is not a second supervised workflow. [trainModel()] discovers
#' split files and calls `TRAIN = TRUE` with ssel's standard defaults.
#' [predictModel()] discovers the same grid, calls `TRAIN = FALSE`, and then
#' performs response-level selection and delivery assembly.
#' [semiSupervisedPipeline()] also calls `TRAIN = FALSE, PARALLEL = FALSE` for
#' its final serial re-emission. Use this engine directly only when those
#' wrappers do not expose the required identifier grid, resampling, tuning,
#' projection, or execution control.
#'
#' @section Split files:
#' The function expands `.responses`, `.datasets`, and `.methods` as a Cartesian
#' product, in response--dataset--method loop order; it does not accept a sparse
#' list of selected tuples. For each requested `response` and `dataset`, the
#' training file is `response_dataset_TRAIN.csv`. It must contain predictor
#' columns and a numeric column named `y`. Integer columns are promoted to
#' double. A cell is skipped with a warning when the file is absent, `y` is
#' absent, `y` has no non-missing value, or fewer than two distinct non-missing
#' response values remain.
#'
#' Evaluation mode additionally requires
#' `response_dataset_TEST.csv`. It must contain the fitted predictors and
#' exactly one column name absent from the training file; that column is the
#' row key. Zero or multiple such names produce an error. Predictor and
#' response values are not scaled or assigned units, so every file, bound, and
#' interpretation must use consistent response units.
#'
#' @section Training and reproducibility:
#' Each learner is fitted by [caret::train()] with ordinary cross-validation,
#' `savePredictions = "final"`, the requested fold count, preprocessing,
#' tuning granularity, and optimization metric. The function does not supply a
#' common `index` or `indexOut`; separate learner calls therefore do not promise
#' identical resampling folds. There is no seed argument. Call [set.seed()]
#' before the function for reproducibility in a fixed R, caret, learner, and
#' parallel environment.
#'
#' A successful fit writes the caret `train` object to
#' `method_response_dataset.Rds` and writes its eight-character predictor-name
#' hash to `method_response_dataset.Rds.hash`. The hash uses only the sorted
#' predictor names. With `OVERRIDE = FALSE`, an equal hash reuses the model even
#' when data values or classes, response values, preprocessing, resampling,
#' tuning, or metric settings changed. Training failures warn and skip the
#' learner; successful cells continue.
#'
#' @section OOF metrics and ensemble definitions:
#' When `a = .min <= .max = b`, define interval projection by
#' \eqn{\Pi_{[a,b]}(z)=\min\{b,\max\{a,z\}\}}. For method \eqn{m}, evaluation
#' mode merges the saved final-tuning caret holdout predictions, orders them by
#' caret row index, and applies this projection.
#'
#' The first requested method that reaches this scoring step for a cell stores
#' its ordered `obs` vector as the cell's shared reference response. Every later
#' method is scored against that stored vector rather than its own merged
#' `obs`. The function does not verify cross-method equality of observations,
#' row indices, or vector lengths. Consequently, the supplied method order is
#' material when fitted objects contain inconsistent saved OOF rows. In the
#' equations below, \eqn{y_i} denotes this first-method reference vector and
#' \eqn{\hat y_{mi}} the projected OOF prediction for method \eqn{m} and row
#' \eqn{i=1,\ldots,n}. The reported scores are
#' \deqn{\mathrm{RMSE}_m=\left[n^{-1}\sum_i(y_i-\hat y_{mi})^2\right]^{1/2},}
#' \deqn{\mathrm{MAE}_m=n^{-1}\sum_i\left|y_i-\hat y_{mi}\right|,}
#' and squared sample correlation
#' \deqn{R_m^2=\mathrm{cor}(y,\hat y_m)^2.}
#' RMSE and MAE have the response's units; `r2` is dimensionless; `n` is the
#' training-row count; and `p` is the predictor count.
#'
#' For the methods with non-missing scores in a cell, the package defines two OOF
#' weighted means. The R2 ensemble uses
#' \deqn{w_m^{(R2)}=R_m^2/\sum_j R_j^2,}
#' and the RMSE ensemble uses
#' \deqn{w_m^{(RMSE)}=(1/\mathrm{RMSE}_m)/\sum_j(1/\mathrm{RMSE}_j).}
#' Here \eqn{j} ranges over the retained methods. Each ensemble prediction is
#' the corresponding weighted mean \eqn{\sum_m w_m\hat y_m}. These are package
#' weighting policies, not claims of optimal weighting. A zero, missing, or
#' otherwise unusable weight sum can skip an ensemble or the cell. The ensemble
#' rows are named `ensemble.R2` and `ensemble.RMSE`.
#'
#' @section Unseen-row prediction weight state:
#' The OOF pass stores the exact named R2 and RMSE score lists under each
#' response--dataset cell. Before predicting unseen rows for a cell, the
#' function restores that cell's pair of lists. It does not reconstruct these
#' numerical weights from metric CSVs, and a cell with no stored OOF score
#' state has no unseen-row ensemble.
#'
#' Each unseen cell uses the intersection of methods predicted successfully for
#' that cell and the restored score-list names. A missing model or a method
#' returning any missing prediction removes that method only from the current
#' cell's local lists. For the R2 ensemble, the unseen pass substitutes equal
#' weights if a retained R2 is missing or their sum is zero; otherwise it
#' normalizes the retained R2 values. For the RMSE ensemble it first takes
#' reciprocal retained RMSE values, then substitutes equal weights if a
#' reciprocal is missing or their sum is zero; otherwise it normalizes those
#' reciprocals. The formulas and fallbacks match the preceding policy, while
#' score state and pruning remain response--dataset-cell local.
#'
#' Base unseen predictions are projected before weighting.
#'
#' @section Evaluation conditions:
#' During OOF reconstruction, a missing training file, missing model, failed
#' merge of saved holdout predictions, unsupported response, no valid method,
#' invalid R2 or RMSE weight vector, or unusable ensemble prediction emits a
#' warning and skips the affected method, ensemble, or cell according to where
#' it occurs. An invalid R2 vector skips the remainder of that cell before its
#' metrics file is written. A missing test file warns and skips unseen
#' prediction; a missing companion training file is skipped silently. During
#' unseen prediction, a missing model or missing predicted value warns and
#' prunes that method from the current cell, while no common method names warns
#' and skips the cell. A prediction error, `NULL` prediction, row-count mismatch,
#' or invalid key schema stops the call.
#'
#' @section Files written and return value:
#' A cell-named metrics file is written only when the OOF cell reaches the
#' post-ensemble write. The file
#' `.path.metrics/response_dataset.csv` contains every current-call metric row
#' accumulated to that point, not only rows for the cell in its name. Base rows
#' accumulated for a cell that skipped before writing can therefore first
#' appear in a later cell's file. Columns are `method`, `set`, `dataset`,
#' `response`, `rmse`, `mae`, `r2`, `n`, and `p`. Existing assigned files are
#' overwritten.
#'
#' After the OOF pass, every CSV in `.path.metrics` is row-bound to determine
#' which base methods are eligible for unseen-row prediction. Stale or unrelated
#' compatible rows can therefore affect method discovery even though ensemble
#' weights come only from the current call's cell-local score lists. A clean,
#' dedicated metrics directory is required for an isolated evaluation.
#'
#' Each predicted cell writes
#' `.path.response/response_dataset.csv` with one row per unseen row and
#' available base or ensemble method. Columns are the original key name,
#' `value`, `method`, `dataset`, and `response`; `value` has the response's
#' units. Training mode invisibly returns `NULL`. Evaluation mode invisibly
#' returns the cumulative metrics `data.table` assembled during the current
#' call. Progress is emitted as messages; skips and degraded cells emit
#' warnings.
#'
#' @return In training mode, invisibly `NULL`. In evaluation mode, invisibly a
#'   `data.table` with the metric schema described in **Files written and
#'   return value**.
#' @seealso [trainModel()] for split discovery and standard fitting defaults,
#'   [predictModel()] for summary and residual-offset products, and
#'   [auditQuantiles()] for the separate OOF residual-offset estimator.
#' @references
#' Kuhn, M. (2008). Building predictive models in R using the caret package.
#' *Journal of Statistical Software*, 28(5), 1--26.
#' \doi{10.18637/jss.v028.i05}.
#' See also the caret documentation for [caret::train()] and
#' [caret::trainControl()].
#' @examples
#' root <- tempfile("ssel-trainRegression-")
#' data_dir <- file.path(root, "data")
#' model_dir <- file.path(root, "model")
#' metrics_dir <- file.path(root, "metrics")
#' response_dir <- file.path(root, "response")
#' for (path in c(data_dir, model_dir, metrics_dir, response_dir)) {
#'   dir.create(path, recursive = TRUE)
#' }
#'
#' train <- data.frame(
#'   x = seq_len(18),
#'   z = rep(c(0, 1, 2), 6),
#'   y = seq_len(18) / 4 + rep(c(-1, 0.5, 1), 6)
#' )
#' test <- data.frame(
#'   x = c(19, 20),
#'   z = c(0, 1),
#'   SampleID = c("P1", "P2")
#' )
#' utils::write.csv(
#'   train,
#'   file.path(data_dir, "target_demo_TRAIN.csv"),
#'   row.names = FALSE
#' )
#' utils::write.csv(
#'   test,
#'   file.path(data_dir, "target_demo_TEST.csv"),
#'   row.names = FALSE
#' )
#'
#' set.seed(2026)
#' suppressMessages(trainRegressionModel(
#'   .path.data = data_dir,
#'   .path.model = model_dir,
#'   .path.metrics = metrics_dir,
#'   .path.response = response_dir,
#'   TRAIN = TRUE,
#'   PARALLEL = FALSE,
#'   .numberCV = 3,
#'   .responses = "target",
#'   .methods = "lm",
#'   .datasets = "demo",
#'   .tuneLength = 1
#' ))
#'
#' metrics <- suppressMessages(trainRegressionModel(
#'   .path.data = data_dir,
#'   .path.model = model_dir,
#'   .path.metrics = metrics_dir,
#'   .path.response = response_dir,
#'   TRAIN = FALSE,
#'   PARALLEL = FALSE,
#'   .responses = "target",
#'   .methods = "lm",
#'   .datasets = "demo"
#' ))
#' metrics[, .(method, response, dataset, rmse, r2)]
#' utils::read.csv(file.path(response_dir, "target_demo.csv"))
#' # Base and two package-defined ensemble rows are returned per SampleID.
#'
#' unlink(root, recursive = TRUE)
#' @usage trainRegressionModel(.path.data, .path.model, .path.metrics,
#'   .path.response, OVERRIDE = FALSE, TRAIN = TRUE, PARALLEL = TRUE,
#'   .min = -Inf, .max = Inf, .numberCV = 10, .responses = NULL,
#'   .methods = NULL, .datasets = NULL, .tuneLength = 10,
#'   .preProcess = NULL, .metric = "RMSE")
#' @name trainRegressionModel
NULL

#' Evaluate fitted cells and select response-level deliveries
#'
#' Run [trainRegressionModel()] in evaluation mode, collect its point
#' predictions and OOF metrics, select one response--dataset cell per response,
#' and add matching signed OOF-residual offsets. This is the first delivery
#' selector. The later [aggregateResponses()] selector has a different
#' candidate set and grouping rule.
#'
#' @param .path.train Path to an existing split directory. Training files are
#'   discovered non-recursively by the suffix \code{_TRAIN.csv}. Their first two
#'   underscore-separated fields are interpreted as response and dataset, so
#'   those identifiers cannot contain underscores.
#' @param .path.model Existing directory containing the fitted model files
#'   required by [trainRegressionModel()] evaluation mode.
#' @param .path.metrics Existing writable directory for cell metric CSVs.
#'   Evaluation can overwrite assigned files; this helper then reads every CSV
#'   in the directory and overwrites \code{metrics.csv} under
#'   \code{.path.summary} with their unique union.
#' @param .path.response Existing writable directory for cell response CSVs.
#'   Evaluation can overwrite assigned files; matching files are then read to
#'   assemble the long prediction table.
#' @param .path.summary Summary-output directory. It is created
#'   non-recursively when absent, so its parent must exist. A compatible
#'   \code{prediction_quantiles.csv} from [auditQuantiles()] must already exist.
#' @param .path.export Export directory. It is created non-recursively when
#'   absent, so its parent must exist. Assigned dataset CSVs are overwritten.
#' @param .file.index \code{NULL}, a nonexistent path, or a CSV index path.
#'   When an existing file is supplied, its first column is the join key used
#'   to rewrite every CSV under \code{.path.summary} and \code{.path.export}
#'   that contains that name.
#' @param METHODS Non-empty character vector of fitted caret method identifiers.
#'   Duplicates are removed and order is randomized before evaluation.
#' @param PPROC Forwarded as \code{.preProcess} to evaluation mode. The current
#'   [trainRegressionModel()] evaluation path does not use preprocessing.
#' @param probs Numeric vector of offset probabilities. Each value is converted
#'   to an integer percentage with \code{as.integer(100 * p)} and requires a
#'   matching \code{q_XX} column in \code{prediction_quantiles.csv}. The
#'   standard producer writes only 0.50, 0.90, and 0.95.
#' @param .min,.max Numeric response bounds. Base point predictions are
#'   projected by the evaluation engine; each point-plus-offset value is
#'   projected again. Bound order and finiteness are not validated.
#' @param .ensemble A value in \code{c("ensemble.R2", "ensemble.RMSE")}.
#'   It selects both the metric and method label used by the response-level
#'   selector. The default is \code{"ensemble.R2"}.
#' @param .deliverable_datasets \code{NULL}, or a character vector restricting
#'   dataset eligibility only for response-level selection. It does not change
#'   split discovery, evaluation, long outputs, or the list of dataset-named
#'   files considered for export.
#' @param .responses \code{NULL}, or a character vector restricting discovered
#'   responses before evaluation. \code{NULL} uses all parsed responses.
#'
#' @section Evaluation and assembly:
#' Discovered response and dataset names are deduplicated independently and
#' passed to [trainRegressionModel()] with \code{TRAIN = FALSE}. The method
#' vector is sampled with R's current random-number generator, so this call
#' advances RNG state even though fitted models are reused. The engine
#' reconstructs OOF metrics and predicts unseen split rows according to its
#' accepted reference contract. Because this wrapper does not override the
#' engine's \code{PARALLEL} argument, evaluation registers a PSOCK backend with
#' at most two workers, stops it on exit, and leaves foreach sequential. A
#' restricted environment that forbids local server sockets can therefore fail
#' before evaluation even though no model is fitted.
#'
#' Every existing metric CSV is row-bound and deduplicated into
#' \code{DT.metrics}. Every existing
#' \code{response_dataset.csv} under \code{.path.response} is read,
#' deduplicated, and tagged with \code{set = "response_dataset"}. Their union,
#' \code{DTL}, retains base-method and both ensemble point predictions. Missing
#' response files are skipped. If none is read, cleanup of the never-created
#' intermediate can error before a result is written.
#'
#' @section Response-level selector and offsets:
#' Let \eqn{r} index a response, \eqn{d} a dataset cell, \eqn{x} one unseen
#' predictor row, and \eqn{\mathcal D_r} the eligible metric rows for
#' response \eqn{r}. Eligibility includes the requested ensemble label and,
#' when supplied, membership in \code{.deliverable_datasets}.
#' For the RMSE ensemble the selected row is
#' \deqn{d_r^{RMSE}=\operatorname*{first\,argmin}_{d\in\mathcal D_r}
#' \mathrm{RMSE}_{rd},}
#' among rows labeled \code{ensemble.RMSE}. For the R2 ensemble it is
#' \deqn{d_r^{R2}=\operatorname*{first\,argmax}_{d\in\mathcal D_r}R^2_{rd},}
#' among rows labeled \code{ensemble.R2}. Missing scores are ignored when a
#' non-missing score exists, and an extremum tie selects the first metric row.
#' If every score in a response group is missing, or filtering leaves no row
#' for that response, the selector contributes no selected row for it. Later
#' offset assembly and reshaping can consequently omit that response or fail
#' when no usable selection remains.
#'
#' For the selected response and dataset, let \eqn{\hat y_r(x)} be the
#' matching ensemble point prediction and let \eqn{q_{p,rd}} be the signed,
#' cell-pooled OOF-residual offset at requested probability \eqn{p}, written by
#' [auditQuantiles()]. Let \eqn{a} and \eqn{b} be bounds in response units,
#' supplied by \code{.min} and \code{.max}, respectively. When \eqn{a\le b},
#' interval projection is
#' \eqn{\Pi_{[a,b]}(z)=\min\{b,\max\{a,z\}\}} for real \eqn{z}. The delivered
#' value is
#' \deqn{Q_{p,r}(x)=\Pi_{[a,b]}\{\hat y_r(x)+q_{p,rd}\}.}
#' This is a projected point-plus-residual-offset estimate. It is not a
#' predictive-distribution quantile and carries no conditional, marginal, or
#' conformal coverage guarantee. Exactly one matching offset row is required
#' for every selected cell. Rows with any missing field are removed before
#' wide reshaping. Bound order and finiteness are not checked; with invalid
#' bounds the nested minimum/maximum remains the executed operation but is not
#' an interval projection.
#'
#' @section Files, index joins, and return:
#' The helper overwrites \code{metrics.csv} with \code{DT.metrics} and
#' \code{response_long.csv} with the unadjusted long point table \code{DTL}.
#' For each discovered dataset, it finds sample IDs occurring in long-table set
#' names ending in \code{_dataset} and writes a dataset-named CSV when at least
#' one selected wide row has such an ID. Dataset names enter an unescaped
#' regular expression. Each wide file has one row per retained
#' sample/probability, a \code{SampleID} column, one column per selected
#' response, and \code{p}. Existing assigned files are overwritten.
#'
#' If an index file is supplied, every CSV currently present under both output
#' directories is read. Files containing the index's first column are replaced
#' by the index-to-file data.table join, which can add columns, change order, or
#' duplicate rows when the key is not unique. This includes pre-existing CSVs
#' not created by the current call. In-memory return tables precede these joins.
#'
#' Errors from evaluation, absent quantile files or columns, non-unique selected
#' rows, malformed schemas, empty casts, joins, and file operations can occur
#' after earlier assigned files were written; there is no transactional
#' rollback. Progress uses ssel's debug channel.
#'
#' @return Invisibly, a list with \code{DTL}, the unadjusted long point table;
#'   \code{DTW}, the selected wide point-plus-offset table; and
#'   \code{DT.metrics}, the deduplicated union of metric CSVs before any index
#'   rewrite.
#' @seealso [trainRegressionModel()] for evaluation and ensemble point
#'   predictions, [auditQuantiles()] for the offset estimator, and
#'   [aggregateResponses()] for the later RMSE-candidate rowwise selector.
#' @references
#' Hyndman, R. J., and Fan, Y. (1996). Sample quantiles in statistical
#' packages. \emph{The American Statistician}, 50(4), 361--365.
#' \doi{10.1080/00031305.1996.10473566}.
#'
#' Parikh, N., and Boyd, S. (2014). Proximal Algorithms.
#' \emph{Foundations and Trends in Optimization}, 1(3), 127--239.
#' \doi{10.1561/2400000003}.
#' @examples
#' \donttest{
#' root <- tempfile("ssel-predict-")
#' train_dir <- file.path(root, "train")
#' model_dir <- file.path(root, "model")
#' metrics_dir <- file.path(root, "metrics")
#' response_dir <- file.path(root, "response")
#' summary_dir <- file.path(root, "summary")
#' export_dir <- file.path(root, "export")
#' for (path in c(train_dir, model_dir, metrics_dir, response_dir,
#'                summary_dir, export_dir)) {
#'   dir.create(path, recursive = TRUE)
#' }
#'
#' train <- data.frame(
#'   x = 1:6,
#'   y = c(1, 2, 2, 4, 5, 7)
#' )
#' test <- data.frame(x = c(7, 8), SampleID = c("P1", "P2"))
#' utils::write.csv(train, file.path(train_dir, "target_demo_TRAIN.csv"),
#'                  row.names = FALSE)
#' utils::write.csv(test, file.path(train_dir, "target_demo_TEST.csv"),
#'                  row.names = FALSE)
#'
#' fit <- stats::lm(y ~ x, data = train)
#' fit$pred <- data.frame(
#'   intercept = TRUE,
#'   pred = c(1.5, 1.5, 3, 3, 6, 6),
#'   obs = train$y,
#'   rowIndex = 1:6
#' )
#' fit$bestTune <- data.frame(intercept = TRUE)
#' saveRDS(fit, file.path(model_dir, "lm_target_demo.Rds"))
#' data.table::fwrite(
#'   data.table::data.table(
#'     response = "target", dataset = "demo", n = 6L,
#'     q_50 = 0, q_90 = 0.5, q_95 = 1
#'   ),
#'   file.path(summary_dir, "prediction_quantiles.csv")
#' )
#'
#' set.seed(2026)
#' result <- suppressMessages(predictModel(
#'   .path.train = train_dir,
#'   .path.model = model_dir,
#'   .path.metrics = metrics_dir,
#'   .path.response = response_dir,
#'   .path.summary = summary_dir,
#'   .path.export = export_dir,
#'   METHODS = "lm",
#'   .ensemble = "ensemble.RMSE"
#' ))
#' result$DTW
#' # p identifies three signed-residual offsets around one selected cell.
#'
#' unlink(root, recursive = TRUE)
#' }
#' @usage predictModel(.path.train, .path.model, .path.metrics,
#'   .path.response, .path.summary, .path.export, .file.index = NULL,
#'   METHODS, PPROC = NULL, probs = c(0.50, 0.90, 0.95),
#'   .min = -Inf, .max = Inf, .ensemble = "ensemble.R2",
#'   .deliverable_datasets = NULL, .responses = NULL)
#' @name predictModel
NULL

#' Reconstruct a projected OOF ensemble
#'
#' Recover the selected-tuning out-of-fold (OOF) predictions stored in fitted
#' caret models and reconstruct ssel's inverse-RMSE ensemble for every
#' response--dataset cell. Use this helper to inspect training-row ensemble
#' residuals. It recomputes cell-local weights from the saved OOF predictions;
#' it does not reuse the unseen-row weight state described by
#' [trainRegressionModel()].
#'
#' @param .path.model Path to an existing directory containing model files
#'   named \code{method_response_dataset.Rds}. The method identifier must be a
#'   non-empty string without an underscore. Matching files must contain an
#'   object with caret-compatible \code{pred} and \code{bestTune} components.
#' @param .path.train Path to an existing directory containing
#'   \code{response_dataset_TRAIN.csv} and, preferably,
#'   \code{response_dataset_TRAIN_ids.csv} files. A TRAIN file must exist for a
#'   model to be eligible. Its values are read but are not used to replace the
#'   observed responses stored in the model.
#' @param .path.datasets Path to assembled \code{dataset.csv} files used only
#'   by the compatibility mapping when a TRAIN-ID sidecar is absent.
#' @param .min,.max Numeric lower and upper response bounds, in response units.
#'   Predictions are projected with \code{pmax(.min, pmin(.max, z))}. The
#'   function does not validate finiteness or \code{.min <= .max}.
#' @param .response_pattern A non-empty character regular expression inserted
#'   into the model-filename parser to identify responses. Use noncapturing
#'   groups when grouping is needed: additional capturing groups change the
#'   parser's field positions. The default \code{NULL} deliberately fails.
#' @param .sort_keys Character vector of columns that must already define the
#'   assembled-dataset row order when the compatibility ID mapping is used.
#'   An empty vector disables the order check.
#'
#' @section OOF reconstruction:
#' For a matching model, the function inner-joins \code{model$pred} to
#' \code{model$bestTune} by every best-tuning column. It converts \code{pred},
#' \code{obs}, and \code{rowIndex} to numeric or integer values and retains
#' only rows where all three are finite. Predictions are then projected onto
#' the configured bounds. Repeated rows for one method, response, dataset, and
#' row index are reduced by taking separate arithmetic means of \code{obs} and
#' the projected prediction.
#'
#' Let \eqn{\Pi_{[a,b]}(z)=\min\{b,\max\{a,z\}\}} be interval projection, let
#' \eqn{y_{mi}} be the saved observed value, and let
#' \eqn{\tilde y_{mi}} be method \eqn{m}'s averaged projected OOF prediction at
#' row \eqn{i}. The observed value is method-specific at this stage; equality
#' across methods is not checked. In cell \eqn{(r,d)}, the helper computes
#' \deqn{\widetilde{\mathrm{RMSE}}_{mrd}
#' =\left[n_m^{-1}\sum_i(y_{mi}-\tilde y_{mi})^2\right]^{1/2}}
#' and defines the package weights
#' \deqn{\tilde w_{mrd}
#' =\frac{1/\widetilde{\mathrm{RMSE}}_{mrd}}
#' {\sum_j(1/\widetilde{\mathrm{RMSE}}_{jrd})}.}
#' These weights are a package reconstruction policy, not an optimality claim.
#' A cell is skipped when any method-level RMSE is non-finite or zero. A
#' one-method cell is retained with weight one.
#'
#' The wide reconstruction is keyed jointly by \code{rowIndex} and the averaged
#' saved \code{obs}. Let \eqn{h=(i,o)} denote one distinct joint key and let
#' \eqn{A_h} be the methods present at that key. The reconstructed value is
#' \deqn{\hat y_h^{OOF}
#' =\Pi_{[a,b]}\left(
#' \frac{\sum_{m\in A_h}\tilde w_m\tilde y_{mh}}
#' {\sum_{m\in A_h}\tilde w_m}\right).}
#' For a complete key, \eqn{A_h} contains every cell method and the denominator
#' is one. A key with no method prediction is excluded. If methods supply
#' different averaged observed values for the same row index, they create
#' separate keys, each is aggregated with its available methods, and the result
#' can contain repeated \code{rowIndex} and \code{SampleID} values. The
#' function does not validate a common observed response across methods. The
#' returned signed residual is \eqn{e_h=o-\hat y_h^{OOF}}, in response units.
#'
#' @section Sample identifiers:
#' The preferred mapping reads the \code{SampleID} column from
#' \code{response_dataset_TRAIN_ids.csv}; row \eqn{i} supplies the identifier
#' for caret \code{rowIndex = i}. No sidecar length or uniqueness check is made
#' before indexing, but an OOF index beyond its length stops the call.
#'
#' If the sidecar is absent, the compatibility path reads
#' \code{dataset.csv}. That file must contain \code{SampleID}, the response,
#' and every \code{.sort_keys} column. When sort keys are supplied, the file
#' must already equal its ordering by those keys. Identifiers are then taken
#' only from rows whose response is non-missing. This older mapping is safe only
#' when TRAIN rows are exactly those assembled rows, in unchanged order, with
#' no additional removal or reordering. Feature complete-case filtering,
#' outlier removal, or any other extra filter can silently shift identifiers
#' even when the maximum-index check passes; the sidecar is required in those
#' cases. A missing dataset, missing required column, broken ordering invariant,
#' or out-of-range OOF index stops the call.
#'
#' @section Skips, ordering, and side effects:
#' Nonmatching filenames, absent TRAIN files, unreadable models, failed
#' best-tune merges, empty saved predictions, and models with no finite saved
#' row are skipped silently. A cell can also disappear under the RMSE and
#' row-availability rules above. If nothing remains, the function returns an
#' empty zero-column \code{data.table}. Result order is an implementation
#' consequence and is not a sorting guarantee.
#'
#' The helper reads files but writes none, does not fit models, and does not
#' alter the random-number state. Base R, data.table, and file-reading errors
#' not listed above propagate.
#'
#' @return A \code{data.table} with one retained row per reconstructed cell and
#'   distinct \code{(rowIndex, obs)} cast key. Columns are \code{response},
#'   \code{dataset}, integer
#'   \code{rowIndex}, character \code{SampleID}, \code{y_obs},
#'   \code{y_hat_oof}, and \code{residual}. The final three columns have the
#'   response's units. Row indices and sample IDs can repeat when saved
#'   observations disagree across methods. If no row is reconstructed, returns
#'   an empty zero-column table.
#' @seealso [trainRegressionModel()] for producing the fitted caret objects and
#'   saved OOF predictions, [buildDataset()] for TRAIN-ID sidecars, and
#'   [auditQuantiles()] for writing residual-offset summaries.
#' @references
#' Kuhn, M. (2008). Building predictive models in R using the caret package.
#' \emph{Journal of Statistical Software}, 28(5), 1--26.
#' \doi{10.18637/jss.v028.i05}.
#'
#' Parikh, N., and Boyd, S. (2014). Proximal Algorithms.
#' \emph{Foundations and Trends in Optimization}, 1(3), 127--239.
#' \doi{10.1561/2400000003}.
#' @examples
#' root <- tempfile("ssel-oof-")
#' model_dir <- file.path(root, "model")
#' train_dir <- file.path(root, "train")
#' dir.create(model_dir, recursive = TRUE)
#' dir.create(train_dir)
#'
#' data.table::fwrite(
#'   data.table::data.table(y = c(1, 3)),
#'   file.path(train_dir, "target_demo_TRAIN.csv")
#' )
#' data.table::fwrite(
#'   data.table::data.table(SampleID = c("S1", "S2")),
#'   file.path(train_dir, "target_demo_TRAIN_ids.csv")
#' )
#' save_oof <- function(method, prediction) {
#'   fit <- list(
#'     pred = data.frame(
#'       tune = 1L, pred = prediction, obs = c(1, 3), rowIndex = 1:2
#'     ),
#'     bestTune = data.frame(tune = 1L)
#'   )
#'   saveRDS(fit, file.path(model_dir, paste0(method, "_target_demo.Rds")))
#' }
#' save_oof("lower", c(0, 2))
#' save_oof("mixedCase", c(0.5, 2.5))
#'
#' oof <- oofEnsemble(
#'   .path.model = model_dir,
#'   .path.train = train_dir,
#'   .response_pattern = "target"
#' )
#' oof[, .(SampleID, y_obs, y_hat_oof, residual)]
#' # Both learners have RMSE 1 and 0.5, so their weights are 1/3 and 2/3.
#'
#' unlink(root, recursive = TRUE)
#' @usage oofEnsemble(.path.model, .path.train,
#'   .path.datasets = file.path("data", "datasets"), .min = -Inf, .max = Inf,
#'   .response_pattern = NULL, .sort_keys = character(0))
#' @name oofEnsemble
NULL

#' Estimate signed OOF-residual offsets
#'
#' Reconstruct projected OOF ensemble residuals with [oofEnsemble()] and write
#' their cell-pooled sample quantiles. The resulting offsets can be added to a
#' point prediction by [predictModel()]. They are descriptive residual-offset
#' estimates: this helper does not construct a predictive distribution or
#' establish conditional, marginal, conformal, or two-sided interval coverage.
#'
#' @param .path.model,.path.train,.path.datasets,.min,.max,.response_pattern,.sort_keys
#'   Passed unchanged to [oofEnsemble()]. See that reference for the model,
#'   split, identifier, projection, regular-expression, and ordering contracts.
#' @param .path.summary Path to the output directory. It is created recursively
#'   when absent. Assigned CSV files are overwritten.
#' @param verbose A non-missing logical value. If true, invoke debug reporting.
#'   Messages are emitted only when \code{options(aR.verbose = TRUE)} and
#'   \code{options(aR.quiet = FALSE)} are both in effect.
#'
#' @section Estimator:
#' For cell \eqn{(r,d)} and each retained reconstructed cast row
#' \eqn{h=(\mathtt{rowIndex},\mathtt{obs})}, let
#' \eqn{e_{hrd}=y_{hrd}-\hat y_{hrd}^{OOF}} be the signed residual returned by
#' [oofEnsemble()]. A row index can therefore contribute more than once when
#' methods supplied different saved observations; each returned row is a
#' separate quantile observation. For
#' \eqn{p\in\{0.50,0.90,0.95\}}, the helper computes R's type-7 sample
#' quantile. With the \eqn{n} retained residual rows sorted as
#' \eqn{e_{(1)}\le\cdots\le e_{(n)}}, define
#' \eqn{h=(n-1)p+1}, \eqn{j=\lfloor h\rfloor}, and
#' \eqn{\gamma=h-j}; then
#' \deqn{q_{p,rd}=(1-\gamma)e_{(j)}+\gamma e_{(j+1)},}
#' with R's endpoint convention. The values \code{q_50}, \code{q_90}, and
#' \code{q_95} are signed and have the response's units. They are computed
#' separately for every response--dataset cell; no centering, symmetrization,
#' covariate conditioning, or finite-sample calibration is performed.
#'
#' @section Files, partial runs, and return:
#' The helper first writes \code{residuals_oof.csv} with columns
#' \code{response}, \code{dataset}, \code{rowIndex}, \code{SampleID},
#' \code{y_obs}, \code{y_hat_oof}, and \code{residual}. If a compatible file
#' already exists, every prior row whose \code{response} occurs in the current
#' result is removed before the current rows are appended. Replacement is by
#' response, not by response--dataset cell: an omitted dataset for a current
#' response is not retained. Rows for other responses remain.
#'
#' Quantiles are recomputed from that complete merged residual table and
#' \code{prediction_quantiles.csv} is overwritten with columns
#' \code{response}, \code{dataset}, \code{n}, \code{q_50}, \code{q_90}, and
#' \code{q_95}. Thus a partial response run carries prior other-response
#' residuals and their recomputed summaries forward. Existing quantile rows
#' are never merged independently. Row order is not a public sorting
#' guarantee.
#'
#' The return is an invisible list named \code{residuals} and
#' \code{quantiles}, containing exactly the two tables written in the current
#' call. If OOF reconstruction returns no row, the function stops after
#' creating \code{.path.summary} and before writing either assigned CSV.
#' A failure after the residual write can leave that file updated while the
#' quantile file remains from an earlier call or absent. Existing-file schema,
#' file, quantile, data.table, and upstream reconstruction errors propagate.
#' The helper does not fit models or alter the random-number state.
#'
#' @return Invisibly, a named list with \code{residuals}, the merged row-level
#'   OOF table, and \code{quantiles}, the six-column cell summary.
#' @seealso [oofEnsemble()] for the exact OOF estimator and identifier mapping,
#'   and [predictModel()] for the separate point-prediction-plus-offset
#'   delivery step.
#' @references
#' Hyndman, R. J., and Fan, Y. (1996). Sample quantiles in statistical
#' packages. \emph{The American Statistician}, 50(4), 361--365.
#' \doi{10.1080/00031305.1996.10473566}.
#'
#' Barber, R. F., Candes, E. J., Ramdas, A., and Tibshirani, R. J. (2021).
#' Predictive inference with the jackknife+. \emph{The Annals of Statistics},
#' 49(1), 486--507. \doi{10.1214/20-AOS1965}. This reference distinguishes
#' coverage procedures using fold- or leave-one-out-specific test predictions
#' from the descriptive pooled offset computed here.
#' @examples
#' root <- tempfile("ssel-offsets-")
#' model_dir <- file.path(root, "model")
#' train_dir <- file.path(root, "train")
#' summary_dir <- file.path(root, "summary")
#' dir.create(model_dir, recursive = TRUE)
#' dir.create(train_dir)
#'
#' data.table::fwrite(
#'   data.table::data.table(y = c(1, 3)),
#'   file.path(train_dir, "target_demo_TRAIN.csv")
#' )
#' data.table::fwrite(
#'   data.table::data.table(SampleID = c("S1", "S2")),
#'   file.path(train_dir, "target_demo_TRAIN_ids.csv")
#' )
#' fit <- list(
#'   pred = data.frame(
#'     tune = 1L, pred = c(0, 2), obs = c(1, 3), rowIndex = 1:2
#'   ),
#'   bestTune = data.frame(tune = 1L)
#' )
#' saveRDS(fit, file.path(model_dir, "lm_target_demo.Rds"))
#'
#' offsets <- auditQuantiles(
#'   .path.model = model_dir,
#'   .path.train = train_dir,
#'   .path.summary = summary_dir,
#'   .response_pattern = "target",
#'   verbose = FALSE
#' )
#' offsets$quantiles
#' # Both signed residuals equal 1, so all three offsets equal 1.
#'
#' unlink(root, recursive = TRUE)
#' @usage auditQuantiles(.path.model, .path.train, .path.summary,
#'   .path.datasets = file.path("data", "datasets"), .min = -Inf, .max = Inf,
#'   .response_pattern = NULL, .sort_keys = character(0), verbose = TRUE)
#' @name auditQuantiles
NULL

#' Audit package-defined in-sample optimism
#'
#' Compare projected in-sample model error with the resampling summaries stored
#' in fitted caret models, then classify a package-defined relative-optimism
#' ratio. Use this table as a diagnostic for unusually large training-versus-
#' resampling gaps. The ratio and default thresholds are ssel policy; they are
#' not a hypothesis test, a universal overfitting measure, or published
#' cutoffs.
#'
#' @param .path.model Path to an existing directory containing model files
#'   named \code{method_response_dataset.Rds}. The method identifier must be a
#'   non-empty string without an underscore. Matching objects must support
#'   \code{predict(model, newdata = TRAIN)} and contain a non-empty
#'   \code{resample} table with \code{RMSE} and \code{Rsquared} columns.
#' @param .path.train Path to an existing directory containing
#'   \code{response_dataset_TRAIN.csv}. Each eligible file must contain the
#'   fitted predictors and a column named \code{y} coercible to numeric.
#' @param .path.summary Path to the output directory. It is created recursively
#'   when absent; \code{overfit.csv} is overwritten.
#' @param .min,.max Numeric lower and upper response bounds, in response units.
#'   Only in-sample predictions are projected with
#'   \code{pmax(.min, pmin(.max, z))}. Bound order and finiteness are not
#'   validated.
#' @param REL_OPTIMISM_CATASTROPHIC Numeric package-policy threshold for the
#'   \code{"catastrophic"} label. It is tested first and defaults to 0.95.
#' @param REL_OPTIMISM_HIGH Numeric package-policy threshold for the
#'   \code{"high_optimism"} label. It is tested second and defaults to 0.60.
#'   Threshold length, finiteness, range, and ordering are not validated.
#' @param .response_pattern A non-empty character regular expression inserted
#'   into the model-filename parser. Use noncapturing groups when grouping is
#'   needed because additional capturing groups change parser field positions.
#'   The default \code{NULL} deliberately fails.
#' @param verbose A non-missing logical value. If true, invoke debug reporting
#'   for the output path and flag counts. Messages are emitted, and their
#'   arguments evaluated, only when \code{options(aR.verbose = TRUE)} and
#'   \code{options(aR.quiet = FALSE)} are both in effect.
#'
#' @section Diagnostic definition:
#' For an eligible model, let \eqn{y_i} be numeric \code{TRAIN$y} and let
#' \eqn{\hat y_i^{in}} be the generic in-sample prediction projected onto
#' \eqn{[a,b]}. The helper reports
#' \deqn{\mathrm{RMSE}_{in}
#' =\left[n^{-1}\sum_i(y_i-\hat y_i^{in})^2\right]^{1/2}}
#' and, when the sample variance of \eqn{y} is nonzero,
#' \deqn{R^2_{in}=\mathrm{cor}(y,\hat y^{in})^2.}
#' It separately reads each row of \code{model$resample}, reports the mean,
#' sample standard deviation, minimum, and maximum of its \code{RMSE} column,
#' and the mean and sample standard deviation of \code{Rsquared}, using
#' \code{na.rm = TRUE}. The output field \code{k_folds} is exactly the number
#' of rows in \code{model$resample}; its name does not establish that those
#' rows are unique folds rather than another caret resampling design.
#'
#' With \eqn{\overline{\mathrm{RMSE}}_{CV}} equal to that resample mean, ssel
#' defines
#' \deqn{\mathrm{rel\_optimism}
#' =\frac{\overline{\mathrm{RMSE}}_{CV}-\mathrm{RMSE}_{in}}
#' {\max(\overline{\mathrm{RMSE}}_{CV},10^{-9})}.}
#' The unscaled numerator is \code{optimism_rmse}. With finite response bounds,
#' this compares projected in-sample error with caret's stored, unprojected
#' resample RMSE summary; it is not a like-for-like reprojection of both terms.
#'
#' Non-finite relative optimism is labeled \code{"unclassified"}. Otherwise,
#' the catastrophic threshold is tested first, followed by the high threshold;
#' remaining rows are \code{"reliable"}. Negative values are possible. These
#' labels express only the configured package policy.
#'
#' @section Files, skips, and side effects:
#' For every retained model, \code{overfit.csv} contains \code{method},
#' \code{response}, \code{dataset}, \code{n_train}, \code{k_folds},
#' \code{rmse_in}, \code{rmse_cv_mean}, \code{rmse_cv_sd},
#' \code{rmse_cv_min}, \code{rmse_cv_max}, \code{optimism_rmse},
#' \code{rel_optimism}, \code{r2_in}, \code{r2_cv_mean}, \code{r2_cv_sd}, and
#' \code{flag}. RMSE and optimism fields have response units; relative optimism
#' and R-squared fields are dimensionless.
#'
#' Nonmatching filenames, absent TRAIN files, unreadable models, failed
#' predictions, prediction-length mismatches, and missing or empty resample
#' tables are skipped silently. Numeric coercion and summary edge cases retain
#' base R behavior, including missing or infinite fields and warnings. If no
#' model survives, \code{data.table::fwrite()} warns and creates a zero-byte
#' \code{overfit.csv}. With call-level reporting disabled, or with either
#' global debug gate closed, the function then returns a zero-row, zero-column
#' table. When \code{verbose = TRUE}, \code{aR.verbose = TRUE}, and
#' \code{aR.quiet = FALSE}, flag-count evaluation references the absent
#' \code{flag} column and errors after the file is written, so no list is
#' returned. Malformed-column, data.table, and file errors not caught by the
#' helper propagate.
#'
#' The function reads models and training data, writes only
#' \code{overfit.csv}, does not fit models, and neither sets a seed nor draws
#' random values directly. Model-specific prediction methods govern any of
#' their own random-number effects.
#'
#' @return Invisibly, a one-element list named \code{overfit}. Its value is the
#'   table with the schema above. When no model survives, this is an empty
#'   zero-column table unless fully enabled debug evaluation errors first, as
#'   described in **Files, skips, and side effects**.
#' @seealso [trainRegressionModel()] for model and resampling production,
#'   [oofEnsemble()] for row-level OOF reconstruction, and
#'   [auditQuantiles()] for the separate residual-offset summary.
#' @references
#' Kuhn, M. (2008). Building predictive models in R using the caret package.
#' \emph{Journal of Statistical Software}, 28(5), 1--26.
#' \doi{10.18637/jss.v028.i05}.
#'
#' Hastie, T., Tibshirani, R., and Friedman, J. (2009).
#' \emph{The Elements of Statistical Learning}, second edition, Chapter 7.
#' Springer. \doi{10.1007/978-0-387-84858-7}.
#' @examples
#' root <- tempfile("ssel-overfit-")
#' model_dir <- file.path(root, "model")
#' train_dir <- file.path(root, "train")
#' summary_dir <- file.path(root, "summary")
#' dir.create(model_dir, recursive = TRUE)
#' dir.create(train_dir)
#'
#' train <- data.frame(
#'   x = seq_len(8),
#'   y = c(1, 2, 2, 4, 5, 5, 7, 8)
#' )
#' fit <- stats::lm(y ~ x, data = train)
#' fit$resample <- data.frame(
#'   RMSE = c(0.2, 0.3),
#'   Rsquared = c(0.8, 0.9)
#' )
#' saveRDS(fit, file.path(model_dir, "lm_target_demo.Rds"))
#' data.table::fwrite(train, file.path(train_dir, "target_demo_TRAIN.csv"))
#'
#' diagnostic <- auditOverfit(
#'   .path.model = model_dir,
#'   .path.train = train_dir,
#'   .path.summary = summary_dir,
#'   .response_pattern = "target",
#'   verbose = FALSE
#' )
#' diagnostic$overfit[, .(
#'   method, rmse_in, rmse_cv_mean, rel_optimism, flag
#' )]
#' # The label follows ssel's configured ratio thresholds.
#'
#' unlink(root, recursive = TRUE)
#' @usage auditOverfit(.path.model, .path.train, .path.summary,
#'   .min = -Inf, .max = Inf, REL_OPTIMISM_CATASTROPHIC = 0.95,
#'   REL_OPTIMISM_HIGH = 0.60,
#'   .response_pattern = NULL, verbose = TRUE)
#' @name auditOverfit
NULL

#' Extract normalized importance for response companions
#'
#' Read fitted model files, ask [caret::varImp()] for each model's importance
#' table, normalize scores within that table, and retain features ending in a
#' configured companion suffix. This helper produces the table consumed by the
#' fixed and shadow chain gates; it does not itself select active responses.
#'
#' @param pathModel Directory scanned non-recursively for case-sensitive
#'   filenames ending in \code{.Rds}.
#' @param suffix Character suffix matched with [endsWith()] against every
#'   importance feature name. Length, missingness, emptiness, and collisions are
#'   not proactively validated.
#' @param useModel Value passed unchanged to
#'   \code{caret::varImp(model, useModel = useModel)}. \code{TRUE} requests
#'   caret's model-specific path and is used by the fixed gate. \code{FALSE}
#'   requests caret's model-independent regression path and is used by the
#'   shadow gate. The helper does not validate a logical scalar.
#'
#' @section File parser and skips:
#' A model basename is split on underscores after removing its extension. The
#' first field becomes \code{method}, the last becomes \code{dataset}, and all
#' interior fields joined by underscores become \code{response}. Method and
#' dataset identifiers therefore cannot be reconstructed when they contain
#' underscores; a response may contain them. A valid basename must have at
#' least three fields: method, one or more response fields, and dataset. This
#' precondition is not validated. With two fields, for example
#' \code{lm_demo.Rds}, the index sequence reverses and the response becomes
#' \code{demo_lm}; with one field, \code{lm.Rds}, it becomes \code{NA_lm}
#' while both method and dataset are \code{lm}. Such malformed stems can
#' therefore contribute spurious response labels rather than being rejected.
#' Files are processed in [list.files()] order.
#'
#' An unreadable model, a caught [caret::varImp()] error, an empty importance
#' table, or a table with no suffix-matching feature contributes no row. The
#' returned caret table is expected to become columns \code{rn} and
#' \code{Overall}; incompatible schemas fail at column renaming rather than
#' being skipped. Apart from the unvalidated short-stem behavior above, other
#' type, filesystem, and data.table errors propagate.
#'
#' @section Normalization and shadow field:
#' For model file \eqn{c}, let \eqn{I_{fc}} be caret's \code{Overall} score for
#' feature \eqn{f}, and let
#' \deqn{T_c=\sum_f I_{fc},}
#' removing missing scores. When \eqn{T_c>0}, ssel writes
#' \deqn{J_{fc}=100 I_{fc}/T_c}
#' rounded to four decimals; otherwise it writes zero for every row. The sum is
#' over all importance rows before companion filtering, so retained companion
#' percentages need not sum to 100. Negative, infinite, duplicated, or
#' otherwise unusual caret scores are not range-checked.
#'
#' The exact feature name \code{shadow}, when it occurs once, supplies that
#' model's normalized \code{shadowImportance} on the same row-local scale. Zero
#' or multiple exact matches yield a missing shadow score. Only features whose
#' names end in \code{suffix} are returned. Their names are not stripped here.
#'
#' Model-specific importance definitions can differ across learners. This
#' normalization is a package aggregation convention and does not establish
#' cross-learner comparability. Likewise, the model-independent/shadow path is
#' one package null-reference input; it is not the Boruta algorithm.
#'
#' @section Side effects and return:
#' The helper reads model files and calls their caret importance methods. It
#' writes no file, sets no seed, draws no random value directly, and emits no
#' deliberate progress condition. Caught model/importance failures are silent.
#'
#' @return A \code{data.table} with one row per retained model--companion pair
#'   and columns \code{feature}, \code{response}, \code{dataset},
#'   \code{method}, \code{normImportance}, and \code{shadowImportance}.
#'   Importance columns are in normalized percentage points. If no row is
#'   retained, returns an empty zero-column \code{data.table}.
#' @seealso [activeByImportance()] for the fixed table gate,
#'   [activeByShadow()] for the strict shadow-majority table gate, and
#'   [computeActiveByImportance()] for fixed-gate composition.
#' @examples
#' root <- tempfile("ssel-chain-importance-")
#' dir.create(root)
#' train <- data.frame(
#'   alpha.o = 1:12,
#'   beta.o = rep(c(0, 1), 6),
#'   x = rep(1:4, 3),
#'   y = c(2.1, 1.7, 3.5, 2.8, 5.2, 4.1,
#'         6.4, 5.3, 7.1, 6.8, 8.2, 7.5)
#' )
#' fit <- caret::train(
#'   y ~ ., data = train, method = "lm",
#'   trControl = caret::trainControl(method = "none")
#' )
#' saveRDS(fit, file.path(root, "lm_target_demo.Rds"))
#'
#' importance <- extractChainImportance(root, suffix = ".o")
#' importance[, .(feature, response, dataset, normImportance)]
#' # Percentages use all model features in their denominator.
#'
#' unlink(root, recursive = TRUE)
#' @usage extractChainImportance(pathModel, suffix = ".o", useModel = TRUE)
#' @name extractChainImportance
NULL

#' Select chain responses by mean normalized importance
#'
#' Select chain-feature responses whose mean supplied importance reaches a
#' package-defined floor. Use this table-only gate when normalized importance
#' scores have already been assembled and a fixed percentage threshold is the
#' desired policy.
#'
#' @param importanceDT A `data.table` with one row per importance observation
#'   and required columns `feature` and `normImportance`. `feature` must contain
#'   non-missing character names ending in `suffix`; `normImportance` must be
#'   numeric and is interpreted in normalized-importance percentage points.
#'   Extra columns are ignored. Values are not normalized or range-checked.
#' @param suffix A single non-missing, non-empty character suffix to remove from
#'   every `feature`, default `".o"`. Removal is positional: the function does
#'   not verify that names end in this suffix.
#' @param thresh A single numeric threshold in normalized-importance percentage
#'   points, default `0`. The comparison is inclusive. The function does not
#'   validate its length, whether it is finite, or its range; `NA` selects no
#'   response.
#'
#' @section Gate definition:
#' For each response name obtained by removing the final `nchar(suffix)`
#' characters from `feature`, the function computes the arithmetic mean of all
#' non-missing `normImportance` values. Every input row contributes separately,
#' including duplicate rows. A response is returned when its mean is greater
#' than or equal to `thresh`; an exact threshold tie therefore passes. A group
#' with only missing scores has a `NaN` mean and is excluded.
#'
#' The threshold and averaging rule are ssel policy. Averaging is mechanical:
#' the function neither establishes nor checks that scores from different
#' learners or other sources are comparable. Output order follows the first
#' appearance of each stripped response name, and grouping removes duplicate
#' names from the result.
#'
#' @section Validation and side effects:
#' `NULL` and a zero-row table return `character(0)`. A non-empty input must be
#' a `data.table`; missing required columns or incompatible types produce
#' underlying base R or data.table errors or warnings rather than a deliberate
#' validation message. The input is copied before the temporary response column
#' is added, so it is not modified by reference. The function reads and writes
#' no files, uses no random numbers, and emits no message or warning for valid
#' inputs.
#'
#' @return A character vector of unique stripped response names in
#'   first-appearance order. It is empty when no group reaches `thresh`.
#' @seealso [activeByShadow()] for the strict-majority shadow alternative.
#'   [extractChainImportance()] is the package table producer, and
#'   [computeActiveByImportance()] composes extraction with this gate; their
#'   model-file and stability contracts are separate from this table operation.
#' @examples
#' importance <- data.table::data.table(
#'   feature = c("beta.o", "alpha.o", "beta.o", "alpha.o", "gamma.o"),
#'   normImportance = c(1, 2, 3, NA, NA)
#' )
#'
#' # beta ties the 2% floor; alpha ignores one NA; gamma has no usable score.
#' activeByImportance(importance, suffix = ".o", thresh = 2)
#' @usage activeByImportance(importanceDT, suffix = ".o", thresh = 0)
#' @name activeByImportance
NULL

#' Select chain responses by a shadow comparison
#'
#' Select chain-feature responses whose supplied importance is strictly greater
#' than a same-row shadow importance in more than half of their rows. Use this
#' table-only gate when each row contains the feature and shadow scores to be
#' compared directly.
#'
#' @param IMP A `data.table` with one row per comparison cell and required
#'   columns `feature`, `normImportance`, and `shadowImportance`. `feature` must
#'   contain non-missing character names ending in `suffix`; both importance
#'   columns must be numeric on the same normalized-percentage scale. Extra
#'   columns are ignored. Values and row uniqueness are not validated.
#' @param suffix A single non-missing, non-empty character suffix to remove from
#'   every `feature`, default `".o"`. Removal is positional: the function does
#'   not verify that names end in this suffix.
#'
#' @section Gate definition:
#' Each input row is one comparison cell; method, response, and dataset columns
#' are not inspected. A row is a success only when `shadowImportance` is not
#' missing and `normImportance > shadowImportance`. Equality is not a success.
#' For each stripped response name, the denominator is the full row count,
#' including duplicate rows and rows with a missing shadow score. The response
#' passes only when successes are strictly greater than half of that count, so
#' a tied half does not pass.
#'
#' A missing `shadowImportance` counts as an unsuccessful row and remains in the
#' denominator. A missing `normImportance` paired with a present shadow makes
#' the group's success count `NA`, so that group is excluded. The helper assumes
#' that the two score columns are comparable within each row; it does not
#' establish comparability across learners or sources.
#'
#' This strict-majority rule is ssel package policy. It uses a supplied shadow
#' score as a reference but does not implement the iterative random-probe tests
#' of the Boruta algorithm.
#'
#' @section Validation and side effects:
#' `NULL`, a zero-row table, or an input without `shadowImportance` returns
#' `character(0)`. Otherwise a non-empty input must be a `data.table`; missing
#' `feature` or `normImportance` columns and incompatible types produce
#' underlying base R or data.table errors or warnings. The function does not
#' modify `IMP` by reference. It reads and writes no files, uses no random
#' numbers, and emits no message or warning for valid inputs.
#'
#' @return A character vector of unique stripped response names in
#'   first-appearance order. It is empty when no response has a strict majority.
#' @seealso [activeByImportance()] for the inclusive fixed-threshold gate.
#'   [extractChainImportance()] is the package table producer; its model-file
#'   contract is separate from this table operation.
#' @references
#' Kursa, M. B. and Rudnicki, W. R. (2010). Feature Selection with the Boruta
#' Package. *Journal of Statistical Software*, 36(11), 1--13.
#' \doi{10.18637/jss.v036.i11}.
#' @examples
#' importance <- data.table::data.table(
#'   feature = c("alpha.o", "alpha.o", "alpha.o",
#'               "beta.o", "beta.o", "gamma.o", "gamma.o"),
#'   normImportance = c(2, 3, 0, 2, 0, 2, 2),
#'   shadowImportance = c(1, 1, 1, 1, 1, 1, NA)
#' )
#'
#' # alpha wins 2/3 cells; beta ties 1/2; gamma's missing shadow counts.
#' activeByShadow(importance, suffix = ".o")
#' @usage activeByShadow(IMP, suffix = ".o")
#' @name activeByShadow
NULL

#' Extract, threshold, and intersect fixed-gate chain responses
#'
#' Compose model-specific companion-importance extraction with
#' [activeByImportance()] and an optional intersection over supplied preceding
#' active sets. This convenience wrapper implements only the fixed threshold
#' policy. [chainPipeline()] separately applies its fallback and common history
#' policy around this result.
#'
#' @param pathModel Directory passed to [extractChainImportance()].
#' @param suffix Companion suffix passed to extraction and positional response
#'   name removal in [activeByImportance()].
#' @param thresh Numeric inclusive mean normalized-importance threshold in
#'   percentage points. It has no default and is forwarded without validation.
#' @param stableK Value compared with 1 and used as the number of current plus
#'   supplied preceding sets to intersect. It is not coerced or validated here.
#' @param history List-like object whose final \code{stableK - 1} elements are
#'   supplied to [intersect()] after the current raw set. The function does not
#'   establish whether these are raw or previously stabilized sets.
#'
#' @section Composition rule:
#' Let \eqn{G_0} be the character vector returned by
#' \code{activeByImportance(extractChainImportance(...), thresh = thresh)}.
#' Extraction always uses its default \code{useModel = TRUE}. If
#' \code{stableK <= 1}, or fewer than \code{stableK - 1} history elements are
#' available, the helper returns \eqn{G_0} unchanged. Otherwise, for the
#' retained history tail \eqn{G_1,\ldots,G_{K-1}}, it returns
#' \deqn{G_0\cap G_1\cap\cdots\cap G_{K-1}.}
#' Base [intersect()] preserves the current vector's order while removing
#' duplicate names. No fallback replaces an empty extraction or intersection.
#'
#' In [chainPipeline()], this wrapper is called with \code{stableK = 1} and an
#' empty history. The pipeline first replaces an empty raw result with all
#' declared responses, then applies its own window by intersecting the current
#' set with preceding returned active sets. Consequently, standalone wrapper
#' output and pipeline fallback/stability must not be described as one rule.
#'
#' @section Conditions and side effects:
#' Validation and failures are inherited from extraction, the fixed table
#' gate, [tail()], [Reduce()], and [intersect()]. In particular, a missing
#' \code{stableK} comparison can error, and unusual history element types retain
#' base R behavior. The helper reads models through the extractor but writes no
#' file, sets no seed, draws no random value directly, and emits no deliberate
#' condition.
#'
#' @return A character vector of stripped response names, in current fixed-gate
#'   order, that survive the optional intersections. It can be empty.
#' @seealso [extractChainImportance()] for model parsing and normalization,
#'   [activeByImportance()] for threshold semantics, and [chainPipeline()] for
#'   pipeline fallback and the common fixed/shadow history policy.
#' @examples
#' root <- tempfile("ssel-chain-stability-")
#' dir.create(root)
#' train <- data.frame(
#'   alpha.o = 1:12,
#'   beta.o = rep(c(0, 1), 6),
#'   x = rep(1:4, 3),
#'   y = c(2.1, 1.7, 3.5, 2.8, 5.2, 4.1,
#'         6.4, 5.3, 7.1, 6.8, 8.2, 7.5)
#' )
#' fit <- caret::train(
#'   y ~ ., data = train, method = "lm",
#'   trControl = caret::trainControl(method = "none")
#' )
#' saveRDS(fit, file.path(root, "lm_target_demo.Rds"))
#'
#' computeActiveByImportance(
#'   pathModel = root,
#'   suffix = ".o",
#'   thresh = 0,
#'   stableK = 2,
#'   history = list("alpha")
#' )
#' # Only a currently active name also present in the supplied set survives.
#'
#' unlink(root, recursive = TRUE)
#' @usage computeActiveByImportance(pathModel, suffix = ".o", thresh, stableK,
#'   history)
#' @name computeActiveByImportance
NULL

#' Flag regression training outliers
#'
#' Adds robust response, local-outlier-factor, and optional Cook's-distance
#' scores to a regression training table.
#'
#' @param DT Training `data.table` containing numeric response column `y`.
#' @param threshold.mad Modified Z-score threshold.
#' @param threshold.lof Local outlier factor threshold.
#' @param threshold.cooks Cook's distance threshold, or `Inf` for default rule.
#' @param minPts Minimum points for local outlier factor.
#' @param use.cooks Logical; include Cook's distance.
#' @return A copy of `DT` with `.score.mad`, `.score.lof`, and `.outlier`
#'   columns; `.score.cooks` is added when requested.
#' @usage detectOutliers(DT, threshold.mad = 3.5, threshold.lof = 2.0,
#'   threshold.cooks = Inf, minPts = 5, use.cooks = FALSE)
#' @name detectOutliers
NULL

#' Remove rows outside feature IQR envelopes
#'
#' Fit a ranger model to rank the predictors by impurity importance, then drop
#' every row that falls outside the interquartile-range envelope of any of the
#' most important numeric ones. The envelopes apply together: a row survives
#' only when it is inside all of them.
#'
#' @param .data Training data containing a numeric response column named `y`.
#'   The response name is the package's split contract: [buildDataset()] renames
#'   each response to `y` when it writes the training splits.
#' @param n Number of top-ranked predictors to inspect. Non-numeric predictors
#'   among them are ignored; when none of the `n` is numeric the data is
#'   returned unchanged.
#' @param q Multiplier of the interquartile range. The envelope of a column runs
#'   from its first quartile minus `q` times its interquartile range to its third
#'   quartile plus the same amount, so a larger `q` keeps more rows. At the
#'   default of 3 an approximately normal column loses no row.
#' @return A `data.table` holding the surviving rows and every original column.
#'
#' @section Missing values:
#' The ranger fit runs through caret's formula interface, whose default
#' `na.action` is `na.fail`, so a missing value anywhere in `.data` stops the
#' call before any envelope is computed. Resolve missing values before calling.
#' A missing value in a column being inspected would not by itself remove its
#' row: this function removes outliers, not incomplete rows.
#'
#' @seealso [detectOutliers()], which scores rows instead of removing them and
#'   is the routine [buildDataset()] applies when outlier removal is enabled.
#' @usage removeOutliersIQR(.data, n = 3, q = 3)
#' @name removeOutliersIQR
NULL

#' Locate values outside base R's numeric grammar
#'
#' Return the positions of present values that become missing when converted by
#' `as.numeric(as.character(x))`. Use this diagnostic to find labels, unit
#' suffixes, localized separators, or other text that base R cannot parse as a
#' number.
#'
#' @param x An atomic vector or factor. Factor levels are tested through their
#'   labels. The result has the same positional indexing as `x`.
#'
#' @details
#' Conversion warnings are suppressed. Original missing values, including a
#' numeric `NaN`, are excluded from the result. A present string such as
#' `"NA"` or `"NaN"` is reported because its conversion is missing while the
#' original element is not. Finite numbers and `Inf` are accepted by base R.
#'
#' This helper does not apply [toNumeric()] first. Consequently, `"1,25"` is
#' reported even though `toNumeric("1,25")` returns `1.25`. Values containing
#' units or currency symbols are also reported; neither units nor locale are
#' inferred.
#'
#' @return An integer vector of ascending one-based positions. Returns
#'   `integer(0)` when every non-missing value is accepted.
#' @seealso [toNumeric()] to normalize the package's supported separator
#'   formats before numeric conversion.
#' @examples
#' x <- c("2.5", "1,25", "12 kg", NA, "Inf")
#' which.nonnum(x)
#' x[which.nonnum(x)]
#' @usage which.nonnum(x)
#' @name which.nonnum
NULL

#' Convert supported numeric strings to numeric values
#'
#' Normalize the package's supported decimal and grouping separators, then
#' coerce values to numeric. Use this helper for simple character columns whose
#' separator convention is known; it is not a general locale or units parser.
#'
#' @param x An atomic vector or factor. Values are processed as character text
#'   and returned as a plain numeric vector of the same length.
#'
#' @section Supported grammar:
#' ASCII spaces are removed first. A value containing two or more period-plus-
#' digits groups has every period removed; the analogous rule removes every
#' comma from a value containing two or more comma-plus-digits groups. After
#' those rules, one comma in a complete string of the form
#' `-?[0-9]+,[0-9]+` is changed to a decimal point. Base R numeric conversion
#' handles the remaining text, including ordinary point decimals, scientific
#' notation, and `Inf`.
#'
#' Group widths are not validated. Thus `"1.234.567"` and `"1,234,567"`
#' become `1234567`, while `"1,2,3"` becomes `123`. A single separator is
#' interpreted as decimal: both `"1.234"` and `"1,234"` become `1.234`.
#'
#' @section Missing, ambiguous, and unit-bearing values:
#' Missing inputs remain missing. Conversion warnings are suppressed and text
#' outside the supported grammar becomes `NA_real_`; this includes mixed
#' separators such as `"1,234.56"`, currency symbols, and unit suffixes.
#' Numbers are not scaled and units are not converted or preserved as metadata.
#' Resolve ambiguous separators and harmonize units before calling this helper.
#'
#' @return A numeric vector with one output per input value. Failed conversions
#'   are `NA_real_`; numeric `NaN` remains a missing numeric value.
#' @seealso [which.nonnum()] for positions that fail direct base R numeric
#'   conversion. [buildDataset()] applies `toNumeric()` to character columns and
#'   replaces a column only when all of its non-missing values convert.
#' @examples
#' x <- c("1 234", "1.234.567", "1,25", "1,234", "1,234.56", NA)
#' data.frame(input = x, numeric = toNumeric(x))
#' @usage toNumeric(x)
#' @name toNumeric
NULL
