# nolint start
# modelPipeline: full supervised pipeline (splits + train + predict + audit)
# All parameters are explicit. The runner script (project-side) is the
# single source of truth for runtime config; ssel does not read a manifest.

#' Orchestrate the supervised ssel workflow
#'
#' Compose split construction, fitting, evaluation, residual-offset and
#' optimism audits, response-level selection, and final aggregation. This
#' function is filesystem orchestration around public helpers; it is not a
#' separate statistical estimator and it does not read a project manifest.
#'
#' @param .path.datasets Existing directory of assembled CSV files. Every
#'   non-recursive filename ending in \code{.csv} is processed by
#'   [buildDataset()], whether or not its basename occurs in \code{datasets}.
#' @param .path.train,.path.model,.path.metrics,.path.response,.path.summary,.path.export
#'   Output directories for splits, fitted models, cell metrics, cell response
#'   predictions, summary products, and final exports. Missing directories are
#'   created recursively after any configured wipe. See **Wipe and cache
#'   policy** for destructive behavior.
#' @param .path.iter \code{NULL}, or an iteration directory. When supplied,
#'   [aggregateResponses()] writes or updates \code{Y.csv} there for a later
#'   chain iteration.
#' @param key A non-empty character key name passed to [buildDataset()] and
#'   [aggregateResponses()]. Final response aggregation currently requires a
#'   column named \code{SampleID} independently of this argument.
#' @param responses Non-empty character vector of assembled response-column
#'   names. Names are inserted unescaped into regular expressions used for
#'   split and model discovery.
#' @param datasets Non-empty character vector of assembled dataset basenames
#'   used only by final [aggregateResponses()] feature extraction. Among these
#'   declared files, the last one that exists supplies \code{X.csv} and
#'   \code{Xo.csv}. This vector does not restrict upstream split construction,
#'   fitting, OOF reconstruction, or prediction.
#' @param colsId Character vector of additional identifier columns excluded
#'   from inferred predictors.
#' @param features \code{NULL}, or exact base predictor names passed to
#'   [buildDataset()]. \code{NULL} delegates predictor inference to that helper.
#' @param setCol Non-empty character name of the assembled row-role column,
#'   expected to distinguish \code{"train"} and \code{"predict"}.
#' @param methods Character vector of fitted caret method identifiers. The
#'   default is \code{c("ranger", "earth", "gbm")}.
#' @param seed \code{NULL}, or a value passed to [set.seed()] before split
#'   construction in every mode. It controls fitting/resampling and other
#'   downstream random operations only to the extent documented by each helper.
#' @param ensemble Forwarded without initial validation to [predictModel()].
#'   That helper later requires one of \code{"ensemble.RMSE"} or
#'   \code{"ensemble.R2"} for its response-level selector. Final
#'   [aggregateResponses()] output nevertheless always uses
#'   \code{ensemble.RMSE} candidates.
#' @param boundsMin,boundsMax Numeric response bounds forwarded through OOF,
#'   point-prediction, audit, and aggregation paths. Each must be a single
#'   non-\code{NA} numeric value; \code{Inf} and \code{-Inf} are allowed and
#'   are the defaults. Their order and finiteness are not validated.
#' @param iterSuffix \code{NULL}, or the suffix identifying prior-iteration
#'   response features in [buildDataset()], commonly \code{".o"}.
#' @param mode A value in \code{c("full", "train", "predict")}.
#' @param responsesActive \code{NULL}, or a value used without subset or type
#'   validation to filter fitting, OOF/audit reconstruction, and prediction.
#'   \code{NULL} uses \code{responses}. All \code{responses} are still split
#'   and passed to final aggregation. See **Partial-response state**.
#' @param wipe \code{NULL}, or any value. \code{NULL} and the literal logical
#'   scalar \code{TRUE} enable wiping; every other value disables it through
#'   [isTRUE()]. See **Wipe and cache policy**.
#' @param shadow Logical value passed to [buildDataset()] to add its Gaussian
#'   shadow predictor.
#'
#' @section Mode composition:
#' Initial validation checks the type/non-emptiness conditions shown for
#' \code{key}, \code{responses}, \code{datasets}, \code{setCol}, and
#' non-null \code{features}, plus membership of \code{mode}. It does not
#' validate \code{responsesActive}, \code{methods}, \code{ensemble},
#' \code{wipe}, \code{shadow}, or the response bounds. A non-null seed is
#' passed directly to [set.seed()]. Other conditions are delegated to the
#' called helpers and can therefore fail after destructive or expensive work.
#'
#' Every mode prepares directories and calls [buildDataset()], which scans
#' every CSV in \code{.path.datasets}. The response-column expression is the
#' unescaped anchored alternation \code{^(r1|r2|...)$}, applied
#' case-insensitively. Fitting and prediction then discover dataset identifiers
#' independently from the generated split filenames. Active response filtering
#' of those names is exact and case-sensitive; the OOF and overfit model-file
#' parsers receive an unescaped, unanchored active-response alternation inside
#' their case-sensitive regular expressions.
#'
#' \itemize{
#' \item \code{"train"} then calls [trainModel()] for active responses and
#' stops. No OOF audit, prediction, overfit audit, or aggregation runs.
#' \item \code{"predict"} skips fitting, then calls [auditQuantiles()],
#' [predictModel()], [auditOverfit()], and [aggregateResponses()] in that order.
#' Existing compatible models are therefore required.
#' \item \code{"full"} performs the training call followed by the complete
#' prediction sequence above.
#' }
#'
#' The offset audit must precede prediction because prediction consumes
#' \code{prediction_quantiles.csv}. Aggregation then consumes unadjusted long
#' point predictions, OOF residual rows, metric rows, and offsets. The overfit
#' audit is diagnostic; aggregation does not use \code{overfit.csv}. In
#' particular, \code{ensemble} is first checked when [predictModel()] starts:
#' after wiping, split construction, and the offset audit in prediction mode,
#' and additionally after fitting in full mode. An invalid value can therefore
#' leave partial products before it errors.
#'
#' @section Wipe and cache policy:
#' With \code{wipe = NULL} or true, the function recursively removes and
#' recreates \code{.path.train}, \code{.path.metrics}, \code{.path.response},
#' and \code{.path.summary} in every mode. In \code{"full"} mode only, it also
#' recursively removes \code{.path.model} and \code{.path.export}; this same
#' condition sets \code{OVERRIDE = TRUE} for fitting. These operations are
#' destructive and have no rollback.
#'
#' In \code{"train"} or \code{"predict"} mode, wiping preserves existing model
#' and export directories. Training mode then uses \code{OVERRIDE = FALSE}, so
#' surviving models can be reused under [trainModel()]'s predictor-name-only
#' cache rule. With \code{wipe = FALSE}, every existing output is preserved,
#' missing directories are created, and full-mode fitting also uses
#' \code{OVERRIDE = FALSE}. Current metric, residual, export, and iteration
#' helpers then apply their own merge or overwrite rules.
#'
#' @section Products, selectors, and state:
#' Every mode can overwrite split files. Train mode can additionally write
#' fitted model and hash files, then returns without summary or delivery calls.
#' Prediction mode never fits or writes model/hash files; it can write cell
#' metrics and responses, \code{metrics.csv}, \code{response_long.csv},
#' \code{residuals_oof.csv}, \code{prediction_quantiles.csv},
#' \code{overfit.csv}, \code{Yo.csv}, \code{X.csv}, \code{Xo.csv}, optional
#' \code{Y.csv}, and dataset-named intermediate exports. Full mode can write
#' all train- and prediction-mode products.
#'
#' The \code{ensemble} argument governs only [predictModel()]'s per-response
#' dataset selector. [aggregateResponses()] independently forms its final
#' rowwise selection from \code{ensemble.RMSE} candidates and cell R2. These
#' selectors must not be described as one common "best model" rule.
#'
#' Supplying \code{seed} changes the caller's RNG state. Both fitting and the
#' evaluation engine invoked by [predictModel()] register a bounded PSOCK
#' backend and leave foreach sequential under their accepted contracts.
#' Shadow generation and prediction method randomization have their documented
#' RNG effects. Errors and partial files from any helper propagate;
#' orchestration is not transactional.
#'
#' @section Partial-response state:
#' With a non-null \code{responsesActive}, the helpers do not follow one common
#' accumulation rule. [auditQuantiles()] replaces residual rows for active
#' responses in an existing \code{residuals_oof.csv} and regenerates offsets
#' from the merged residual table. [predictModel()] overwrites
#' \code{response_long.csv} with current active-response predictions; its
#' \code{metrics.csv} is the union of every cell-metric CSV currently present.
#' [auditOverfit()] overwrites \code{overfit.csv} from models matching the
#' active-response expression. [aggregateResponses()] preserves old
#' \code{Yo.csv} rows only for responses absent from the current unseen result,
#' and optional \code{Y.csv} preserves old non-overlapping response columns;
#' both use their own replacement rules. \code{X.csv} and \code{Xo.csv} are
#' overwritten from the last existing declared dataset. Thus
#' \code{wipe = FALSE} preserves inputs for mixed accumulation but does not
#' make every summary cumulative.
#'
#' @return Invisibly, \code{NULL}. Results are the files produced by the
#'   selected mode and its composed helper calls.
#' @seealso [buildDataset()] for assembled input, [trainModel()] for fitting,
#'   [predictModel()] for the first selector, and [aggregateResponses()] for
#'   final delivery.
#' @examples
#' \donttest{
#' root <- tempfile("ssel-pipeline-")
#' datasets_dir <- file.path(root, "datasets")
#' dir.create(datasets_dir, recursive = TRUE)
#'
#' assembled <- data.frame(
#'   SampleID = c(paste0("S", 1:16), "P1", "P2"),
#'   set = c(rep("train", 16), "predict", "predict"),
#'   x = 1:18,
#'   z = rep(c(0, 1), 9),
#'   target = c(1:16 / 3 + rep(c(-0.5, 0.5), 8), NA, NA)
#' )
#' utils::write.csv(
#'   assembled,
#'   file.path(datasets_dir, "demo.csv"),
#'   row.names = FALSE,
#'   na = ""
#' )
#'
#' old_quiet <- getOption("aR.quiet")
#' options(aR.quiet = TRUE)
#' modelPipeline(
#'   .path.datasets = datasets_dir,
#'   .path.train = file.path(root, "train"),
#'   .path.model = file.path(root, "model"),
#'   .path.metrics = file.path(root, "metrics"),
#'   .path.response = file.path(root, "response"),
#'   .path.summary = file.path(root, "summary"),
#'   .path.export = file.path(root, "export"),
#'   key = "SampleID",
#'   responses = "target",
#'   datasets = "demo",
#'   methods = "lm",
#'   seed = 2026,
#'   ensemble = "ensemble.RMSE",
#'   mode = "full"
#' )
#' options(aR.quiet = old_quiet)
#'
#' sort(basename(list.files(file.path(root, "summary"))))
#' data.table::fread(file.path(root, "export", "Yo.csv"))
#'
#' unlink(root, recursive = TRUE)
#' }
#' @import data.table
#' @export
modelPipeline <- function(
    .path.datasets,
    .path.train,
    .path.model,
    .path.metrics,
    .path.response,
    .path.summary,
    .path.export,
    .path.iter      = NULL,
    key,
    responses,
    datasets,
    colsId          = character(0),
    features        = NULL,
    setCol          = "set",
    methods         = c("ranger", "earth", "gbm"),
    seed            = NULL,
    ensemble        = "ensemble.RMSE",
    boundsMin       = -Inf,
    boundsMax       = Inf,
    iterSuffix      = NULL,
    mode            = "full",
    responsesActive = NULL,
    wipe            = NULL,
    shadow          = FALSE
) {
  .keysOld <- options(datatable.keepLeadingZeros = TRUE)
  on.exit(options(.keysOld), add = TRUE)
  stopifnot(is.character(key),       nzchar(key))
  stopifnot(is.character(responses), length(responses) > 0L)
  stopifnot(is.character(datasets),  length(datasets) > 0L)
  stopifnot(is.character(setCol),    nzchar(setCol))
  stopifnot(mode %in% c("full","train","predict"))
  if (!is.null(features)) stopifnot(is.character(features))
  .checkBound(boundsMin, "boundsMin")
  .checkBound(boundsMax, "boundsMax")
  if (!is.null(seed)) set.seed(seed)

  Y.PATTERN   <- paste0("^(", paste(responses, collapse = "|"), ")$")
  RESP.active <- if (is.null(responsesActive)) responses else responsesActive
  RESP_PAT    <- paste(RESP.active, collapse = "|")
  WIPE        <- if (is.null(wipe)) TRUE else isTRUE(wipe)
  OVERRIDE    <- WIPE && (mode == "full")

  # WIPE controls all wipes. Default TRUE preserves legacy behaviour. GS
  # sub-iters call with WIPE=FALSE so previous responses' models / metrics
  # accumulate across sub-iters within the same chain iteration.
  if (WIPE) {
    .guardWipe(c(.path.train, .path.metrics, .path.response, .path.summary,
                 if (isTRUE(OVERRIDE)) c(.path.model, .path.export)),
               .path.datasets)
    for (Dir in c(.path.train, .path.metrics, .path.response, .path.summary))
      { if (dir.exists(Dir)) unlink(Dir, recursive = TRUE); dir.create(Dir, recursive = TRUE) }
    if (isTRUE(OVERRIDE)) {
      if (dir.exists(.path.model))  unlink(.path.model,  recursive = TRUE)
      if (dir.exists(.path.export)) unlink(.path.export, recursive = TRUE)
    }
  }
  for (Dir in c(.path.train, .path.metrics, .path.response, .path.summary,
                .path.model, .path.export, .path.iter))
    if (!is.null(Dir) && !dir.exists(Dir)) dir.create(Dir, recursive = TRUE)

  # Build train/test splits.
  buildDataset(.path.input = .path.datasets, .path.train = .path.train,
               .file.index = NULL,
               Y.PATTERN = Y.PATTERN, KEY = key, COLS.id = colsId,
               features = features, OUTLIER_REMOVAL = FALSE,
               SET_COL = setCol, ITER_SUFFIX = iterSuffix, .shadow = shadow)

  if (mode %in% c("full","train")) {
    trainModel(.path.train = .path.train, .path.model = .path.model,
               .path.metrics = .path.metrics, .path.response = .path.response,
               METHODS = methods, PPROC = NULL, OVERRIDE = OVERRIDE,
               .min = boundsMin, .max = boundsMax,
               .responses = responsesActive)
  }

  if (mode %in% c("full","predict")) {
    auditQuantiles(.path.model = .path.model, .path.train = .path.train,
                   .path.summary = .path.summary, .path.datasets = .path.datasets,
                   .min = boundsMin, .max = boundsMax,
                   .response_pattern = RESP_PAT, .sort_keys = character(0))

    predictModel(.path.train = .path.train, .path.model = .path.model,
                 .path.metrics = .path.metrics, .path.response = .path.response,
                 .path.summary = .path.summary, .path.export = .path.export,
                 .file.index = NULL, METHODS = methods, PPROC = NULL,
                 .min = boundsMin, .max = boundsMax, .ensemble = ensemble,
                 .responses = responsesActive)

    auditOverfit(.path.model = .path.model, .path.train = .path.train,
                 .path.summary = .path.summary,
                 .min = boundsMin, .max = boundsMax,
                 REL_OPTIMISM_CATASTROPHIC = 0.95, REL_OPTIMISM_HIGH = 0.60,
                 .response_pattern = RESP_PAT)

    aggregateResponses(.path.summary = .path.summary,
                       .path.export  = .path.export,
                       .path.datasets = .path.datasets,
                       key           = key,
                       responses     = responses,
                       datasets      = datasets,
                       boundsMin     = boundsMin,
                       boundsMax     = boundsMax,
                       .path.iter    = .path.iter)
  }
}
# nolint end
