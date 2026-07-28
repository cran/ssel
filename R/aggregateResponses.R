# nolint start
# aggregateResponses: assemble final exports from per-method predictions.
#
# Yo.csv  - predictions at predict rows (long format, quantile bands).
# Y.csv   - predictions at training observations (wide, p=0.5 only).
#           Only written when .path.iter is provided (chain consumers).
# X.csv   - feature values at training observations (wide).
# Xo.csv  - feature values at predict observations (wide).
#
# Sources: response_long.csv, metrics.csv, prediction_quantiles.csv,
#          residuals_oof.csv from .path.summary.

#' Select and assemble final response deliveries
#'
#' Combine unseen ensemble point predictions with reconstructed training-row
#' OOF predictions, add signed residual offsets, select one candidate per
#' sample and response by cell R2, and write delivery/feature products. This
#' final selector is distinct from [predictModel()]'s response-level
#' RMSE-or-R2 selector.
#'
#' @param .path.summary Existing directory containing the four summary inputs
#'   described in **Input tables**.
#' @param .path.export Existing writable output directory. It is not created by
#'   this helper. \code{Yo.csv}, \code{X.csv}, and \code{Xo.csv} are assigned
#'   here.
#' @param .path.datasets Directory containing assembled
#'   \code{dataset.csv} inputs used only for feature exports.
#' @param key A non-empty character key name used when extracting feature
#'   tables. Response aggregation itself is hard-coded to \code{SampleID};
#'   setting another key does not replace that requirement.
#' @param responses Non-empty character vector excluded, with \code{set}, from
#'   feature columns in \code{X.csv} and \code{Xo.csv}.
#' @param datasets Non-empty character vector of assembled dataset basenames.
#'   Feature extraction uses the last declared file that actually exists.
#' @param boundsMin,boundsMax Numeric response bounds used to project every
#'   point-plus-offset value. Their order and finiteness are not validated.
#' @param .path.iter \code{NULL}, or an iteration directory. When non-null, its
#'   parent path for \code{Y.csv} is created recursively and the training-row
#'   median-offset table is written or merged.
#'
#' @section Input tables:
#' The helper stops unless \code{response_long.csv}, \code{metrics.csv}, and
#' \code{prediction_quantiles.csv} exist. It then separately requires
#' \code{residuals_oof.csv}.
#'
#' \itemize{
#' \item \code{response_long.csv} must provide \code{SampleID}, \code{value},
#' \code{method}, \code{dataset}, and \code{response}. Only rows with
#' \code{method == "ensemble.RMSE"} are retained and marked as unseen rows.
#' \item \code{residuals_oof.csv} must provide \code{SampleID},
#' \code{y_hat_oof}, \code{dataset}, and \code{response}. Every row is converted
#' to a training candidate with \code{value = y_hat_oof} and method
#' \code{ensemble.RMSE}.
#' \item \code{metrics.csv} must provide \code{method}, \code{response},
#' \code{dataset}, and \code{r2}. Only \code{ensemble.RMSE} metric rows enter.
#' \item \code{prediction_quantiles.csv} must provide \code{response},
#' \code{dataset}, \code{q_50}, \code{q_90}, and \code{q_95}.
#' }
#'
#' Extra columns can propagate through joins. Missing, duplicate, or
#' incompatible keys retain data.table join behavior; no uniqueness assertion
#' is made.
#'
#' @section Offset construction and final selector:
#' Metric and offset rows are joined to every unseen or training candidate by
#' response and dataset. Let \eqn{i} index \code{SampleID}, \eqn{r} a
#' response, \eqn{d} a joined candidate dataset, and
#' \eqn{p\in\{0.50,0.90,0.95\}} an offset probability. Let
#' \eqn{\hat y_{ird}} be a candidate point value and \eqn{q_{p,rd}} its signed
#' OOF-residual offset, all in response units. Let \eqn{a} and \eqn{b} be the
#' bounds supplied by \code{boundsMin} and \code{boundsMax}, respectively.
#' When \eqn{a\le b}, define interval projection by
#' \eqn{\Pi_{[a,b]}(z)=\min\{b,\max\{a,z\}\}} for real \eqn{z}. The helper
#' computes
#' \deqn{Q_{p,ird}=\Pi_{[a,b]}\{\hat y_{ird}+q_{p,rd}\}.}
#' These are projected point-plus-offset estimates, not calibrated predictive
#' quantiles or intervals. Bound order and finiteness are not checked; with
#' invalid bounds the nested minimum/maximum is still executed but is not an
#' interval projection.
#'
#' For each \code{(SampleID, response)}, all RMSE-ensemble candidates are
#' ordered by descending joined cell \code{r2}; the first row is selected and
#' supplies one dataset for all three probabilities:
#' \deqn{d^*_{ir}=\operatorname*{first\,argmax}_{d}R^2_{rd}.}
#' The score is cell-level even though selection is grouped by sample.
#' Ties follow joined-row order. Finite scores precede missing scores; when all
#' joined scores in a group are missing, the first joined row is still selected
#' with missing \code{r2}. Missing offsets propagate missing delivery values
#' because this helper does not remove them. This selector never considers
#' \code{ensemble.R2} rows and is not the response-level metric selector used
#' by [predictModel()]. Therefore \code{modelPipeline(ensemble = )} does not
#' change the final candidate family assembled here.
#'
#' @section Delivery and iteration products:
#' The selected table is expanded to long probabilities \code{p = 0.50},
#' \code{0.90}, and \code{0.95}. It retains available identifier columns from
#' \code{SampleID}, \code{HoleID}, \code{From}, and \code{To}, followed by
#' \code{response}, \code{p}, \code{value}, \code{r2}, and \code{dataset}.
#' Response values and offsets share response units; \code{r2} and \code{p}
#' are dimensionless.
#'
#' Unseen rows are written to \code{Yo.csv}. If that file exists, every old row
#' whose response occurs in the current unseen result is removed, current rows
#' are appended, and other responses are preserved. Replacement is by response,
#' not dataset.
#'
#' When \code{.path.iter} is non-null, training rows at \code{p = 0.50} are
#' cast to \code{Y.csv} with one response column per \code{SampleID}. On merge,
#' new response columns replace same-named old columns and old non-overlapping
#' response columns remain. Thus \code{Y.csv} contains median signed-offset
#' estimates; \code{p = 0.50} is not necessarily the unadjusted point value.
#'
#' @section Feature products, conditions, and return:
#' Among declared assembled files that exist, the last one is read. Every
#' column except \code{key}, the declared responses, and \code{set} is treated
#' as a feature. Rows with \code{set == "train"} write \code{X.csv}; rows with
#' \code{set == "predict"} write \code{Xo.csv}. If no assembled file exists, a
#' warning is emitted and neither feature file is written. If no feature column
#' remains, a warning is emitted but key-only files are still written.
#'
#' Assigned CSVs are overwritten and writes are not transactional. Required
#' file, column, join, cast, and directory errors propagate after any earlier
#' writes. Valid progress is sent through ssel's debug channel, which emits
#' only when global verbose is enabled and quiet is disabled. The helper uses
#' no random numbers.
#'
#' @return Invisibly, \code{NULL}. Results are \code{Yo.csv}, optional
#'   \code{Y.csv}, and the feature files written under the rules above.
#' @seealso [predictModel()] for cell evaluation and the first selector,
#'   [auditQuantiles()] for signed residual offsets, and [chainPipeline()] for
#'   the consumer of optional \code{Y.csv}.
#' @references
#' Parikh, N., and Boyd, S. (2014). Proximal Algorithms.
#' \emph{Foundations and Trends in Optimization}, 1(3), 127--239.
#' \doi{10.1561/2400000003}.
#' @examples
#' root <- tempfile("ssel-aggregate-")
#' summary_dir <- file.path(root, "summary")
#' export_dir <- file.path(root, "export")
#' datasets_dir <- file.path(root, "datasets")
#' iter_dir <- file.path(root, "iter")
#' for (path in c(summary_dir, export_dir, datasets_dir)) {
#'   dir.create(path, recursive = TRUE)
#' }
#'
#' data.table::fwrite(
#'   data.table::data.table(
#'     SampleID = rep(c("P1", "P2"), each = 2),
#'     value = c(10, 20, 11, 21),
#'     method = "ensemble.RMSE",
#'     dataset = rep(c("d1", "d2"), 2),
#'     response = "target"
#'   ),
#'   file.path(summary_dir, "response_long.csv")
#' )
#' data.table::fwrite(
#'   data.table::data.table(
#'     method = "ensemble.RMSE", response = "target",
#'     dataset = c("d1", "d2"), r2 = c(0.5, 0.9)
#'   ),
#'   file.path(summary_dir, "metrics.csv")
#' )
#' data.table::fwrite(
#'   data.table::data.table(
#'     response = "target", dataset = c("d1", "d2"),
#'     q_50 = c(0, 1), q_90 = c(1, 2), q_95 = c(2, 3)
#'   ),
#'   file.path(summary_dir, "prediction_quantiles.csv")
#' )
#' data.table::fwrite(
#'   data.table::data.table(
#'     response = "target", dataset = c("d1", "d2"),
#'     rowIndex = 1L, SampleID = "S1",
#'     y_obs = c(8, 18), y_hat_oof = c(9, 19), residual = -1
#'   ),
#'   file.path(summary_dir, "residuals_oof.csv")
#' )
#'
#' for (dataset in c("d1", "d2")) {
#'   data.table::fwrite(
#'     data.table::data.table(
#'       SampleID = c("S1", "P1", "P2"),
#'       set = c("train", "predict", "predict"),
#'       feature = 1:3
#'     ),
#'     file.path(datasets_dir, paste0(dataset, ".csv"))
#'   )
#' }
#'
#' aggregateResponses(
#'   .path.summary = summary_dir,
#'   .path.export = export_dir,
#'   .path.datasets = datasets_dir,
#'   key = "SampleID",
#'   responses = "target",
#'   datasets = c("d1", "d2"),
#'   .path.iter = iter_dir
#' )
#' data.table::fread(file.path(export_dir, "Yo.csv"))
#' data.table::fread(file.path(iter_dir, "Y.csv"))
#' # Dataset d2 wins because its RMSE-ensemble cell has the larger R2.
#'
#' unlink(root, recursive = TRUE)
#' @import data.table
#' @export
aggregateResponses <- function(
    .path.summary,
    .path.export,
    .path.datasets,
    key,
    responses,
    datasets,
    boundsMin    = -Inf,
    boundsMax    = Inf,
    .path.iter   = NULL
) {
  stopifnot(is.character(key), nzchar(key))
  stopifnot(is.character(responses), length(responses) > 0L)
  stopifnot(is.character(datasets), length(datasets) > 0L)

  .clamp <- function(x) pmax(boundsMin, pmin(boundsMax, x))

  FILE.rl  <- file.path(.path.summary, "response_long.csv")
  FILE.met <- file.path(.path.summary, "metrics.csv")
  FILE.pq  <- file.path(.path.summary, "prediction_quantiles.csv")
  for (.f in c(FILE.rl, FILE.met, FILE.pq))
    if (!file.exists(.f)) stop(sprintf("aggregateResponses: required file missing: %s", .f))

  # Predict rows: caret predictions from response_long (ensemble.RMSE).
  RL.pred <- fread(FILE.rl)[method == "ensemble.RMSE"]
  RL.pred[, .is_train := FALSE]

  # Training rows: OOF predictions from residuals_oof. Under the integrated
  # domain pattern, training rows are never in set == "predict", so caret
  # never predicts them; their values come from the cross-validated OOF
  # ensemble reconstruction (sklearn.RegressorChain(cv=int) regime).
  FILE.oof <- file.path(.path.summary, "residuals_oof.csv")
  if (!file.exists(FILE.oof))
    stop("aggregateResponses: residuals_oof.csv missing - auditQuantiles must run before predictModel.")
  OOF.synth <- fread(FILE.oof)[, .(SampleID, value = y_hat_oof,
                                    method = "ensemble.RMSE",
                                    dataset, response)]
  OOF.synth[, set := paste0(response, "_", dataset)]
  OOF.synth[, .is_train := TRUE]

  RL  <- rbind(RL.pred, OOF.synth, use.names = TRUE, fill = TRUE)
  MET <- fread(FILE.met)[method == "ensemble.RMSE",
                          .(response, dataset, r2)]
  PQ  <- fread(FILE.pq)[, .(response, dataset, q_50, q_90, q_95)]

  DT <- MET[RL, on = .(response, dataset)]
  DT <- PQ[DT, on = .(response, dataset)]

  DT[, `:=`(value.50 = .clamp(value + q_50),
            value.90 = .clamp(value + q_90),
            value.95 = .clamp(value + q_95))]

  best <- DT[order(-r2), .SD[1L], by = .(SampleID, response)]

  id_cols <- intersect(c("SampleID","HoleID","From","To"), names(best))
  final <- rbindlist(lapply(c(50, 90, 95), function(pct) {
    col <- paste0("value.", pct)
    best[, c(id_cols, "response", "dataset", "r2", ".is_train"),
         with = FALSE][, `:=`(p = pct / 100, value = best[[col]])]
  }), use.names = TRUE)
  setcolorder(final, c(id_cols, "response", "p", "value", "r2", "dataset"))

  # Training rows vs prediction rows: tagged at source (RL.pred = predict,
  # OOF.synth = train) -- robust across all projects, independent of any
  # SampleID prefix convention. Train rows go to data/iter/Y.csv (chain),
  # predict rows go to Yo.csv (deliverable).
  IS_TRAIN <- final$.is_train
  final[, .is_train := NULL]

  Yo <- final[!IS_TRAIN]
  # Merge with existing Yo.csv: preserve rows of responses NOT being updated
  # this run (relevant for Gauss-Seidel sub-iters where one response runs
  # at a time and others survive from the prior sub-iter).
  EXPORT.yo <- file.path(.path.export, "Yo.csv")
  if (file.exists(EXPORT.yo)) {
    Yo.prev <- fread(EXPORT.yo)
    Yo.prev <- Yo.prev[!response %in% unique(Yo$response)]
    Yo      <- rbindlist(list(Yo.prev, Yo), use.names = TRUE, fill = TRUE)
  }
  fwrite(Yo, EXPORT.yo)

  # data/iter/Y.csv: wide, p=0.5 - one column per response. Only written
  # when .path.iter is provided. Accumulates across iterations.
  Y.wide <- NULL
  if (!is.null(.path.iter)) {
    ITER_Y <- file.path(.path.iter, "Y.csv")
    if (!dir.exists(dirname(ITER_Y))) dir.create(dirname(ITER_Y), recursive = TRUE)
    Y.new  <- dcast(final[IS_TRAIN & p == 0.5, .(SampleID, response, value)],
                    SampleID ~ response, value.var = "value")
    if (file.exists(ITER_Y)) {
      Y.prev <- fread(ITER_Y)
      Y.wide <- merge(Y.prev, Y.new, by = "SampleID", all = TRUE, suffixes = c(".old", ""))
      OLD    <- grep("\\.old$", names(Y.wide), value = TRUE)
      if (length(OLD) > 0L) Y.wide[, (OLD) := NULL]
    } else {
      Y.wide <- Y.new
    }
    fwrite(Y.wide, ITER_Y)
  }

  # X.csv / Xo.csv: feature values (wide), using the last-declared dataset
  # (typically the most feature-rich one).
  DS.files <- file.path(.path.datasets, paste0(datasets, ".csv"))
  DS.files <- DS.files[file.exists(DS.files)]
  DS.pred  <- NULL
  DS.train <- NULL
  if (length(DS.files) == 0L) {
    .WARN("aggregateResponses: no dataset files found; X.csv/Xo.csv not written")
  } else {
    DS   <- fread(DS.files[length(DS.files)])
    RESP <- c(responses, "set")
    FEAT <- setdiff(names(DS), c(key, RESP))
    if (length(FEAT) == 0L) .WARN("aggregateResponses: no feature columns found for X.csv")
    DS.train <- DS[set == "train",   c(key, FEAT), with = FALSE]
    DS.pred  <- DS[set == "predict", c(key, FEAT), with = FALSE]
    fwrite(DS.pred,  file.path(.path.export, "Xo.csv"))
    fwrite(DS.train, file.path(.path.export, "X.csv"))
  }

  .DEBUG(sprintf("Yo.csv: %d rows  |  Y.csv: %s rows  |  Xo: %s grid pts  |  X: %s train pts",
                 nrow(Yo),
                 if (is.null(Y.wide))  "n/a" else as.character(nrow(Y.wide)),
                 if (is.null(DS.pred)) "n/a" else as.character(nrow(DS.pred)),
                 if (is.null(DS.train))"n/a" else as.character(nrow(DS.train))))
}
# nolint end
