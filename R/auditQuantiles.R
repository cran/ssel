# nolint start
# *****************************************************************************
# auditQuantiles - per-cell empirical quantiles of OOF ensemble residuals.
#
# Consumes the oofEnsemble() helper output (per-row OOF predictions + honest
# SampleID + residual per training row of each responsexdataset cell).
#
# Writes:
#   data/summary/residuals_oof.csv   - one row per (response x dataset x row)
#       Columns: response, dataset, rowIndex, SampleID, y_obs, y_hat_oof, residual
#   data/summary/prediction_quantiles.csv - one row per (response x dataset)
#       Columns: response, dataset, n, q_50, q_90, q_95
#
# Quantile convention: for each cell, q_p is the empirical p-quantile of the
# centred residuals { y_obs - y_hat_oof } shifted so that q_50 approx median
# offset. Used by predictModel to construct the predictive band
#   [yhat + q_p]  (symmetric-only when residuals are symmetric; asymmetric
#   otherwise by construction).
#
# NOTE on band construction: the currently-delivered probs are c(0.50, 0.90,
# 0.95) and predictModel reads q_50/q_90/q_95 as additive offsets to value.
# Symmetric interpretation of q_90 means "90th-percentile residual offset";
# an operator who wants a *two-sided* 90 % band constructs it as
#   [yhat + q_05,  yhat + q_95]
# which this module does NOT compute by default. If two-sided bands are
# required later, add q_05 to the probs vector and handle in predictModel.
# *****************************************************************************

#' @export
auditQuantiles <- function(.path.model, .path.train, .path.summary,
                           .path.datasets    = file.path("data", "datasets"),
                           .min              = -Inf, .max = Inf,
                           .response_pattern = NULL,
                           .sort_keys        = character(0),
                           verbose           = TRUE) {
  .keysOld <- options(datatable.keepLeadingZeros = TRUE)
  on.exit(options(.keysOld), add = TRUE)
  stopifnot(!is.null(.response_pattern),
            is.character(.response_pattern),
            nzchar(.response_pattern))
  .checkBound(.min, ".min")
  .checkBound(.max, ".max")

  if (!dir.exists(.path.summary)) dir.create(.path.summary, recursive = TRUE)

  RES <- oofEnsemble(.path.model       = .path.model,
                     .path.train       = .path.train,
                     .path.datasets    = .path.datasets,
                     .min              = .min,
                     .max              = .max,
                     .response_pattern = .response_pattern,
                     .sort_keys        = .sort_keys)

  if (nrow(RES) == 0) {
    # Name what was observed. The previous message blamed absent models even
    # when they were present and the response pattern had excluded them,
    # sending the caller to look for files that were there all along.
    stop("auditQuantiles: oofEnsemble reconstructed no rows from ",
         length(.listFiles(.path.model, pattern = "\\.Rds$")),
         " model file(s) in ", .path.model,
         " under the response pattern '", .response_pattern,
         "'. See the oofEnsemble warnings for which files matched and why",
         " each was skipped.", call. = FALSE)
  }

  # Merge with existing files when present. Full-pipeline runs (auditing every
  # response) replace every row; partial runs (e.g. GS sub-iters auditing one
  # response at a time) preserve rows for responses not in this batch.
  res_file <- file.path(.path.summary, "residuals_oof.csv")
  if (file.exists(res_file)) {
    RES.prev <- fread(res_file)
    RES.prev <- RES.prev[!response %in% unique(RES$response)]
    RES      <- rbindlist(list(RES.prev, RES), use.names = TRUE, fill = TRUE)
  }
  fwrite(RES, res_file)
  if (verbose) .DEBUG("auditQuantiles ->", res_file)

  PQ <- RES[, .(
    n    = .N,
    q_50 = quantile(residual, 0.50, names = FALSE, type = 7),
    q_90 = quantile(residual, 0.90, names = FALSE, type = 7),
    q_95 = quantile(residual, 0.95, names = FALSE, type = 7)
  ), by = .(response, dataset)]

  pq_file <- file.path(.path.summary, "prediction_quantiles.csv")
  fwrite(PQ, pq_file)
  if (verbose) {
    .DEBUG("auditQuantiles ->", pq_file)
    .DEBUG(sprintf("%d cells  q_90 median=%.4f  q_95 median=%.4f",
                 nrow(PQ), median(PQ$q_90), median(PQ$q_95)))
  }

  invisible(list(residuals = RES, quantiles = PQ))
}
# nolint end
