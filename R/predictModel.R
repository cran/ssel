# nolint start
#' @export
predictModel <- function(
    .path.train,
    .path.model,
    .path.metrics,
    .path.response,
    .path.summary,
    .path.export,
    .file.index           = NULL,
    METHODS,
    PPROC                 = NULL,
    probs                 = c(0.50, 0.90, 0.95),
    .min                  = -Inf,
    .max                  = Inf,
    .ensemble             = "ensemble.R2",
    .deliverable_datasets = NULL,
    .responses            = NULL   # optional filter: predict only these responses
) {
  .keysOld <- options(datatable.keepLeadingZeros = TRUE)
  on.exit(options(.keysOld), add = TRUE)
  stopifnot(.ensemble %in% c("ensemble.R2", "ensemble.RMSE"))
  .checkBound(.min, ".min")
  .checkBound(.max, ".max")
  if (!dir.exists(.path.summary)) dir.create(.path.summary)
  if (!dir.exists(.path.export))  dir.create(.path.export)

  FILES     <- .splitFiles(.path.train, .responses, "predictModel")
  RESPONSES <- sapply(FILES, function(.x) strsplit(.x, "_")[[1]][1]) |> unname() |> unique()
  DATASETS  <- sapply(FILES, function(.x) strsplit(.x, "_")[[1]][2]) |> unname() |> unique() |> .portableSort()

  # -- Predict ----------------------------------------------------------------
  trainRegressionModel(
    TRAIN          = FALSE,
    .path.data     = .path.train,
    .path.model    = .path.model,
    .path.metrics  = .path.metrics,
    .path.response = .path.response,
    .responses     = RESPONSES,
    .datasets      = DATASETS,
    .preProcess    = PPROC,
    .methods       = METHODS |> unique() |> sample(),
    .min           = .min,
    .max           = .max
  )

  .summarizePredictions(
    pathMetrics = .path.metrics, pathResponse = .path.response,
    pathSummary = .path.summary, pathExport = .path.export,
    datasets = DATASETS, responses = RESPONSES, probs = probs,
    ensemble = .ensemble, boundsMin = .min, boundsMax = .max,
    pathIndex = .file.index, deliverableDatasets = .deliverable_datasets
  )
}

.summarizePredictions <- function(pathMetrics, pathResponse, pathSummary, pathExport,
                                  datasets, responses, probs, ensemble,
                                  boundsMin, boundsMax, pathIndex, deliverableDatasets) {
  # -- Bind metrics -----------------------------------------------------------
  DT.metrics <- rbindlist(
    lapply(.listFiles(pathMetrics, pattern = "\\.csv$", full.names = TRUE), fread),
    use.names = TRUE, fill = TRUE
  ) |> unique()
  fwrite(DT.metrics, file.path(pathSummary, "metrics.csv"))

  # -- Merge responses --------------------------------------------------------
  DT <- data.table()
  for (.dataset in datasets) {
    for (.response in responses) {
      FILE <- file.path(pathResponse, paste0(.response, "_", .dataset, ".csv"))
      if (!file.exists(FILE)) next
      AUX <- fread(FILE) |> unique()
      AUX[, set := paste0(.response, "_", .dataset)]
      DT <- rbindlist(list(AUX, DT), use.names = TRUE, fill = TRUE)
    }
  }
  DTL <- DT |> unique()
  rm(DT, AUX)

  # -- Confidence intervals ---------------------------------------------------
  # Predictive band parameterised by empirical quantiles of OOF ensemble
  # residuals per cell (written by auditQuantiles to prediction_quantiles.csv).
  # value.P := value + q_P(residuals), where q_P is the empirical P-quantile
  # of { y_obs - y_hat_oof } on the training rows of the winning dataset.
  newCols <- paste0("value.", as.integer(probs * 100))
  # Select the best dataset per response using the metric matching the chosen ensemble.
  # When deliverableDatasets is NULL (default), every dataset in DT.metrics is
  # eligible. Callers that need to restrict to a subset (e.g. drop XRF-augmented
  # variants that have zero training rows on a given delivery) pass a vector.
  DT.metrics.main <- if (is.null(deliverableDatasets)) DT.metrics
                     else DT.metrics[dataset %in% deliverableDatasets]
  BM  <- if (ensemble == "ensemble.RMSE")
           DT.metrics.main[method == ensemble, .SD[which.min(rmse)], by = response]
         else
           DT.metrics.main[method == ensemble, .SD[which.max(r2)],   by = response]

  pq_file <- file.path(pathSummary, "prediction_quantiles.csv")
  if (!file.exists(pq_file)) {
    stop("predictModel: ", pq_file, " not found - auditQuantiles must run before predictModel.")
  }
  PQ <- fread(pq_file)

  DT  <- data.table()
  for (.set in unique(BM$set)) {
    AUX  <- DTL[method == ensemble & set == .set]
    row  <- BM[set == .set]
    stopifnot(nrow(row) == 1)
    Qrow <- PQ[response == row$response & dataset == row$dataset]
    stopifnot(nrow(Qrow) == 1)
    AUX[, (newCols) := lapply(probs, function(p) {
      offset <- Qrow[[sprintf("q_%02d", as.integer(p * 100))]]
      value + offset
    })]
    # Clamp predictive-band quantiles to physical bounds (same as point predictions)
    AUX[, (newCols) := lapply(.SD, function(q) pmax(boundsMin, pmin(boundsMax, q))),
        .SDcols = newCols]
    DT <- rbindlist(list(DT, AUX[]), use.names = TRUE)
  }
  DT  <- DT |> na.omit() |> unique()
  DTW <- rbindlist(lapply(as.integer(probs * 100), function(pct) {
    col <- paste0("value.", pct)
    dcast(DT, SampleID ~ response, value.var = col)[, p := pct / 100]
  }), use.names = TRUE)

  fwrite(DTL, file.path(pathSummary, "response_long.csv"))
  for (.dataset in datasets) {
    .sids <- unique(DTL[grepl(paste0("_", .dataset, "$"), set), SampleID])
    DTW.ds <- DTW[SampleID %in% .sids]
    if (nrow(DTW.ds) > 0) {
      fwrite(DTW.ds, file.path(pathExport, paste0(.dataset, ".csv")))
      .DEBUG("Fitted dataset:", .dataset, "->", file.path(pathExport, paste0(.dataset, ".csv")))
    }
  }

  # -- Bind location data -----------------------------------------------------
  if (!is.null(pathIndex) && file.exists(pathIndex)) {
    IDX  <- fread(pathIndex)
    .key <- names(IDX)[1]
    for (.f in .listFiles(c(pathSummary, pathExport), pattern = "\\.csv$", full.names = TRUE)) {
      DT <- fread(.f)
      if (.key %in% names(DT)) {
        DT <- IDX[DT, on = .key]
        fwrite(DT, .f)
        .DEBUG("Location joined to:", basename(.f))
      }
    }
  }

  invisible(list(DTL = DTL, DTW = DTW, DT.metrics = DT.metrics))
}
# nolint end
