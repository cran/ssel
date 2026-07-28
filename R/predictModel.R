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
  stopifnot(.ensemble %in% c("ensemble.R2", "ensemble.RMSE"))
  if (!dir.exists(.path.summary)) dir.create(.path.summary)
  if (!dir.exists(.path.export))  dir.create(.path.export)

  FILES     <- list.files(.path.train, pattern = "_TRAIN\\.csv$", full.names = FALSE) |>
    sub(pattern = "\\.csv$", replacement = "") |> basename()
  if (!is.null(.responses))
    FILES <- FILES[sapply(FILES, function(.x) strsplit(.x, "_")[[1]][1]) %in% .responses]
  RESPONSES <- sapply(FILES, function(.x) strsplit(.x, "_")[[1]][1]) |> unname() |> unique()
  DATASETS  <- sapply(FILES, function(.x) strsplit(.x, "_")[[1]][2]) |> unname() |> unique() |> sort()

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

  # -- Bind metrics -----------------------------------------------------------
  DT.metrics <- rbindlist(
    lapply(list.files(.path.metrics, pattern = "\\.csv$", full.names = TRUE), fread),
    use.names = TRUE, fill = TRUE
  ) |> unique()
  fwrite(DT.metrics, file.path(.path.summary, "metrics.csv"))

  # -- Merge responses --------------------------------------------------------
  DT <- data.table()
  for (.dataset in DATASETS) {
    for (.response in RESPONSES) {
      FILE <- file.path(.path.response, paste0(.response, "_", .dataset, ".csv"))
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
  # When .deliverable_datasets is NULL (default), every dataset in DT.metrics is
  # eligible. Callers that need to restrict to a subset (e.g. drop XRF-augmented
  # variants that have zero training rows on a given delivery) pass a vector.
  DT.metrics.main <- if (is.null(.deliverable_datasets)) DT.metrics
                     else DT.metrics[dataset %in% .deliverable_datasets]
  BM  <- if (.ensemble == "ensemble.RMSE")
           DT.metrics.main[method == .ensemble, .SD[which.min(rmse)], by = response]
         else
           DT.metrics.main[method == .ensemble, .SD[which.max(r2)],   by = response]

  pq_file <- file.path(.path.summary, "prediction_quantiles.csv")
  if (!file.exists(pq_file)) {
    stop("predictModel: ", pq_file, " not found - auditQuantiles must run before predictModel.")
  }
  PQ <- fread(pq_file)

  DT  <- data.table()
  for (.set in unique(BM$set)) {
    AUX  <- DTL[method == .ensemble & set == .set]
    row  <- BM[set == .set]
    stopifnot(nrow(row) == 1)
    Qrow <- PQ[response == row$response & dataset == row$dataset]
    stopifnot(nrow(Qrow) == 1)
    AUX[, (newCols) := lapply(probs, function(p) {
      offset <- Qrow[[sprintf("q_%02d", as.integer(p * 100))]]
      value + offset
    })]
    # Clamp predictive-band quantiles to physical bounds (same as point predictions)
    AUX[, (newCols) := lapply(.SD, function(q) pmax(.min, pmin(.max, q))),
        .SDcols = newCols]
    DT <- rbindlist(list(DT, AUX[]), use.names = TRUE)
  }
  DT  <- DT |> na.omit() |> unique()
  DTW <- rbindlist(lapply(as.integer(probs * 100), function(pct) {
    col <- paste0("value.", pct)
    dcast(DT, SampleID ~ response, value.var = col)[, p := pct / 100]
  }), use.names = TRUE)

  fwrite(DTL, file.path(.path.summary, "response_long.csv"))
  for (.dataset in DATASETS) {
    .sids <- unique(DTL[grepl(paste0("_", .dataset, "$"), set), SampleID])
    DTW.ds <- DTW[SampleID %in% .sids]
    if (nrow(DTW.ds) > 0) {
      fwrite(DTW.ds, file.path(.path.export, paste0(.dataset, ".csv")))
      .DEBUG("Fitted dataset:", .dataset, "->", file.path(.path.export, paste0(.dataset, ".csv")))
    }
  }

  # -- Bind location data -----------------------------------------------------
  if (!is.null(.file.index) && file.exists(.file.index)) {
    IDX  <- fread(.file.index)
    .key <- names(IDX)[1]
    for (.f in list.files(c(.path.summary, .path.export), pattern = "\\.csv$", full.names = TRUE)) {
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
