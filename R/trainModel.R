# nolint start
#' @export
trainModel <- function(
    .path.train,
    .path.model,
    .path.metrics,
    .path.response,
    METHODS,
    PPROC      = NULL,
    OVERRIDE   = FALSE,
    .min       = -Inf,
    .max       = Inf,
    .responses = NULL   # optional filter: train only these responses (default: all detected)
) {
  FILES     <- list.files(.path.train, pattern = "_TRAIN\\.csv$", full.names = FALSE) |>
    sub(pattern = "\\.csv$", replacement = "") |> basename()
  if (!is.null(.responses))
    FILES <- FILES[sapply(FILES, function(.x) strsplit(.x, "_")[[1]][1]) %in% .responses]
  RESPONSES <- sapply(FILES, function(.x) strsplit(.x, "_")[[1]][1]) |> unname() |> unique()
  DATASETS  <- sapply(FILES, function(.x) strsplit(.x, "_")[[1]][2]) |> unname() |> unique() |> sort() |> sample()

  if (!dir.exists(.path.model))    dir.create(.path.model)
  if (!dir.exists(.path.metrics))  dir.create(.path.metrics)
  if (!dir.exists(.path.response)) dir.create(.path.response)

  for (DatasetID in DATASETS) {
    withCallingHandlers(
    trainRegressionModel(
      OVERRIDE       = OVERRIDE,
      TRAIN          = TRUE,
      .path.data     = .path.train,
      .path.model    = .path.model,
      .path.metrics  = .path.metrics,
      .path.response = .path.response,
      .numberCV      = 5,
      .responses     = RESPONSES,
      .datasets      = DatasetID,
      .preProcess    = PPROC,
      .methods       = METHODS |> unique() |> sample(),
      .tuneLength    = 10,
      .metric        = "RMSE",
      .min           = .min,
      .max           = .max
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      # Generic caret warnings now have inline attribution emitted by
      # trainRegressionModel.R after each train() - muffle them here to
      # keep R's tail "Warning messages" buffer from re-listing them
      # without context.
      if (grepl("closing unused connection",                msg) ||
          grepl("missing values in resampled performance",  msg))
        invokeRestart("muffleWarning")
    })
  }
  invisible(NULL)
}
# nolint end
