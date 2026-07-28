# nolint start
# *****************************************************************************
# auditOverfit - in-sample vs cross-validation memorisation audit.
#
# Per docs/handoff/03-overfit-memorization-audit.md.
#
# Per-method (written to data/summary/overfit.csv):
# For every caret::train object in .path.model: predict in-sample, compute the
# in-sample RMSE; read model$resample (per-fold CV metrics) for the CV RMSE.
# Relative optimism = (rmse_cv_mean - rmse_in) / rmse_cv_mean is the classical
# measure (Efron & Tibshirani 1993; Hastie, Tibshirani, Friedman 2009 ESL section 7).
# A value near 1 means the in-sample error is near zero while the honest CV
# error is large - the model memorised the training set.
#
# Thresholds are provided by the caller (defaults empirically derived from the
# current CMM data; see handoff section "Why the thresholds are 0.95 and 0.60").
# *****************************************************************************

#' @export
auditOverfit <- function(.path.model, .path.train, .path.summary,
                        .min                      = -Inf, .max = Inf,
                        REL_OPTIMISM_CATASTROPHIC = 0.95,
                        REL_OPTIMISM_HIGH         = 0.60,
                        .response_pattern         = NULL,
                        verbose                   = TRUE) {
  stopifnot(!is.null(.response_pattern),
            is.character(.response_pattern),
            nzchar(.response_pattern))

  clamp <- function(x) pmax(.min, pmin(.max, x))

  files       <- list.files(.path.model, pattern = "\\.Rds$", full.names = TRUE)
  fname_regex <- sprintf("^([^_]+)_(%s)_(.+)\\.Rds$", .response_pattern)
  rows        <- list()

  for (f in files) {
    # Model filename pattern: <method>_<response>_<dataset>.Rds
    m <- regmatches(basename(f), regexec(fname_regex, basename(f)))[[1]]
    if (length(m) < 4) next
    method <- m[2]; response <- m[3]; dataset <- m[4]

    train_file <- file.path(.path.train, paste0(response, "_", dataset, "_TRAIN.csv"))
    if (!file.exists(train_file)) next
    DT    <- fread(train_file)
    y     <- suppressWarnings(as.numeric(DT$y))

    model <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(model)) next

    # In-sample RMSE + R2 (clamped to physical bounds)
    yhat_in <- tryCatch(clamp(as.numeric(predict(model, newdata = DT))),
                        error = function(e) NULL)
    if (is.null(yhat_in) || length(yhat_in) != length(y)) next

    rmse_in <- sqrt(mean((y - yhat_in)^2))
    r2_in   <- if (var(y) == 0) NA_real_ else suppressWarnings(cor(y, yhat_in))^2

    # Per-fold CV metrics (one row per fold in model$resample)
    res <- model$resample
    if (is.null(res) || nrow(res) == 0) next
    rmse_cv <- suppressWarnings(as.numeric(res$RMSE))
    r2_cv   <- suppressWarnings(as.numeric(res$Rsquared))

    rmse_cv_mean <- mean(rmse_cv, na.rm = TRUE)
    rel_opt      <- (rmse_cv_mean - rmse_in) / pmax(rmse_cv_mean, 1e-9)

    flag <- fcase(
      !is.finite(rel_opt),                  "unclassified",
      rel_opt >= REL_OPTIMISM_CATASTROPHIC, "catastrophic",
      rel_opt >= REL_OPTIMISM_HIGH,         "high_optimism",
      default =                             "reliable"
    )

    rows[[length(rows) + 1L]] <- data.table(
      method        = method,
      response      = response,
      dataset       = dataset,
      n_train       = length(y),
      k_folds       = nrow(res),
      rmse_in       = rmse_in,
      rmse_cv_mean  = rmse_cv_mean,
      rmse_cv_sd    = sd(rmse_cv, na.rm = TRUE),
      rmse_cv_min   = min(rmse_cv, na.rm = TRUE),
      rmse_cv_max   = max(rmse_cv, na.rm = TRUE),
      optimism_rmse = rmse_cv_mean - rmse_in,
      rel_optimism  = rel_opt,
      r2_in         = r2_in,
      r2_cv_mean    = mean(r2_cv, na.rm = TRUE),
      r2_cv_sd      = sd(r2_cv,   na.rm = TRUE),
      flag          = flag
    )
  }

  out <- rbindlist(rows)
  if (!dir.exists(.path.summary)) dir.create(.path.summary, recursive = TRUE)
  out_file <- file.path(.path.summary, "overfit.csv")
  fwrite(out, out_file)

  if (verbose) {
    .DEBUG("auditOverfit ->", out_file)
    .DEBUG("flag counts:")
    .DEBUG(paste(utils::capture.output(out[, .N, by = flag]), collapse = "\n"))
  }

  invisible(list(overfit = out))
}
# nolint end
