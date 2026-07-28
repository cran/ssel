# nolint start
# oofEnsemble: per-training-row OOF predictions of the ensemble.RMSE
# reconstruction, with SampleID recovered from the source dataset.
#
# Returns one row per (response, dataset, training row) with columns:
#   response, dataset, rowIndex, SampleID, y_obs, y_hat_oof, residual.
# y_hat_oof is clamped to [.min, .max] and residual = y_obs - y_hat_oof.
#
# SampleID recovery: the row order in <dataset>.csv (sorted by the
# manifest-declared .sort_keys) is the source of truth for the caret
# rowIndex -> SampleID mapping. The helper FAILS LOUDLY if the sort
# invariant is broken or if lengths disagree; it never falls back to
# seq_along().

#' @export
oofEnsemble <- function(.path.model, .path.train,
                        .path.datasets    = file.path("data", "datasets"),
                        .min              = -Inf, .max = Inf,
                        .response_pattern = NULL,
                        .sort_keys        = character(0)) {
  stopifnot(!is.null(.response_pattern),
            is.character(.response_pattern),
            nzchar(.response_pattern))

  clamp <- function(x) pmax(.min, pmin(.max, x))

  files       <- list.files(.path.model, pattern = "\\.Rds$", full.names = TRUE)
  fname_regex <- sprintf("^([^_]+)_(%s)_(.+)\\.Rds$", .response_pattern)
  oof_list    <- list()

  for (f in files) {
    # {method}_{response}_{dataset}.Rds
    m <- regmatches(basename(f),
                    regexec(fname_regex, basename(f)))[[1]]
    if (length(m) < 4) next
    method <- m[2]; response <- m[3]; dataset <- m[4]

    train_file <- file.path(.path.train,
                            paste0(response, "_", dataset, "_TRAIN.csv"))
    if (!file.exists(train_file)) next
    DT <- fread(train_file)
    y  <- suppressWarnings(as.numeric(DT$y))

    model <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(model)) next

    pr <- tryCatch(
      merge(model$pred, model$bestTune, by = names(model$bestTune), sort = FALSE),
      error = function(e) NULL
    )
    if (is.null(pr) || nrow(pr) == 0) next
    pr_pred <- suppressWarnings(as.numeric(as.character(pr$pred)))
    pr_obs  <- suppressWarnings(as.numeric(as.character(pr$obs)))
    pr_row  <- suppressWarnings(as.integer(pr$rowIndex))
    ok      <- is.finite(pr_pred) & is.finite(pr_obs) & is.finite(pr_row)
    if (!any(ok)) next

    oof_list[[length(oof_list) + 1L]] <- data.table(
      method   = method,
      response = response,
      dataset  = dataset,
      rowIndex = pr_row[ok],
      obs      = pr_obs[ok],
      pred     = clamp(pr_pred[ok])
    )
  }

  OOF <- rbindlist(oof_list)
  if (nrow(OOF) == 0) return(data.table())

  # Collapse duplicate (rowIndex) within a cell+method (can happen with
  # some tune grids): average.
  OOF <- OOF[, .(obs = mean(obs), pred = mean(pred)),
             by = .(method, response, dataset, rowIndex)]

  # -- Ensemble reconstruction per cell -------------------------------------
  out_rows <- list()
  cells <- unique(OOF[, .(response, dataset)])

  for (i in seq_len(nrow(cells))) {
    r  <- cells$response[i]
    ds <- cells$dataset[i]
    sub <- OOF[response == r & dataset == ds]
    if (uniqueN(sub$method) < 1L) next   # only skip cells with zero methods; a
                                          # 1-method "ensemble" trivially weights
                                          # to that single method (w = 1).
    mk <- sub[, .(rmse_k = sqrt(mean((obs - pred)^2))), by = method]
    if (any(!is.finite(mk$rmse_k)) || any(mk$rmse_k <= 0)) next
    mk[, w := (1 / rmse_k) / sum(1 / rmse_k)]

    pw <- dcast(sub, rowIndex + obs ~ method, value.var = "pred")
    methods_here <- mk$method
    # Keep rows with at least one method's prediction (was complete.cases,
    # which dropped any row with NA in any method -- losing rare-factor
    # rows where earth NA'd on its fold but ranger+gbm produced valid
    # predictions). Per-row fallback below renormalizes weights across
    # the non-NA methods for those rows; for fully-complete rows the
    # matrix-vector fast path preserves byte equivalence with prior runs.
    pw <- pw[rowSums(!is.na(pw[, ..methods_here])) > 0]
    if (nrow(pw) == 0) next

    mat  <- as.matrix(pw[, ..methods_here])
    w    <- mk$w[match(methods_here, mk$method)]
    # Fast path: matrix-vector product (byte-identical to the prior code
    # when no NAs are present in mat).
    yhat <- as.numeric(mat %*% w)
    na_idx <- which(is.na(yhat))
    for (i in na_idx) {
      ok <- !is.na(mat[i, ])
      yhat[i] <- if (any(ok)) sum(mat[i, ok] * w[ok]) / sum(w[ok]) else NA_real_
    }
    yhat <- clamp(yhat)

    # -- Honest SampleID recovery -----------------------------------------
    # Preferred path: sidecar `<response>_<dataset>_TRAIN_ids.csv` written
    # by buildDataset alongside TRAIN.csv. Row order matches TRAIN.csv
    # exactly (same `na.omit` filter applied), so rowIndex maps directly
    # to SampleID without any inference.
    ids_file <- file.path(.path.train,
                          paste0(r, "_", ds, "_TRAIN_ids.csv"))
    if (file.exists(ids_file)) {
      sids_full <- as.character(fread(ids_file)$SampleID)
    } else {
      # Fallback for older buildDataset outputs without the sidecar.
      # WARNING: this fallback is unsafe when na.omit drops rows for
      # reasons other than NA in the response (e.g. chain features in
      # iter >= 1 introduce NAs in features). Length matches break the
      # rowIndex -> SampleID mapping. Sidecar is required for correctness.
      full_file <- file.path(.path.datasets, paste0(ds, ".csv"))
      if (!file.exists(full_file)) {
        stop("oofEnsemble: dataset file not found: ", full_file)
      }
      full <- fread(full_file)
      required_cols <- c(.sort_keys, "SampleID", r)
      if (!all(required_cols %in% names(full))) {
        stop("oofEnsemble: dataset ", ds, " is missing required columns (",
             paste(required_cols, collapse = ", "), ")")
      }
      if (length(.sort_keys) > 0) {
        inv_ok <- isTRUE(full[, all.equal(.I, do.call(order, .SD)),
                              .SDcols = .sort_keys])
        if (!isTRUE(inv_ok)) {
          stop("oofEnsemble: dataset ", ds, " is not sorted by (",
               paste(.sort_keys, collapse = ", "),
               "); row-order invariant broken - SampleID recovery unsafe.")
        }
      }
      sids_full <- as.character(full[!is.na(get(r)), SampleID])
    }
    if (max(pw$rowIndex) > length(sids_full)) {
      stop("oofEnsemble: rowIndex out of range for ", r, "x", ds,
           " (max=", max(pw$rowIndex), ", sids length=", length(sids_full), ")")
    }

    out_rows[[length(out_rows) + 1L]] <- data.table(
      response  = r,
      dataset   = ds,
      rowIndex  = pw$rowIndex,
      SampleID  = sids_full[pw$rowIndex],
      y_obs     = pw$obs,
      y_hat_oof = yhat,
      residual  = pw$obs - yhat
    )
  }

  rbindlist(out_rows)
}
# nolint end
