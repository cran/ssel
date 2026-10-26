# nolint start
# postProcess: assemble user-facing deliverables from ssel intermediates.
#
# Reads response_long, residuals_oof, metrics, prediction_quantiles
# (ssel-computed at .path.summary), Yo.csv (ssel intermediate at
# .path.iter), and a project-supplied location source. Produces two
# files in .path.export:
#
#   predictions.csv  wide best per response, one row per
#                    (SampleID, p[, Transformed]).
#                    Cols: key + location + p + [Transformed] +
#                          <response_i>, R2.<phase_i>, ...
#                    When `transform` or `normalize` is non-null/non-
#                    identity, a Transformed=TRUE twin is written
#                    alongside the raw Transformed=FALSE rows.
#
#   audit.csv        long disaggregated by (key, response, dataset,
#                    method, p). Cols: key + location + response +
#                    dataset + method + p + value + r2. Raw values
#                    (no transform / normalize applied) -- audit
#                    inspects the model evidence, not the deliverable.
#
# Transform configuration (`transform`, `normalize`): record with `op`
# field plus op-specific params. Both default to `identity` (no-op).
# When provided as NULL or "identity" the corresponding pass is
# skipped. Element-wise `transform` runs before row-wise `normalize`.
#
# Supported ops:
#   transform: identity | log (base) | exp (base)
#   normalize: identity | closure (target)
#
# Location source: a CSV path (or a list of candidate paths) containing
# the key column plus the location columns. All existing files carrying
# those columns are combined, retaining the first row per key. Conventions like
# `data/domains/index.csv` and `data/iter/Xo.csv` are accepted.

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Element-wise transform of a numeric vector.
.applyTransform <- function(v, spec) {
  if (is.null(spec) || identical(spec$op, "identity")) return(v)
  switch(spec$op,
    log = log(v, base = spec$base %||% exp(1)),
    exp = (spec$base %||% exp(1))^v,
    stop(sprintf("postProcess: unknown transform op: %s", spec$op),
         call. = FALSE)
  )
}

# Row-wise normalize a wide data.table over the specified columns.
.applyNormalize <- function(DT, cols, spec) {
  if (is.null(spec) || identical(spec$op, "identity")) return(DT)
  if (identical(spec$op, "closure")) {
    target <- spec$target
    if (is.null(target)) stop("postProcess: normalize op=closure requires 'target'",
                              call. = FALSE)
    DT <- copy(DT)
    DT[, .row_total := Reduce(`+`, .SD), .SDcols = cols]
    DT[, (cols) := lapply(.SD, function(v)
        fifelse(.row_total == 0, v, v / .row_total * target)),
       .SDcols = cols]
    DT[, .row_total := NULL]
    return(DT)
  }
  stop(sprintf("postProcess: unknown normalize op: %s", spec$op), call. = FALSE)
}

.hasTransformedTwin <- function(transform, normalize) {
  has_t <- !is.null(transform) && !identical(transform$op, "identity")
  has_n <- !is.null(normalize) && !identical(normalize$op, "identity")
  has_t || has_n
}

.resolveLocation <- function(locationSources, key, location) {
  # Union ALL sources that carry the location cols. A single source may
  # cover only train rows (X.csv) or only predict rows (Xo.csv); taking
  # the first match would leave the other set with NA location values.
  # data/domains/index.csv (mineralogy) carries all rows, so the union
  # is deduped to the same result. This is the robust path for all projects.
  FILES <- Filter(function(f) {
    file.exists(f) && all(c(key, location) %in% names(fread(f, nrows = 0)))
  }, locationSources)
  if (length(FILES) == 0L) {
    stop(sprintf("postProcess: no source has location cols (%s) in any of: %s",
                 paste(location, collapse = ", "),
                 paste(locationSources, collapse = ", ")),
         call. = FALSE)
  }
  unique(rbindlist(lapply(FILES, function(f)
    fread(f)[, c(key, location), with = FALSE])), by = key)
}

#' Assemble user-facing prediction deliverables.
#'
#' Builds two CSVs in `.path.export`:
#' `predictions.csv` (wide best per response, optional Transformed twin)
#' and `audit.csv` (long disaggregated per method, raw values).
#'
#' @param .path.summary Directory holding ssel computed aggregates
#'   (`response_long.csv`, `residuals_oof.csv`, `metrics.csv`,
#'   `prediction_quantiles.csv`).
#' @param .path.export Directory holding ssel intermediate `Yo.csv`
#'   and where outputs (`predictions.csv`, `audit.csv`) are written.
#'   Created if missing.
#' @param key Identity column name (typically `"SampleID"`).
#' @param responses Character vector of response column names.
#' @param location Character vector of location columns to carry from
#'   the project's identity source into `predictions.csv` / `audit.csv`.
#' @param locationSources Character vector of candidate file paths
#'   from which to read `(key, location)`. All existing files carrying
#'   the required columns are combined in source order, retaining the first
#'   row for each key.
#' @param transform Element-wise transform spec, a list with `op` and
#'   op-specific params. Supported: `list(op="identity")` (default
#'   when NULL), `list(op="log", base=10)`, `list(op="exp", base=exp(1))`.
#' @param normalize Row-wise normalize spec. Supported:
#'   `list(op="identity")` (default), `list(op="closure", target=100)`.
#' @param boundsMin Lower clamp for predicted values. Default `-Inf`.
#' @param boundsMax Upper clamp for predicted values. Default `Inf`.
#' @return Invisible NULL. Side effect: writes `predictions.csv` and
#'   `audit.csv` to `.path.export`.
#' @import data.table
#' @importFrom stats as.formula
#' @export
postProcess <- function(
    .path.summary,
    .path.export,
    key,
    responses,
    location,
    locationSources = c("data/domains/index.csv",
                        file.path(.path.export, "X.csv"),
                        file.path(.path.export, "Xo.csv")),
    transform = NULL,
    normalize = NULL,
    boundsMin = -Inf,
    boundsMax = Inf
) {
  .keysOld <- options(datatable.keepLeadingZeros = TRUE)
  on.exit(options(.keysOld), add = TRUE)
  stopifnot(is.character(key), nzchar(key))
  stopifnot(is.character(responses), length(responses) > 0L)
  stopifnot(is.character(location), length(location) > 0L)
  .checkBound(boundsMin, "boundsMin")
  .checkBound(boundsMax, "boundsMax")

  .clamp <- function(x) pmax(boundsMin, pmin(boundsMax, x))

  # Inputs ----
  # Yo.csv is the ssel intermediate (predict predictions long with bands).
  # It currently lives in .path.export alongside the project deliverables;
  # we read but never overwrite it.
  EXP.yo   <- file.path(.path.export,  "Yo.csv")
  SUM.rl   <- file.path(.path.summary, "response_long.csv")
  SUM.oof  <- file.path(.path.summary, "residuals_oof.csv")
  SUM.met  <- file.path(.path.summary, "metrics.csv")
  SUM.pq   <- file.path(.path.summary, "prediction_quantiles.csv")
  for (.f in c(EXP.yo, SUM.rl, SUM.oof, SUM.met, SUM.pq)) {
    if (!file.exists(.f)) stop(sprintf("postProcess: required file missing: %s", .f),
                               call. = FALSE)
  }

  Yo  <- fread(EXP.yo)
  RL  <- fread(SUM.rl)
  ROF <- fread(SUM.oof)
  MET <- fread(SUM.met)
  PQ  <- fread(SUM.pq)[, .(response, dataset, q_50, q_90, q_95)]

  LOC <- .resolveLocation(locationSources, key, location)

  # predictions.csv: best per (key, response, p), wide ----
  # Train rows from residuals_oof: 3 p bands via +q_p offsets.
  TR <- copy(ROF)
  TR[PQ, on = .(response, dataset),
     `:=`(q_50 = i.q_50, q_90 = i.q_90, q_95 = i.q_95)]
  TR[MET[method == "ensemble.RMSE", .(response, dataset, r2)],
     on = .(response, dataset), r2 := i.r2]
  TR[, `:=`(value.50 = .clamp(y_hat_oof + q_50),
            value.90 = .clamp(y_hat_oof + q_90),
            value.95 = .clamp(y_hat_oof + q_95))]

  # Predict rows: ensemble.RMSE point predictions + 3 p bands.
  PR <- RL[method == "ensemble.RMSE"]
  PR[PQ, on = .(response, dataset),
     `:=`(q_50 = i.q_50, q_90 = i.q_90, q_95 = i.q_95)]
  PR[MET[method == "ensemble.RMSE", .(response, dataset, r2)],
     on = .(response, dataset), r2 := i.r2]
  PR[, `:=`(value.50 = .clamp(value + q_50),
            value.90 = .clamp(value + q_90),
            value.95 = .clamp(value + q_95))]

  COLS.keep <- c(key, "response", "dataset", "r2",
                 "value.50", "value.90", "value.95")
  BEST <- rbindlist(list(TR[, ..COLS.keep], PR[, ..COLS.keep]))[
          order(-r2), .SD[1L], by = c(key, "response")]

  LONG <- rbindlist(lapply(c(50, 90, 95), function(pct)
    BEST[, .(get(key), response, dataset, r2,
             p = pct / 100, value = get(paste0("value.", pct)))]))
  setnames(LONG, "V1", key)

  LONG[LOC, on = key, (location) := mget(paste0("i.", location))]

  PIVOT.formula <- as.formula(paste(
    paste(c(key, location, "p"), collapse = " + "), "~ response"))
  WIDE.raw <- dcast(LONG, PIVOT.formula, value.var = c("value", "r2"))

  PREFIX <- sub("^([^.]+)\\..*$", "\\1", responses[1])
  PHASES <- sub(paste0("^", PREFIX, "\\."), "", responses)
  setnames(WIDE.raw, paste0("value_", responses), responses)
  setnames(WIDE.raw, paste0("r2_",    responses), paste0("R2.", PHASES))

  ORD <- .portableOrder(PHASES)
  PIVOT.keys <- c(key, location, "p")
  COL.order <- c(PIVOT.keys,
                 unlist(lapply(ORD, function(i)
                   c(responses[i], paste0("R2.", PHASES[i])))))

  HAS.twin <- .hasTransformedTwin(transform, normalize)
  if (HAS.twin) {
    WIDE.raw[, Transformed := FALSE]
    WIDE.tx <- copy(WIDE.raw)
    if (!is.null(transform) && !identical(transform$op, "identity")) {
      WIDE.tx[, (responses) := lapply(.SD, .applyTransform, spec = transform),
             .SDcols = responses]
    }
    WIDE.tx <- .applyNormalize(WIDE.tx, responses, normalize)
    WIDE.tx[, Transformed := TRUE]
    WIDE <- rbindlist(list(WIDE.raw, WIDE.tx))
    PIVOT.keys <- c(PIVOT.keys, "Transformed")
    COL.order  <- c(PIVOT.keys[1:(length(PIVOT.keys) - 1L)], "Transformed",
                    unlist(lapply(ORD, function(i)
                      c(responses[i], paste0("R2.", PHASES[i])))))
  } else {
    WIDE <- WIDE.raw
  }

  setcolorder(WIDE, COL.order)
  if (HAS.twin) {
    setorderv(WIDE, c(key, "p", "Transformed"))
  } else {
    setorderv(WIDE, c(key, "p"))
  }

  # Post-condition for closure-style normalize: sums must hit target.
  if (HAS.twin && !is.null(normalize) &&
      identical(normalize$op, "closure")) {
    .sums <- WIDE[Transformed == TRUE, rowSums(.SD), .SDcols = responses]
    .bad  <- .sums[abs(.sums - normalize$target) > 1e-6 & .sums > 1e-9]
    if (length(.bad) > 0L) {
      stop(sprintf("postProcess: closure failed: %d rows sum != %g (first=%g)",
                   length(.bad), normalize$target, .bad[1]), call. = FALSE)
    }
  }

  if (!dir.exists(.path.export)) dir.create(.path.export, recursive = TRUE)
  fwrite(WIDE, file.path(.path.export, "predictions.csv"))

  # audit.csv: long disaggregated by method, raw values ----
  PR.audit <- copy(RL)
  PR.audit[PQ, on = .(response, dataset),
           `:=`(q_50 = i.q_50, q_90 = i.q_90, q_95 = i.q_95)]

  TR.audit <- copy(ROF)
  TR.audit[PQ, on = .(response, dataset),
           `:=`(q_50 = i.q_50, q_90 = i.q_90, q_95 = i.q_95)]
  TR.audit[, `:=`(method = "ensemble.RMSE", value = y_hat_oof)]

  AUDIT <- rbindlist(lapply(c(50, 90, 95), function(pct) {
    qcol <- paste0("q_", pct)
    rbindlist(list(
      PR.audit[, .(get(key), response, dataset, method,
                   p = pct / 100, value = .clamp(value + get(qcol)))],
      TR.audit[, .(get(key), response, dataset, method,
                   p = pct / 100, value = .clamp(value + get(qcol)))]
    ))
  }))
  setnames(AUDIT, "V1", key)

  AUDIT[MET, on = .(response, dataset, method), r2 := i.r2]
  AUDIT[LOC, on = key, (location) := mget(paste0("i.", location))]
  setcolorder(AUDIT, c(key, location, "response", "dataset",
                       "method", "p", "value", "r2"))
  setorderv(AUDIT, c(key, "response", "dataset", "method", "p"))
  fwrite(AUDIT, file.path(.path.export, "audit.csv"))

  invisible(NULL)
}
# nolint end
