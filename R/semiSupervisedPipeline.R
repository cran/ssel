# nolint start

#' Fit a range-ratio pseudo-label promotion loop
#'
#' Refit the supervised ssel workflow while promoting selected prediction rows
#' as ordinary `ensemble.RMSE` pseudo-labels. Selection uses a package-defined
#' cross-method/dataset prediction range; acceptance uses a separate
#' out-of-fold squared-correlation gauge. This is a pseudo-label policy, not
#' tri-training, co-training regression, held-out validation, or calibrated
#' predictive uncertainty.
#'
#' @param .path.datasets Directory scanned non-recursively for assembled CSV
#'   files whose basenames occur in `datasets`. It is ignored after a
#'   non-`NULL` `.path.iter` selects an iteration-specific dataset directory.
#' @param .path.work Working directory. Every fit recursively removes and
#'   recreates its `round` child without rollback.
#' @param key Character key column used for joins and pool membership. The
#'   final OOF gauge normally requires `"SampleID"` because that is the key
#'   column in the accepted residual schema. Missing key values are not
#'   rejected; their downstream behavior is described under input discovery.
#' @param responses Character vector of target response columns. Exactly one is
#'   required when `.path.iter` is supplied. A standalone call can receive
#'   multiple responses; each final TEST split uses its fitted TRAIN schema.
#' @param datasets Character dataset identifiers matched to CSV basenames.
#'   Matching preserves [list.files()] order, not this vector's order.
#' @param methods Character vector of caret method identifiers. The initial
#'   check requires at least two entries; promotion additionally requires at
#'   least two distinct method names among available predictions.
#' @param seed Value passed unchanged to every full [modelPipeline()] fit. It
#'   is not passed to the final serial re-emission.
#' @param tau Numeric strict upper threshold for the dimensionless range ratio
#'   \eqn{D}. Equality does not promote. Length, missingness, and finiteness are
#'   not proactively validated.
#' @param maxRounds Value passed to [seq_len()] as the maximum number of
#'   candidate rounds after the baseline fit. No terminal history row is added
#'   merely because this sequence is exhausted.
#' @param epsRevert Numeric absolute tolerance in squared-correlation units.
#'   A candidate reverts only below `bestR2 - epsRevert`; the value is not
#'   validated as finite or nonnegative.
#' @param promotionCap Numeric multiplier used in the key budget
#'   `floor(promotionCap * length(U))`. It is not restricted to \eqn{[0,1]}.
#' @param boundsMin,boundsMax Numeric response bounds forwarded to supervised
#'   fitting and final evaluation. Let \eqn{a} be `boundsMin` and \eqn{b} be
#'   `boundsMax`. When \eqn{a\le b}, both ordinary and reconstructed OOF
#'   predictions use interval projection
#'   \eqn{\Pi_{[a,b]}(z)=\min\{b,\max\{a,z\}\}}. Order and finiteness are not
#'   checked. If \eqn{a>b}, the two executed nestings differ: ordinary
#'   evaluation computes \eqn{\min\{b,\max\{a,z\}\}=b}, whereas OOF
#'   reconstruction computes \eqn{\max\{a,\min\{b,z\}\}=a}. Neither is then an
#'   interval projection.
#' @param .path.iter `NULL`, or a [chainPipeline()] iteration directory
#'   containing `metrics_<integer>.csv` and the corresponding
#'   `datasets_<integer>` directories. Selection is described in
#'   **Input discovery and optional chain warm start**.
#' @param allResponses Character vector intended to name every response column.
#'   Before each fit, named non-target responses are removed. Missing names are
#'   ignored; an omitted non-target response remains and is numerically coerced
#'   as a predictor. The default is `responses`.
#'
#' @section Input discovery and optional chain warm start:
#' With `.path.iter = NULL`, non-recursive `.csv` files in `.path.datasets` are
#' filtered by basename membership in `datasets`; [list.files()] order is
#' retained. With `.path.iter` supplied, the function reads every lexically
#' ordered `metrics_<integer>.csv`, retains rows for `method ==
#' "ensemble.RMSE"` and the single target response, and selects the first row
#' attaining the global maximum `r2`. Its integer becomes `iterStar`, and the
#' input directory becomes `datasets_<iterStar>`.
#'
#' This selects one augmented dataset snapshot. It does not consume a stitched
#' model, stitch response-specific features, or rerun a chain sweep. The
#' selected directory must exist; in particular, no fallback maps iteration
#' zero to the original dataset directory. With no retained metric row or only
#' missing `r2`, [which.max()] returns no position, no CSV is discovered, and
#' the later non-empty-file assertion fails.
#'
#' The first retained CSV alone defines the labelled-key vector
#' \eqn{L=\{u:\mathrm{set}(u)=\text{"train"}\}} and original prediction-key
#' vector \eqn{U=\{u:\mathrm{set}(u)=\text{"predict"}\}}. Row order and
#' duplicate keys are retained. Missing key values are also retained without a
#' missingness check. They enter membership filters, update joins,
#' `length(U)`, unique-key accounting, and the final completeness count under
#' ordinary R/data.table semantics. Round CSV writing and subsequent type
#' inference can materialize a missing character key as an empty string while
#' preserving a missing numeric key, so no cross-file missing-key normalization
#' is guaranteed. Other files are not checked for the same key pools, and
#' missing `set` or `key` columns fail through data.table evaluation. The base
#' budget size \eqn{|U|} is `length(U)`, including duplicates and missing
#' values.
#'
#' Before every fit, each retained CSV is copied after removing only names in
#' `setdiff(allResponses, responses)`. Every remaining column except `key` and
#' `set`, including targets and predictors, is replaced by `as.numeric()`.
#' Text and factor-like input can therefore warn and become missing. Missing
#' features, targets, or role columns are left to the composed supervised
#' helpers and data.table operations to reject or omit under their contracts.
#'
#' @section Destructive round fit:
#' For each baseline, candidate, or restoration fit, the function recursively
#' deletes `.path.work/round`, recreates its `datasets` directory, and writes
#' all selected CSV copies. Accepted pseudo-label rows update targets by
#' `(key, response)`, and every matching key is changed to `set = "train"`.
#' Duplicate label keys and labels from multiple datasets are not collapsed;
#' update-join order can make the last matching value material.
#'
#' The rewritten tables are passed to [modelPipeline()] in its default
#' `"full"` mode with `ensemble = "ensemble.RMSE"` and the same `seed` on every
#' fit. Round products therefore follow that helper's split, fit, OOF,
#' prediction, summary, aggregation, bounded-PSOCK, and deletion contracts.
#' Ordinary prediction `ensemble.RMSE` rows used below are distinct from the
#' reconstructed OOF inverse-RMSE estimator underlying the acceptance gauge.
#'
#' @section Range-ratio promotion score:
#' For original prediction key \eqn{u}, response \eqn{r}, method \eqn{m},
#' dataset \eqn{d}, and round \eqn{s}, let \eqn{f_{mdr}^{(s)}(u)} be every
#' available projected ordinary base-method prediction. Pool all method and
#' dataset rows into
#' \deqn{\mathcal A_{ur}^{(s)}=\{f_{mdr}^{(s)}(u)\},\qquad
#' S_{ur}^{(s)}=\max\mathcal A_{ur}^{(s)}-\min\mathcal A_{ur}^{(s)}.}
#' A pair is retained only when at least two distinct method names occur; two
#' datasets alone do not suffice.
#'
#' Let \eqn{\bar f_{ur,\mathrm{last}}^{(s)}(u)} be the last matching ordinary
#' `ensemble.RMSE` value in response-file concatenation and update-join order.
#' ssel defines
#' \deqn{D_{ur}^{(s)}
#' =\frac{S_{ur}^{(s)}}
#' {\max\{1,|\bar f_{ur,\mathrm{last}}^{(s)}(u)|\}}.}
#' This dimensionless one-dimensional range ratio is package policy, not a
#' variance, probability, interval, or calibrated uncertainty. The fixed
#' denominator floor makes it non-invariant to response rescaling and
#' translation. Pooling across datasets and choosing the last ensemble match
#' also make row composition and order material.
#'
#' Neither maximum nor minimum removes missing values. A missing base value
#' makes the score missing; it cannot satisfy `D < tau`. Identical finite base
#' predictions give \eqn{D=0}, which promotes only when `tau > 0`. A score
#' equal to `tau` does not promote. Equal scores retain their preceding group
#' order. Previously accepted `(key, response)` pairs are removed before this
#' strict gate.
#'
#' @section Key budget and pseudo-label rows:
#' Let \eqn{c} be `promotionCap`, let \eqn{P} be the accepted pseudo-label
#' table, and let \eqn{K(P)} be its unique keys. The remaining budget is
#' \deqn{B_s=\lfloor c|U|\rfloor-|K(P)|.}
#' New keys are encountered in ascending-score, first-row order. When they
#' exceed \eqn{B_s}, only the first \eqn{B_s} new keys are retained, together
#' with all their eligible response pairs and new response pairs on already
#' promoted keys. A nonpositive budget or absence of a new key produces
#' `saturation-stop`. Because \eqn{|U|} counts rows while spending counts unique
#' keys, duplicate prediction keys can allow `promotionCap < 1` to admit every
#' unique key. Invalid cap values retain base arithmetic and condition
#' behavior.
#'
#' Pseudo-labels are all response-file rows matching `method ==
#' "ensemble.RMSE"` and the candidate pairs; no median or cross-dataset
#' reduction is applied. Multiple datasets can therefore contribute multiple
#' label rows for one pair. `history$promoted` counts candidate pairs, whereas
#' accepted `history$cumulative` and the returned `promoted` table count label
#' rows.
#'
#' @section OOF gauge and accept/revert rule:
#' Let \eqn{H_r^{(s)}} contain all rows of `residuals_oof.csv` for response
#' \eqn{r} whose configured key occurs in the first-file labelled vector
#' \eqn{L}. Rows from all datasets remain pooled. For each response, compute
#' squared sample correlation and then define
#' \deqn{g_s=\operatorname{mean}_r
#' \{\operatorname{cor}(y,\hat y_{\mathrm{OOF}})^2\},}
#' removing response-level missing values and weighting each surviving
#' response equally rather than by row count. The gauge is dimensionless. It
#' is not held-out risk, a fully nested OOF estimate, or a no-degradation
#' guarantee. Duplicate pooled rows retain their multiplicity. Missing values
#' or constant vectors can make a response correlation missing; if every
#' response is missing, \eqn{g_s} is `NaN` and a later logical comparison can
#' error. A key other than `"SampleID"` generally fails when the accepted
#' residual product has no such column.
#'
#' Baseline round zero sets historical `bestR2` to \eqn{g_0}. Candidate round
#' \eqn{s} reverts only when
#' \deqn{g_s < \mathrm{bestR2}-\varepsilon,}
#' where \eqn{\varepsilon} is `epsRevert`. Equality is accepted. An accepted
#' round may be worse than the historical maximum by up to this absolute
#' tolerance; `bestR2` becomes `max(bestR2, g_s)`. Thus the returned `finalR2`
#' is a historical maximum and need not equal the final fitted state's gauge.
#' A rejected candidate writes a `REVERT-stop` history row, then the accepted
#' pool is materially refitted, deleting the rejected round state. That
#' restoration gauge is not recomputed.
#'
#' @section Stops and history:
#' `history` begins with action `"baseline"`, zero counts, and \eqn{g_0}.
#' Accepted rounds use `"accept"`, the candidate-pair count, accepted label-row
#' count, and candidate gauge. If no pair passes, `"empty-stop"` records zero
#' promoted rows, the accepted label-row count, and historical `bestR2`. A
#' nonpositive/exhausted budget or no new key similarly records
#' `"saturation-stop"`. A rejection uses uppercase `"REVERT-stop"`, records the
#' attempted pair count, the preceding accepted label-row count, and the
#' rejected gauge. Exhausting `maxRounds` adds no separate stop row.
#'
#' No-promotion runs return `promoted = NULL`. Depending on cap and duplicate
#' keys, every unique prediction key can be promoted. Subsequent scoring uses
#' only rows still emitted as predictions by the latest accepted full fit.
#'
#' @section Final serial re-emission:
#' After the loop and any restoration, the function rebuilds one TEST file for
#' every declared dataset/response cell. It uses all rows from each original
#' selected CSV whose key belongs to the first-file \eqn{U}, taking feature
#' names from that cell's final TRAIN split, excluding its `y` column.
#' Other target responses and discarded features do not enter TEST.
#' Missing declared datasets or required
#' feature columns fail through file/data.table operations.
#'
#' [trainRegressionModel()] then runs with `TRAIN = FALSE` and
#' `PARALLEL = FALSE`, overwriting cell metric and response CSVs. For each cell,
#' it asserts that the number of unique `ensemble.RMSE` keys equals
#' `length(U)` and that the key sets agree. Duplicate keys in \eqn{U} make the
#' assertion fail even when all unique keys were emitted.
#'
#' The shared prediction-summary calculation and [aggregateResponses()] then
#' refresh summaries and exports for the original prediction pool, including
#' promoted keys. Aggregation uses the original selected datasets for observed
#' and prediction row membership. The accepted fitted models, OOF residuals,
#' [auditQuantiles()] offsets and [auditOverfit()] results are retained;
#' refreshing the delivery does not fit or evaluate models again.
#'
#' @section Conditions, RNG, and side effects:
#' Apart from the method-count and optional single-response/file assertions,
#' most validation is delegated to base R, data.table, caret, and the composed
#' ssel helpers. Paths, scalar lengths, response/dataset agreement, bounds,
#' thresholds, caps, and tolerances are not proactively normalized. Failures
#' can leave partial round files; there is no transaction or rollback.
#'
#' Every full round receives the same `seed`, so [modelPipeline()] resets RNG
#' before its sampled method/dataset order and downstream fitting. Reproducible
#' results remain conditional on the R, RNG, caret, learner, and parallel
#' environment. Rounds are sequential, each full round uses the supervised
#' bounded-PSOCK contracts, and final re-emission is explicitly serial. The
#' helper emits the conditions of its composed functions and returns no fitted
#' model or prediction object.
#'
#' @return Visibly, a list with:
#' \describe{
#'   \item{`history`}{A `data.table` with integer `round`, `promoted`, and
#'     `cumulative`; numeric `oofR2`; and character `action`, under the counting
#'     and action rules above.}
#'   \item{`promoted`}{`NULL`, or a `data.table` with the configured key,
#'     `response`, and numeric `value`, one row per accepted matching ensemble
#'     label row.}
#'   \item{`baselineR2`}{The numeric baseline gauge \eqn{g_0}.}
#'   \item{`finalR2`}{The numeric historical maximum gauge, not necessarily the
#'     final fitted-state gauge.}
#'   \item{`iterStar`}{`NA_integer_` without `.path.iter`; otherwise the
#'     selected iteration integer.}
#' }
#' @seealso [modelPipeline()] for each destructive full fit,
#'   [trainRegressionModel()] for ordinary prediction and final serial
#'   re-emission, [oofEnsemble()] for the reconstructed OOF estimator, and
#'   [chainPipeline()] for the optional augmented-dataset source.
#' @examples
#' \donttest{
#' root <- tempfile("ssel-semi-")
#' datasets_dir <- file.path(root, "datasets")
#' dir.create(datasets_dir, recursive = TRUE)
#'
#' n_train <- 30L
#' assembled <- data.frame(
#'   SampleID = c(paste0("S", seq_len(n_train)), "P1", "P2"),
#'   set = c(rep("train", n_train), "predict", "predict"),
#'   x = seq_len(n_train + 2L),
#'   z = rep(c(0, 1), length.out = n_train + 2L),
#'   target = c(
#'     1.5 * seq_len(n_train) + rep(c(-0.4, 0.2, 0.6), 10),
#'     NA, NA
#'   )
#' )
#' utils::write.csv(
#'   assembled, file.path(datasets_dir, "demo.csv"),
#'   row.names = FALSE, na = ""
#' )
#'
#' old_quiet <- getOption("aR.quiet")
#' options(aR.quiet = TRUE)
#' result <- semiSupervisedPipeline(
#'   .path.datasets = datasets_dir,
#'   .path.work = file.path(root, "work"),
#'   key = "SampleID",
#'   responses = "target",
#'   allResponses = "target",
#'   datasets = "demo",
#'   methods = c("lm", "glm"),
#'   seed = 2026,
#'   tau = 0.1,
#'   maxRounds = 1,
#'   epsRevert = Inf,
#'   promotionCap = 0.5
#' )
#' options(aR.quiet = old_quiet)
#'
#' result$history
#' result$promoted
#' c(baseline = result$baselineR2, historical_best = result$finalR2)
#' # The one-round policy promotes at most one of the two original keys.
#' # iterStar is NA because no chain iteration directory was supplied.
#'
#' unlink(root, recursive = TRUE)
#' }
#' @export
#' @export
semiSupervisedPipeline <- function(.path.datasets, .path.work,
                                   key, responses, datasets, methods,
                                   seed, tau, maxRounds = 10L,
                                   epsRevert = 0.02, promotionCap = 0.8,
                                   boundsMin = -Inf, boundsMax = Inf,
                                   .path.iter = NULL, allResponses = responses) {
  .keysOld <- options(datatable.keepLeadingZeros = TRUE)
  on.exit(options(.keysOld), add = TRUE)
  stopifnot(length(methods) >= 2L)
  .checkBound(boundsMin, "boundsMin")
  .checkBound(boundsMax, "boundsMax")

  iterStar <- NA_integer_
  if (!is.null(.path.iter)) {
    stopifnot(length(responses) == 1L)
    MFILES <- .listFiles(.path.iter, pattern = "^metrics_[0-9]+[.]csv$", full.names = TRUE)
    stopifnot(length(MFILES) > 0L)
    MT <- rbindlist(lapply(MFILES, function(f)
      fread(f)[method == "ensemble.RMSE" & response == responses,
               .(iter = as.integer(gsub("\\D", "", basename(f))), r2)]))
    iterStar <- MT[which.max(r2), iter]
    .path.datasets <- file.path(.path.iter, sprintf("datasets_%d", iterStar))
  }

  FILES <- .listFiles(.path.datasets, full.names = TRUE, pattern = "[.]csv$")
  FILES <- FILES[gsub("[.]csv$", "", basename(FILES)) %in% datasets]
  stopifnot(length(FILES) > 0L)

  D0 <- fread(FILES[1L])
  KEYS.L <- D0[set == "train", get(key)]
  KEYS.U <- D0[set == "predict", get(key)]

  .fitRound <- function(PSEUDO) {
    PATH.r <- file.path(.path.work, "round")
    .guardWipe(PATH.r, .path.datasets)
    unlink(PATH.r, recursive = TRUE)
    dir.create(file.path(PATH.r, "datasets"), recursive = TRUE, showWarnings = FALSE)
    for (f in FILES) {
      DT <- fread(f)
      # the non-target responses never enter the feature set
      DT <- DT[, setdiff(names(DT), setdiff(allResponses, responses)), with = FALSE]
      COLS <- setdiff(names(DT), c(key, "set"))
      DT[, (COLS) := lapply(.SD, as.numeric), .SDcols = COLS]
      if (!is.null(PSEUDO) && nrow(PSEUDO)) {
        for (r in unique(PSEUDO$response)) {
          DT[PSEUDO[response == r], on = key, (r) := i.value]
        }
        DT[get(key) %in% unique(PSEUDO[[key]]), set := "train"]
      }
      fwrite(DT, file.path(PATH.r, "datasets", basename(f)))
    }
    modelPipeline(
      .path.datasets = file.path(PATH.r, "datasets"),
      .path.train    = file.path(PATH.r, "train"),
      .path.model    = file.path(PATH.r, "model"),
      .path.metrics  = file.path(PATH.r, "metrics"),
      .path.response = file.path(PATH.r, "response"),
      .path.summary  = file.path(PATH.r, "summary"),
      .path.export   = file.path(PATH.r, "export"),
      key = key, responses = responses, datasets = datasets,
      colsId = character(0), setCol = "set", methods = methods, seed = seed,
      ensemble = "ensemble.RMSE", boundsMin = boundsMin, boundsMax = boundsMax
    )
    PATH.r
  }

  .predictions <- function(PATH.r) {
    rbindlist(lapply(.listFiles(file.path(PATH.r, "response"), full.names = TRUE), fread),
              use.names = TRUE, fill = TRUE)
  }

  .gauge <- function(PATH.r) {
    R <- fread(file.path(PATH.r, "summary", "residuals_oof.csv"))
    R <- R[get(key) %in% KEYS.L]
    mean(R[, .(r2 = cor(y_obs, y_hat_oof)^2), by = response]$r2, na.rm = TRUE)
  }

  .disagreement <- function(PRED) {
    BASE <- PRED[method %in% methods & get(key) %in% KEYS.U]
    ENS  <- PRED[method == "ensemble.RMSE" & get(key) %in% KEYS.U,
                 c(key, "response", "value"), with = FALSE]
    AUX <- BASE[, .(spread = max(value) - min(value), nm = uniqueN(method)),
                by = c(key, "response")][nm >= 2L]
    AUX[ENS, on = c(key, "response"), D := spread / pmax(1, abs(i.value))]
    AUX[, c(key, "response", "D"), with = FALSE]
  }

  PATH.r <- .fitRound(NULL)
  PRED <- .predictions(PATH.r)
  bestR2 <- .gauge(PATH.r)
  HIST <- data.table(round = 0L, promoted = 0L, cumulative = 0L,
                     oofR2 = bestR2, action = "baseline")
  PSEUDO <- NULL

  for (s in seq_len(maxRounds)) {
    CAND <- .disagreement(PRED)
    if (!is.null(PSEUDO)) CAND <- CAND[!PSEUDO, on = c(key, "response")]
    NEW <- CAND[D < tau][order(D)]
    if (!nrow(NEW)) {
      HIST <- rbind(HIST, data.table(round = s, promoted = 0L,
                                     cumulative = if (is.null(PSEUDO)) 0L else nrow(PSEUDO),
                                     oofR2 = bestR2, action = "empty-stop"))
      break
    }

    DONE.keys <- if (is.null(PSEUDO)) character(0) else unique(PSEUDO[[key]])
    BUDGET <- floor(promotionCap * length(KEYS.U)) - length(DONE.keys)
    NEW.keys <- setdiff(unique(NEW[[key]]), DONE.keys)
    if (BUDGET <= 0L || !length(NEW.keys)) {
      HIST <- rbind(HIST, data.table(round = s, promoted = 0L,
                                     cumulative = if (is.null(PSEUDO)) 0L else nrow(PSEUDO),
                                     oofR2 = bestR2, action = "saturation-stop"))
      break
    }
    if (length(NEW.keys) > BUDGET) {
      KEEP <- NEW[!get(key) %in% DONE.keys][!duplicated(get(key))][seq_len(BUDGET), get(key)]
      NEW <- NEW[get(key) %in% c(KEEP, DONE.keys)]
    }
    NEW <- NEW[, c(key, "response"), with = FALSE]

    LABELS <- PRED[method == "ensemble.RMSE"][NEW, on = c(key, "response"),
                                              c(key, "response", "value"), with = FALSE]
    PSEUDO.try <- rbindlist(list(PSEUDO, LABELS), use.names = TRUE)

    PATH.r <- .fitRound(PSEUDO.try)
    PRED.try <- .predictions(PATH.r)
    g <- .gauge(PATH.r)

    if (g < bestR2 - epsRevert) {
      HIST <- rbind(HIST, data.table(round = s, promoted = nrow(NEW),
                                     cumulative = if (is.null(PSEUDO)) 0L else nrow(PSEUDO),
                                     oofR2 = g, action = "REVERT-stop"))
      break
    }
    PSEUDO <- PSEUDO.try; PRED <- PRED.try; bestR2 <- max(bestR2, g)
    HIST <- rbind(HIST, data.table(round = s, promoted = nrow(NEW), cumulative = nrow(PSEUDO),
                                   oofR2 = g, action = "accept"))
  }

  if (HIST[.N, action] == "REVERT-stop") .fitRound(PSEUDO)

  # full-pool re-emission: promoted rows left the predict pool, so the final
  # response file only covers the remnant; re-emit the accepted models over
  # every ORIGINAL predict row and assert completeness.
  for (dn in datasets) {
    for (rp in responses) {
      COLS <- names(fread(file.path(.path.work, "round", "train",
                                   sprintf("%s_%s_TRAIN.csv", rp, dn)), nrows = 0L))
      COLS <- c(key, setdiff(COLS, "y"))
      TEST <- fread(FILES[match(dn, gsub("[.]csv$", "", basename(FILES)))])[
        get(key) %in% KEYS.U, COLS, with = FALSE]
      fwrite(TEST, file.path(.path.work, "round", "train", sprintf("%s_%s_TEST.csv", rp, dn)))
    }
  }
  trainRegressionModel(TRAIN = FALSE, PARALLEL = FALSE,
    .path.data     = file.path(.path.work, "round", "train"),
    .path.model    = file.path(.path.work, "round", "model"),
    .path.metrics  = file.path(.path.work, "round", "metrics"),
    .path.response = file.path(.path.work, "round", "response"),
    .responses = responses, .datasets = datasets,
    .methods = methods, .min = boundsMin, .max = boundsMax)
  for (dn in datasets) for (rp in responses) {
    OUT <- fread(file.path(.path.work, "round", "response", sprintf("%s_%s.csv", rp, dn)))
    stopifnot(uniqueN(OUT[method == "ensemble.RMSE", get(key)]) == length(KEYS.U))
    stopifnot(setequal(OUT[method == "ensemble.RMSE", get(key)], KEYS.U))
  }

  .summarizePredictions(
    pathMetrics = file.path(.path.work, "round", "metrics"),
    pathResponse = file.path(.path.work, "round", "response"),
    pathSummary = file.path(.path.work, "round", "summary"),
    pathExport = file.path(.path.work, "round", "export"),
    datasets = datasets, responses = responses, probs = c(0.50, 0.90, 0.95),
    ensemble = "ensemble.RMSE", boundsMin = boundsMin, boundsMax = boundsMax,
    pathIndex = NULL, deliverableDatasets = NULL
  )
  aggregateResponses(
    .path.summary = file.path(.path.work, "round", "summary"),
    .path.export = file.path(.path.work, "round", "export"),
    .path.datasets = .path.datasets, key = key, responses = responses,
    datasets = datasets, boundsMin = boundsMin, boundsMax = boundsMax
  )

  list(history = HIST, promoted = PSEUDO, baselineR2 = HIST$oofR2[1L],
       finalR2 = bestR2, iterStar = iterStar)
}
# nolint end
