# nolint start
# chainPipeline: iterative multi-response regressor.
#
# All runtime parameters are explicit. The runner script (project-side)
# is the single source of truth; ssel does not read a manifest.

# Internal helper: augment each dataset with chain-feature columns for
# iter t. Train rows (^X) get features from previous Y.csv (wide). Grid
# rows get features from previous Yo.csv (long, p=0.5). The augmented
# datasets are written to a ssel-internal scratch dir, leaving the
# project's original `data/datasets/` untouched.
.augmentDatasetsChain <- function(.path.datasets, .path.iter.ds,
                                  prevY, prevYo, suffix, activeChain, key) {
  if (!dir.exists(.path.iter.ds))
    dir.create(.path.iter.ds, recursive = TRUE)

  Y.wide  <- if (!is.null(prevY)  && file.exists(prevY))  fread(prevY)  else NULL
  Yo.long <- if (!is.null(prevYo) && file.exists(prevYo)) fread(prevYo) else NULL

  Yo.wide <- if (!is.null(Yo.long)) {
    YP <- Yo.long[p == 0.5, .(SampleID, response, value)]
    if (!is.null(activeChain)) YP <- YP[response %in% activeChain]
    if (nrow(YP) > 0L) dcast(YP, SampleID ~ response, value.var = "value") else NULL
  } else NULL

  C <- NULL
  if (!is.null(Y.wide)) {
    Cy <- copy(Y.wide)
    if (!is.null(activeChain))
      Cy <- Cy[, c(key, intersect(activeChain, names(Cy))), with = FALSE]
    C <- Cy
  }
  if (!is.null(Yo.wide)) {
    if (is.null(C)) C <- Yo.wide
    else {
      common <- setdiff(intersect(names(C), names(Yo.wide)), key)
      C <- merge(C, Yo.wide, by = key, all = TRUE, suffixes = c(".y", ".yo"))
      for (col in common) {
        C[, (col) := fcoalesce(get(paste0(col, ".y")), get(paste0(col, ".yo")))]
        C[, c(paste0(col, ".y"), paste0(col, ".yo")) := NULL]
      }
    }
  }
  if (is.null(C) || nrow(C) == 0L)
    return(invisible(NULL))

  cols.chain <- setdiff(names(C), key)
  setnames(C, cols.chain, paste0(cols.chain, suffix))

  FILES <- .listFiles(.path.datasets, pattern = "\\.csv$", full.names = TRUE)
  for (.f in FILES) {
    DS <- fread(.f)
    if (!key %in% names(DS))
      stop(sprintf(".augmentDatasetsChain: %s missing key %s", .f, key))
    out <- C[DS, on = key]
    setcolorder(out, names(DS))
    fwrite(out, file.path(.path.iter.ds, basename(.f)))
  }
  invisible(NULL)
}

.chainSummaryFiles <- c(
  "metrics.csv",
  "response_long.csv",
  "residuals_oof.csv",
  "prediction_quantiles.csv",
  "overfit.csv"
)

.chainSummaryDir <- function(.path.iter, iter) {
  file.path(.path.iter, sprintf("summary_%d", iter))
}

.snapshotChainSummary <- function(.path.summary, .path.iter, iter) {
  To <- .chainSummaryDir(.path.iter, iter)
  if (dir.exists(To)) unlink(To, recursive = TRUE)
  dir.create(To, recursive = TRUE)
  for (File in .chainSummaryFiles) {
    From <- file.path(.path.summary, File)
    if (file.exists(From)) file.copy(From, file.path(To, File), overwrite = TRUE)
  }
  invisible(To)
}

# Stitch guard: a declared response with no iteration-0 metric row has no fitted
# cells in any iteration (every cell skipped for insufficient support:
# constant response, no complete training rows). It has no delivery rows to
# preserve, so the stitch excludes it with a warning instead of aborting on
# a zero-length metric lookup.
.chainBestIters <- function(responses, M0.tab, Conv) {
  Fit <- intersect(responses, M0.tab$response)
  Missing <- setdiff(responses, Fit)
  if (length(Missing)) {
    .WARN(sprintf("chainPipeline: no iteration-0 metrics for %s - excluded from stitch",
                  paste(Missing, collapse = ", ")))
  }
  if (length(Fit) == 0L) {
    stop("chainPipeline: no response has iteration-0 metrics - nothing to stitch.",
         call. = FALSE)
  }
  Best <- vapply(Fit, function(r) {
    r2.0   <- M0.tab[response == r, r2]
    r2.col <- paste0("r2_", r)
    iters  <- c(0L, Conv$iter)
    r2s    <- c(r2.0, if (r2.col %in% names(Conv)) Conv[[r2.col]] else rep(NA_real_, nrow(Conv)))
    iters[which.max(r2s)]
  }, integer(1))
  names(Best) <- Fit
  Best
}

.stitchChainSummary <- function(.path.summary, .path.iter, responses, bestPerResp) {
  for (File in .chainSummaryFiles) {
    Rows <- rbindlist(lapply(responses, function(Response) {
      Iter <- bestPerResp[[Response]]
      Path <- file.path(.chainSummaryDir(.path.iter, Iter), File)
      if (!file.exists(Path)) {
        stop(sprintf(
          paste0("chainPipeline: %s not found for response %s at iter %d. ",
                 "Rerun the chain with summary snapshot support before rendering stitched summaries."),
          Path, Response, Iter
        ), call. = FALSE)
      }
      DT <- fread(Path)
      if (!"response" %in% names(DT)) {
        stop(sprintf("chainPipeline: %s has no response column.", Path), call. = FALSE)
      }
      DT[response == Response]
    }), use.names = TRUE, fill = TRUE)
    fwrite(Rows, file.path(.path.summary, File))
  }
  invisible(NULL)
}


#' Fit an iterative multi-response companion chain
#'
#' Refit the supervised ssel workflow while using other responses' preceding
#' delivery values as predictors. The helper supports simultaneous Jacobi
#' updates and an asymmetric Gauss--Seidel update in which training companions
#' can refresh within a sweep but prediction-row companions remain frozen. It
#' is filesystem orchestration around [modelPipeline()], not a standard
#' regressor-chain estimator or a general fixed-point solver.
#'
#' @param .path.datasets Existing directory containing the original assembled
#'   CSV files. Every non-recursive CSV is copied into augmented iteration
#'   directories; [modelPipeline()] applies its all-CSV split contract.
#' @param .path.train,.path.model,.path.metrics,.path.response,.path.summary,.path.export
#'   Mutable supervised-workflow directories. They are repeatedly overwritten
#'   or removed under the rules in **Files and final stitch**.
#' @param .path.iter Mutable chain-state directory. A fresh run recursively
#'   removes it. It then contains augmented datasets, iteration response and
#'   metric copies, five-file summary snapshots, and
#'   \code{convergence.csv}.
#' @param key Non-empty character key passed to joins and [modelPipeline()].
#'   Final response products still inherit [aggregateResponses()]'s independent
#'   \code{SampleID} requirement.
#' @param responses Non-empty character vector of response column names. The
#'   names also determine sorted Gauss--Seidel order, metric columns, and
#'   suffixed companion names. Supervised split-file discovery interprets the
#'   first underscore field as the response, so compatible response identifiers
#'   cannot contain underscores.
#' @param datasets Non-empty character vector passed to [modelPipeline()] for
#'   final feature-file precedence. It does not restrict all-CSV split/model
#'   discovery. Upstream split parsers likewise require dataset identifiers
#'   without underscores.
#' @param colsId Character vector of identifier columns excluded from inferred
#'   predictors.
#' @param features \code{NULL}, or exact base predictor names. With exact
#'   names, available response companions are appended automatically by
#'   [buildDataset()]. The shadow predictor is not appended automatically and
#'   must be named explicitly when \code{chainGate = "shadow"};
#'   \code{NULL} inference includes it.
#' @param setCol Non-empty character name of the assembled row-role column.
#' @param methods Character vector of caret method identifiers passed to every
#'   supervised fit. The default is
#'   \code{c("ranger", "earth", "gbm")}.
#' @param seed \code{NULL}, or a value passed to iteration-zero
#'   [modelPipeline()]. Iteration \eqn{t} receives \code{seed + t}. The
#'   function does not validate arithmetic compatibility.
#' @param ensemble Forwarded to [modelPipeline()] for its first delivery
#'   selector. Chain metric comparison and final iteration selection instead
#'   use \code{ensemble.RMSE} rows under their separate contracts. Active-set
#'   extraction does not use ensemble rows: it reads the individual fitted
#'   model files in \code{.path.model}. Final aggregation consumes the selected
#'   snapshots rather than reapplying this argument.
#' @param boundsMin,boundsMax Response bounds forwarded to every supervised
#'   call. They apply to response and companion deliveries as documented by
#'   the called helpers; order and finiteness are not checked here.
#' @param maxIter Numeric number of additional chain iterations to attempt.
#'   On resume it is additional to the last completed iteration, not an
#'   absolute total cap. Positivity, integrality, finiteness, and length are not
#'   proactively checked.
#' @param tol Numeric strict change-score threshold. The chain stops when the
#'   current \eqn{\delta_t} is strictly less than this value. See **Change and
#'   stopping policies**.
#' @param patience Numeric strike threshold used independently for consecutive
#'   non-improvement and increasing change. It is not validated as a positive
#'   integer.
#' @param sweep One of \code{"jacobi"} or \code{"gauss-seidel"}.
#' @param suffix Character suffix appended to companion columns and passed to
#'   importance extraction. It is not validated as non-empty or collision-free.
#' @param chainGate One of \code{"fixed"} or \code{"shadow"}. The fixed policy
#'   uses model-specific importance and an inclusive mean threshold. The shadow
#'   policy uses ssel's strict-majority single-shadow rule; it is not Boruta.
#' @param chainThresh Numeric normalized-importance percentage threshold used
#'   only by the fixed gate. It is forwarded without validation.
#' @param chainStable Value coerced with [as.integer()] to the raw active-set
#'   intersection window. Missing, nonpositive, or otherwise unsuitable values
#'   are not proactively rejected.
#' @param resume Logical flag. Resume occurs only when both
#'   \code{convergence.csv} and \code{Y.csv} exist; otherwise a fresh run
#'   starts. The value is tested by ordinary logical conditions rather than
#'   validated as a scalar.
#'
#' @section Companion values and row classes:
#' Let \eqn{Y_k^{(t)}(i)} denote response \eqn{k}'s \code{p = 0.5} value in
#' iteration \eqn{t}'s wide \code{Y.csv} for training key \eqn{i}. Let
#' \eqn{Z_k^{(t)}(u)} denote its \code{p = 0.5} value in \code{Yo<t>.csv} for
#' prediction key \eqn{u}. These are median signed-residual-offset delivery
#' values after interval projection, in response \eqn{k}'s units; they are not
#' unadjusted point predictions.
#'
#' Before a fit, the helper casts the selected \code{Yo} responses wide and
#' joins them to \code{Y} by \code{key}. For a common key and response,
#' \code{Y} takes precedence and \code{Yo} fills only a missing \code{Y}
#' value. Companion columns are renamed with \code{suffix} and joined to every
#' original assembled CSV. [buildDataset()] removes target \eqn{j}'s own
#' suffixed column from that target's predictors. Its complete-case rules can
#' remove rows with missing companions.
#'
#' @section Jacobi and Gauss--Seidel recurrences:
#' Let \eqn{A^{(t-1)}} be the active companion-response set entering sweep
#' \eqn{t}. In a Jacobi sweep, one augmented dataset snapshot is built before
#' any target is refitted. Every target \eqn{j} therefore uses
#' \deqn{\{Y_k^{(t-1)}(i):k\in A^{(t-1)},k\ne j\}}
#' for a training row and
#' \deqn{\{Z_k^{(t-1)}(u):k\in A^{(t-1)},k\ne j\}}
#' for a prediction row.
#'
#' Gauss--Seidel targets are processed in sorted response-name order. That
#' order does not depend on the session locale: ASCII case is ignored, and
#' \code{_} and \code{.} sort before any other character. The
#' augmented CSVs are rebuilt before every target. Earlier targets' new
#' training-row values can therefore enter later targets through the evolving
#' \code{Y.csv}; later targets retain their preceding-sweep values. In contrast,
#' the \code{PrevYo} path is fixed for the entire sweep, so every target uses
#' only \eqn{Z_k^{(t-1)}(u)} on prediction rows. Thus the immediate-update
#' analogy applies only to training companions. No within-sweep prediction-row
#' companion is consumed.
#'
#' These recurrences are ssel package definitions related to response-as-input
#' multi-target methods. They do not inherit published regressor-chain
#' ordering, out-of-fold, convergence, or optimality claims.
#'
#' @section Active companion policy:
#' This policy is independent of the \code{ensemble.RMSE} metric rows used for
#' change history and final stitching. After each completed fit, \code{"fixed"} calls
#' [extractChainImportance()] with \code{useModel = TRUE}, then applies
#' [activeByImportance()] at \code{chainThresh}. The \code{"shadow"} policy
#' calls the extractor with \code{useModel = FALSE} and applies
#' [activeByShadow()]. Both routes scan individual \code{.Rds} model files in
#' \code{.path.model}; neither extracts importance from an ensemble row. The
#' shadow route is a strict-majority comparison to one injected Gaussian
#' predictor and does not implement Boruta's iterative random probes or
#' statistical tests.
#'
#' An empty raw gate result is first replaced by all declared responses. If
#' enough history exists, the current raw set is then intersected with the
#' preceding \code{chainStable - 1} returned active sets. These stored sets can
#' already reflect earlier intersections. A new intersection can still be
#' empty and is recorded with active-set size zero. If another iteration is
#' attempted from that empty set, augmentation reaches data.table column
#' renaming with no companion columns and errors before the supervised fit.
#' The pipeline invokes [computeActiveByImportance()] with its stability
#' disabled and applies this common history rule itself for both gate types.
#'
#' @section Change and stopping policies:
#' For metrics, only \code{ensemble.RMSE} rows are considered and the first
#' minimum-RMSE dataset row is retained for each response. Let
#' \eqn{R_{r,t}^2} be that row's squared sample correlation. An iteration is
#' improving only when its unweighted mean across responses is strictly greater
#' than the mean at the historical accepted metric baseline. That baseline is
#' replaced only on strict improvement.
#'
#' With response columns of \code{Y.csv} compared by current row and column
#' position, ssel defines
#' \deqn{\delta_t=\max_{i,r}
#' \frac{|Y_r^{(t)}(i)-Y_r^{(t-1)}(i)|}
#' {\max\{1,|Y_r^{(t-1)}(i)|\}},}
#' removing missing cells in the final maximum. Compatible table order is
#' therefore a precondition; no key join aligns the two matrices. Although
#' numerically dimensionless, \eqn{\delta_t} is not invariant to response-unit
#' rescaling or translation because of the fixed floor 1. It is a package
#' stopping heuristic, not proof that an equation solver converged.
#'
#' Non-improvement strikes reset after an improving iteration. Divergence
#' strikes increment only when \eqn{\delta_t} strictly exceeds the preceding
#' value and reset otherwise. After all current artifacts are written, stop
#' tests run in this order: current change strictly below \code{tol}, divergence
#' strikes at \code{patience}, then non-improvement strikes at
#' \code{patience}. If every cellwise change is missing, the final
#' \code{max(..., na.rm = TRUE)} produces \code{-Inf}; that value is written
#' and satisfies \code{Delta < tol} for a finite \code{tol}, so the tolerance
#' stop can fire. If the current or accepted mean R2 is missing, the strict
#' comparison yields \code{NA} and the subsequent \code{if (!OK)} condition
#' errors before the current convergence row and snapshots are written. Other
#' invalid or non-finite inputs retain their underlying base R and data.table
#' comparison behavior. Stored change and metric values are rounded as
#' described in **Files and final stitch**.
#'
#' @section Resume behavior:
#' A fresh run recursively removes \code{.path.iter}, runs iteration zero on
#' the original assembled CSVs, computes the first active set, and writes its
#' snapshots. A recognized resume additionally requires current summary
#' \code{metrics.csv}; absence stops with a diagnostic. Older convergence files
#' lacking \code{nActive} are rewritten with that missing column.
#'
#' Resume takes the largest recorded iteration, last recorded change, current
#' summary metrics, and the matching \code{Yo} snapshot when present. It resets
#' both strike counters, does not reconstruct active-set history, and sets the
#' first resumed active set to \code{NULL}; that first augmentation therefore
#' retains every available companion. It attempts \code{maxIter} further
#' iterations. Resume state and subsequent writes are not transactional.
#'
#' @section Files and final stitch:
#' Iteration zero copies \code{Y0.csv}, \code{Yo0.csv},
#' \code{metrics_0.csv}, and a \code{summary_0} directory. Every later
#' iteration writes \code{datasets_t}, \code{Yt.csv}, \code{Yot.csv},
#' \code{metrics_t.csv}, \code{summary_t}, and a convergence row. Summary
#' snapshots cover \code{metrics.csv}, \code{response_long.csv},
#' \code{residuals_oof.csv}, \code{prediction_quantiles.csv}, and
#' \code{overfit.csv}. Missing source summary files are simply absent from a
#' snapshot. Convergence rows contain iteration, change rounded to eight
#' decimals, mean RMSE to six, mean R2 and per-response R2 to four, improvement,
#' strike count, and active-set size.
#'
#' At completion, response \eqn{r} selects the first maximum R2 among baseline
#' iteration zero and recorded iterations. Baseline R2 is read without
#' rounding; later candidates use the four-decimal convergence values, so
#' rounding can affect selection. \code{Yo.csv} and all five current summary
#' files are then overwritten response by response from their selected
#' snapshots. Missing snapshots or a missing \code{response} column stop the
#' stitch.
#'
#' Models, TRAIN/TEST splits, cell metric/response files, \code{Y.csv},
#' \code{X.csv}, and \code{Xo.csv} remain from the last fitted iteration; they
#' are not stitched. Filesystem deletion, fitting, and writes have no rollback.
#' Progress uses ssel's condition channels. RNG and bounded PSOCK effects are
#' those of the composed supervised helpers; each fitted iteration receives the
#' seed described above.
#'
#' @return Invisibly, \code{NULL}. Results are the mutable supervised products,
#'   iteration state, convergence history, and stitched response/summary files
#'   described above.
#' @seealso [modelPipeline()] for each supervised refit,
#'   [aggregateResponses()] for \code{Y}/\code{Yo} construction,
#'   [extractChainImportance()] for model-file importance, and
#'   [activeByImportance()] and [activeByShadow()] for the two table gates.
#' @references
#' Spyromitros-Xioufis, E., Tsoumakas, G., Groves, W., and Vlahavas, I. (2016).
#' Multi-target regression via input space expansion: treating targets as
#' inputs. \emph{Machine Learning}, 104, 55--98.
#' \doi{10.1007/s10994-016-5546-z}.
#' @examples
#' \donttest{
#' root <- tempfile("ssel-chain-")
#' datasets_dir <- file.path(root, "datasets")
#' dir.create(datasets_dir, recursive = TRUE)
#'
#' assembled <- data.frame(
#'   SampleID = c(paste0("S", 1:18), "P1", "P2"),
#'   set = c(rep("train", 18), "predict", "predict"),
#'   x = 1:20,
#'   z = rep(c(0, 1), 10),
#'   targetA = c(1.0, 2.4, 1.8, 3.7, 2.9, 5.1, 4.0, 6.2, 5.4,
#'               7.0, 6.1, 8.3, 7.2, 9.1, 8.0, 10.5, 9.4, 11.2,
#'               NA, NA),
#'   targetB = c(2.2, 1.3, 3.6, 2.7, 4.1, 3.8, 5.7, 4.6, 6.5,
#'               5.9, 7.8, 6.4, 8.6, 7.5, 9.9, 8.7, 10.4, 9.6,
#'               NA, NA)
#' )
#' utils::write.csv(assembled, file.path(datasets_dir, "demo.csv"),
#'                  row.names = FALSE, na = "")
#'
#' old_quiet <- getOption("aR.quiet")
#' options(aR.quiet = TRUE)
#' chainPipeline(
#'   .path.datasets = datasets_dir,
#'   .path.train = file.path(root, "train"),
#'   .path.model = file.path(root, "model"),
#'   .path.metrics = file.path(root, "metrics"),
#'   .path.response = file.path(root, "response"),
#'   .path.summary = file.path(root, "summary"),
#'   .path.export = file.path(root, "export"),
#'   .path.iter = file.path(root, "iter"),
#'   key = "SampleID",
#'   responses = c("targetA", "targetB"),
#'   datasets = "demo",
#'   methods = "lm",
#'   seed = 2026,
#'   maxIter = 1,
#'   tol = 0,
#'   patience = 2,
#'   sweep = "jacobi"
#' )
#' options(aR.quiet = old_quiet)
#'
#' data.table::fread(file.path(root, "iter", "convergence.csv"))
#' data.table::fread(file.path(root, "export", "Yo.csv"))
#' # Final Yo rows can come from different best iterations by response.
#'
#' unlink(root, recursive = TRUE)
#' }
#' @import data.table
#' @importFrom caret train trainControl varImp
#' @importFrom crayon blue yellow red green silver bold
#' @importFrom digest digest
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makePSOCKcluster stopCluster
#' @export
chainPipeline <- function(
    .path.datasets,
    .path.train,
    .path.model,
    .path.metrics,
    .path.response,
    .path.summary,
    .path.export,
    .path.iter,
    key,
    responses,
    datasets,
    colsId       = character(0),
    features     = NULL,
    setCol       = "set",
    methods      = c("ranger", "earth", "gbm"),
    seed         = NULL,
    ensemble     = "ensemble.RMSE",
    boundsMin    = -Inf,
    boundsMax    = Inf,
    maxIter,
    tol,
    patience,
    sweep        = "jacobi",
    suffix       = ".o",
    chainGate    = "fixed",
    chainThresh  = 0,
    chainStable  = 1L,
    resume       = FALSE
) {
  .keysOld <- options(datatable.keepLeadingZeros = TRUE)
  on.exit(options(.keysOld), add = TRUE)
  stopifnot(is.character(key),       nzchar(key))
  stopifnot(is.character(responses), length(responses) > 0L)
  stopifnot(is.character(datasets),  length(datasets) > 0L)
  stopifnot(is.character(setCol),    nzchar(setCol))
  stopifnot(sweep %in% c("jacobi", "gauss-seidel"))
  stopifnot(chainGate %in% c("fixed", "shadow"))
  stopifnot(is.numeric(maxIter), is.numeric(tol), is.numeric(patience))
  if (!is.null(features)) stopifnot(is.character(features))
  .checkBound(boundsMin, "boundsMin")
  .checkBound(boundsMax, "boundsMax")
  SHADOW <- (chainGate == "shadow")
  STABLE_K <- as.integer(chainStable)

  .guardWipe(c(.path.train, .path.model, .path.metrics, .path.response,
               .path.summary, .path.export, .path.iter),
             .path.datasets)

  if (!dir.exists(.path.iter)) dir.create(.path.iter, recursive = TRUE)
  ITER_Y    <- file.path(.path.iter, "Y.csv")
  ITER_CONV <- file.path(.path.iter, "convergence.csv")
  FILE.met  <- file.path(.path.summary, "metrics.csv")

  ORIGINAL_DATASETS <- .path.datasets

  # Helper closures reuse the runtime params from the enclosing call.
  RunModel <- function(seedT, dsPath, responsesActive = NULL, wipe = NULL) {
    modelPipeline(.path.datasets = dsPath,
                  .path.train    = .path.train,
                  .path.model    = .path.model,
                  .path.metrics  = .path.metrics,
                  .path.response = .path.response,
                  .path.summary  = .path.summary,
                  .path.export   = .path.export,
                  .path.iter     = .path.iter,
                  key            = key,
                  responses      = responses,
                  datasets       = datasets,
                  colsId         = colsId,
                  features       = features,
                  setCol         = setCol,
                  methods        = methods,
                  seed           = seedT,
                  ensemble       = ensemble,
                  boundsMin      = boundsMin,
                  boundsMax      = boundsMax,
                  iterSuffix     = suffix,
                  responsesActive = responsesActive,
                  wipe           = wipe,
                  shadow         = SHADOW)
  }

  BestMetrics <- function() {
    m <- fread(FILE.met)[method == "ensemble.RMSE",
           .SD[which.min(rmse)], by = response][order(response)]
    list(rmse = m$rmse, r2 = m$r2, response = m$response)
  }

  ComputeActive <- function(History) {
    Raw <- if (chainGate == "shadow") {
      ImpDT <- extractChainImportance(.path.model, suffix, useModel = FALSE)
      activeByShadow(ImpDT, suffix)
    } else {
      computeActiveByImportance(.path.model, suffix, chainThresh, 1L, list())
    }
    if (length(Raw) == 0L) Raw <- responses
    if (STABLE_K <= 1L || length(History) < STABLE_K - 1L) return(Raw)
    Reduce(intersect, c(list(Raw), tail(History, STABLE_K - 1L)))
  }

  RunGaussSeidel <- function(t, SeedT, ActiveChain, PrevYo) {
    for (Dir in c(.path.model, .path.train, .path.metrics, .path.response,
                  .path.summary, .path.export))
      { if (dir.exists(Dir)) unlink(Dir, recursive = TRUE); dir.create(Dir, recursive = TRUE) }

    Order  <- .portableSort(responses)
    iterDS <- file.path(.path.iter, sprintf("datasets_%d", t))
    .INFO("[GS] sweep order:", paste(Order, collapse = ", "))
    for (i in seq_along(Order)) {
      j <- Order[i]
      .SUB(sprintf("sub-iter %d/%d: %s", i, length(Order), j))
      .augmentDatasetsChain(ORIGINAL_DATASETS, iterDS,
                            prevY = ITER_Y, prevYo = PrevYo,
                            suffix = suffix, activeChain = ActiveChain,
                            key = key)
      RunModel(SeedT, iterDS, responsesActive = c(j), wipe = FALSE)
    }
  }

  # Iter 0 (no chain features) ----
  if (resume && file.exists(ITER_CONV) && file.exists(ITER_Y)) {
    if (!file.exists(FILE.met)) {
      stop(sprintf(
        paste0("chainPipeline: resume=TRUE but resume state is incomplete.\n",
               "Found: %s\nFound: %s\nMissing: %s\n",
               "Start a fresh chain run with resume=FALSE or restore the ",
               "missing summary metrics artifact."),
        ITER_CONV, ITER_Y, FILE.met
      ), call. = FALSE)
    }
    Conv.prev  <- fread(ITER_CONV)
    if (!"nActive" %in% names(Conv.prev)) {
      Conv.prev[, nActive := NA_integer_]
      fwrite(Conv.prev, ITER_CONV)
    }
    T_DONE     <- max(Conv.prev$iter)
    DeltaPrev  <- tail(Conv.prev$delta, 1)
    M0         <- BestMetrics()
    Strikes    <- 0L
    DivStrikes <- 0L
    PrevYo     <- file.path(.path.iter, sprintf("Yo%d.csv", T_DONE))
    if (!file.exists(PrevYo)) PrevYo <- NULL
    ActiveHistory <- list()
    ActiveChain   <- NULL
    .HEAD(sprintf("Chain resume from iteration %d", T_DONE))
  } else {
    if (dir.exists(.path.iter)) unlink(.path.iter, recursive = TRUE)
    dir.create(.path.iter, recursive = TRUE)

    .HEAD("Iteration 0 (X only)")
    RunModel(seed, ORIGINAL_DATASETS)

    if (!file.exists(ITER_Y))
      stop("chainPipeline: Y.csv not written - check aggregateResponses.R")

    M0 <- BestMetrics()
    .snapshotChainSummary(.path.summary, .path.iter, 0L)
    file.copy(ITER_Y,                                       file.path(.path.iter, "Y0.csv"))
    file.copy(file.path(.path.export,  "Yo.csv"),           file.path(.path.iter, "Yo0.csv"),     overwrite = TRUE)
    file.copy(file.path(.path.summary, "metrics.csv"),      file.path(.path.iter, "metrics_0.csv"))
    .KV("meanRMSE", sprintf("%.4f", mean(M0$rmse)))
    .KV("meanR2",   sprintf("%.4f", mean(M0$r2)))
    T_DONE     <- 0L
    Strikes    <- 0L
    DivStrikes <- 0L
    DeltaPrev  <- Inf
    PrevYo     <- file.path(.path.iter, "Yo0.csv")

    ActiveHistory <- list()
    ActiveChain   <- ComputeActive(ActiveHistory)
    ActiveHistory <- list(ActiveChain)
    .KV("activeChain", sprintf("%d/%d [%s]", length(ActiveChain), length(responses),
                               paste(.portableSort(ActiveChain), collapse = ",")))
  }

  # Iter 1..maxIter (with chain features) ----
  for (t in seq(T_DONE + 1L, T_DONE + maxIter)) {
    .HEAD(sprintf("Iteration %d/%d [%s]", t, T_DONE + maxIter, sweep))
    Y.prev <- fread(ITER_Y)

    SeedT <- if (!is.null(seed)) seed + t else NULL
    if (sweep == "gauss-seidel") {
      RunGaussSeidel(t, SeedT, ActiveChain, PrevYo)
    } else {
      iterDS <- file.path(.path.iter, sprintf("datasets_%d", t))
      .augmentDatasetsChain(ORIGINAL_DATASETS, iterDS,
                            prevY = ITER_Y, prevYo = PrevYo,
                            suffix = suffix, activeChain = ActiveChain,
                            key = key)
      RunModel(SeedT, iterDS)
    }

    Y.curr  <- fread(ITER_Y)
    M.curr  <- BestMetrics()

    COLS  <- setdiff(names(Y.curr), "SampleID")
    Delta <- max(abs(Y.curr[,..COLS] - Y.prev[,..COLS]) /
                 pmax(1, abs(Y.prev[,..COLS])), na.rm = TRUE)

    OK <- mean(M.curr$r2) > mean(M0$r2)
    if (!OK) Strikes    <- Strikes    + 1L else Strikes    <- 0L
    if (Delta > DeltaPrev) DivStrikes <- DivStrikes + 1L else DivStrikes <- 0L

    ActiveChain   <- ComputeActive(ActiveHistory)
    ActiveHistory <- c(ActiveHistory, list(ActiveChain))
    if (length(ActiveHistory) > STABLE_K) ActiveHistory <- tail(ActiveHistory, STABLE_K)

    R2 <- setNames(as.list(round(M.curr$r2, 4)), paste0("r2_", M.curr$response))

    fwrite(c(list(iter      = t,
                  delta     = round(Delta, 8),
                  meanRMSE  = round(mean(M.curr$rmse), 6),
                  meanR2    = round(mean(M.curr$r2),  4),
                  improving = OK,
                  strikes   = Strikes,
                  nActive   = length(ActiveChain)),
             R2),
           ITER_CONV, append = TRUE)
    file.copy(ITER_Y,                                       file.path(.path.iter, sprintf("Y%d.csv",  t)))
    file.copy(file.path(.path.export,  "Yo.csv"),           file.path(.path.iter, sprintf("Yo%d.csv", t)), overwrite = TRUE)
    file.copy(file.path(.path.summary, "metrics.csv"),      file.path(.path.iter, sprintf("metrics_%d.csv", t)))
    .snapshotChainSummary(.path.summary, .path.iter, t)
    PrevYo <- file.path(.path.iter, sprintf("Yo%d.csv", t))

    .KV("delta",       sprintf("%.6f", Delta))
    .KV("meanRMSE",    sprintf("%.4f", mean(M.curr$rmse)))
    .KV("meanR2",      sprintf("%.4f", mean(M.curr$r2)))
    .KV("improving",   if (OK) crayon::green("YES") else crayon::yellow("no"))
    .KV("strikes",     sprintf("%d/%d", Strikes, patience))
    .KV("activeChain", sprintf("%d/%d [%s]", length(ActiveChain), length(responses),
                               paste(.portableSort(ActiveChain), collapse = ",")))

    if (Delta      <  tol)      { .OK ("Chain converged (Delta < tol)");                   break }
    if (DivStrikes >= patience) { .WARN("Chain: Delta increasing consistently - stopping"); break }
    if (Strikes    >= patience) { .WARN("Chain: meanR2 not improving - stopping");          break }

    DeltaPrev <- Delta
    M0        <- if (OK) M.curr else M0
  }

  Conv <- fread(ITER_CONV)

  # Per-response best-iteration stitch ----
  M0.tab <- fread(file.path(.path.iter, "metrics_0.csv"))[method == "ensemble.RMSE",
              .SD[which.min(rmse)], by = response]

  BestPerResp <- .chainBestIters(responses, M0.tab, Conv)

  .HEAD("Per-response best iteration (stitched final pass)")
  for (r in names(BestPerResp)) {
    t.j  <- BestPerResp[[r]]
    r2.j <- if (t.j == 0L) M0.tab[response == r, r2] else Conv[iter == t.j, get(paste0("r2_", r))]
    .SUB(sprintf("%-5s -> iter %2d  (R2=%.4f)", r, t.j, r2.j))
  }

  Yo.stitch <- rbindlist(lapply(names(BestPerResp), function(r) {
    t.j     <- BestPerResp[[r]]
    Yo.path <- file.path(.path.iter, sprintf("Yo%d.csv", t.j))
    if (!file.exists(Yo.path)) stop(sprintf("chainPipeline: %s not found", Yo.path))
    fread(Yo.path)[response == r]
  }), use.names = TRUE)
  fwrite(Yo.stitch, file.path(.path.export, "Yo.csv"))

  .stitchChainSummary(.path.summary, .path.iter, names(BestPerResp), BestPerResp)
  Metrics.stitch <- fread(file.path(.path.summary, "metrics.csv"))

  .OK(sprintf("data/export/Yo.csv: %d rows (stitched from %d snapshots)",
              nrow(Yo.stitch), length(unique(BestPerResp))))
  .OK(sprintf("data/summary/*.csv stitched coherently from %d snapshots",
              length(unique(BestPerResp))))
  .DEBUG(sprintf("data/summary/metrics.csv: %d rows (stitched)", nrow(Metrics.stitch)))
  .INFO("data/model/ and data/iter/Y.csv reflect last iter (T_DONE); ",
        "data/export/Yo.csv and data/summary/*.csv are stitched by response.")

  .DEBUG(paste(utils::capture.output(Conv), collapse = "\n"))
  invisible(NULL)
}
# nolint end
