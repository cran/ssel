# chainImportance.R - feature-importance gate for chain regression.
#
# extractChainImportance(pathModel, suffix) -> long data.table of
#   normalised importance for chain features across all trained
#   models.
# activeByImportance(importanceDT, suffix, thresh) -> character
#   vector of chain responses whose aggregate importance >= thresh.
#
# Both functions are pure (no side effects).



# -- Stage 1 ------------------------------------------------------------------

# Returns data.table(feature, response, dataset, method, normImportance):
#   feature       - e.g. "ppvL_prev"
#   response      - the response this model predicts (e.g. "ppa")
#   dataset       - dataset key (e.g. "QDS")
#   method        - learner (ranger / gbm / earth)
#   normImportance - importance of `feature` as % of total importance in this model
#
# Returns empty data.table if no chain features found in any model.

#' @export
extractChainImportance <- function(pathModel, suffix = ".o", useModel = TRUE) {
  # useModel=TRUE  -> caret picks the model-specific importance (e.g. ranger
  #                  impurity if it was trained with importance="...", earth
  #                  basis-function counts, gbm relative.influence). This is
  #                  the legacy path; behaviour is preserved verbatim so
  #                  chainGate="fixed" with chainThresh > 0 keeps its old
  #                  active-set decision.
  # useModel=FALSE -> model-agnostic, correlation-based for regression. Every
  #                  column of the training data (including a Boruta-style
  #                  "shadow" noise column when injected) gets a score on
  #                  the same scale. Used by activeByShadow() / chainGate
  #                  ="shadow"; useModel=TRUE is unusable there because
  #                  earth omits unused features and ranger/gbm here are
  #                  trained without importance args.
  Files <- list.files(pathModel, pattern = "\\.Rds$", full.names = TRUE)
  Files <- Files[!endsWith(Files, ".hash")]
  if (length(Files) == 0L) return(data.table())

  Result <- rbindlist(lapply(Files, function(f) {
    Parts   <- strsplit(tools::file_path_sans_ext(basename(f)), "_")[[1]]
    Method  <- Parts[1]
    Dataset <- Parts[length(Parts)]
    Resp    <- paste(Parts[2:(length(Parts) - 1L)], collapse = "_")

    Mod <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(Mod)) return(NULL)

    Imp <- tryCatch(
      as.data.table(caret::varImp(Mod, useModel = useModel)$importance, keep.rownames = TRUE),
      error = function(e) NULL
    )
    if (is.null(Imp) || nrow(Imp) == 0L) return(NULL)
    setnames(Imp, c("rn", "Overall"), c("feature", "importance"))

    Total <- sum(Imp$importance, na.rm = TRUE)
    Imp[, normImportance := if (Total > 0) round(importance / Total * 100, 4) else 0]

    # R3: shadow's importance, if injected by buildDataset(.shadow=TRUE).
    ShadowImp <- Imp[feature == "shadow", normImportance]
    ShadowImp <- if (length(ShadowImp) == 1L) ShadowImp else NA_real_

    Chain <- Imp[endsWith(feature, suffix)]
    if (nrow(Chain) == 0L) return(NULL)

    Chain[, `:=`(method = Method, response = Resp, dataset = Dataset,
                 shadowImportance = ShadowImp)]
    Chain[, .(feature, response, dataset, method, normImportance, shadowImportance)]
  }), fill = TRUE)

  if (is.null(Result)) return(data.table())
  Result
}

# -- Stage 2 ------------------------------------------------------------------

# Aggregates normImportance across all models per chain feature, then maps
# feature -> response name (strips suffix) and returns active responses.
#
# thresh: minimum mean normalized importance (%) to be considered active.
#         0 -> any presence qualifies (SoT baseline).
#         0.5 -> must average at least 0.5% of model importance.

#' @export
activeByImportance <- function(importanceDT, suffix = ".o", thresh = 0) {
  if (is.null(importanceDT) || nrow(importanceDT) == 0L) return(character(0))

  DT <- copy(importanceDT)
  DT[, chainResp := substr(feature, 1L, nchar(feature) - nchar(suffix))]

  Agg <- DT[, .(meanImport = mean(normImportance, na.rm = TRUE)), by = chainResp]
  Agg[meanImport >= thresh, chainResp]
}

# R3 - gate via shadow null floor.
# A chain feature Y_k.o is "active" iff its importance in a given model
# exceeds the shadow's importance in that same model (the per-model null
# floor produced by buildDataset(.shadow=TRUE) injecting Gaussian noise).
# Aggregate across the (response, dataset, method) cells where the
# feature appears: keep features that beat shadow in the majority of cells.
#' @export
activeByShadow <- function(IMP, suffix = ".o") {
  if (is.null(IMP) || !nrow(IMP) || !"shadowImportance" %in% names(IMP))
    return(character(0))
  IMP[, .(b = sum(!is.na(shadowImportance) & normImportance > shadowImportance), n = .N),
      by = .(r = substr(feature, 1L, nchar(feature) - nchar(suffix)))
     ][b > n / 2L, r]
}

# -- Convenience wrapper (used in chain.R Stage 3) ----------------------------

# Combines extract + filter + stability into a single call.
# History: list of previous raw Active vectors (character()).
# Returns stable active character vector.

#' @export
computeActiveByImportance <- function(pathModel, suffix = ".o", thresh, stableK, history) {
  ImpDT  <- extractChainImportance(pathModel, suffix)
  Raw    <- activeByImportance(ImpDT, suffix, thresh)
  if (stableK <= 1L || length(history) < stableK - 1L) return(Raw)
  Reduce(intersect, c(list(Raw), tail(history, stableK - 1L)))
}
