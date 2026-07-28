# nolint start
# buildDataset: prepare per-response TRAIN / TEST splits from an
# assembled dataset CSV. Reads each <DatasetID>.csv from .path.input,
# applies basic cleaning, classifies columns as features or responses
# via Y.PATTERN, and writes one (<response>_<DatasetID>_TRAIN.csv,
# <response>_<DatasetID>_TEST.csv) pair per detected numeric response.

#' @export
buildDataset <- function(
    .path.input,
    .path.train,
    .file.index     = NULL,
    Y.PATTERN       = NULL,
    KEY             = NULL,
    COLS.id         = character(0),
    features        = NULL,
    OUTLIER_REMOVAL = FALSE,
    SET_COL         = NULL,
    ITER_SUFFIX     = NULL,
    .shadow         = FALSE   # R3: inject a Gaussian noise column "shadow" for null-importance gate
) {
  stopifnot(!is.null(Y.PATTERN), is.character(Y.PATTERN), nzchar(Y.PATTERN))
  stopifnot(!is.null(KEY),       is.character(KEY),       nzchar(KEY))
  stopifnot(!is.null(SET_COL),   is.character(SET_COL),   nzchar(SET_COL))
  if (!is.null(features)) {
    stopifnot(is.character(features))
    if (anyNA(features) || any(!nzchar(features))) {
      stop("buildDataset: features must contain non-empty column names.",
           call. = FALSE)
    }
    Duplicates <- unique(features[duplicated(features)])
    if (length(Duplicates)) {
      stop(sprintf("buildDataset: duplicated features: %s",
                   paste(Duplicates, collapse = ", ")), call. = FALSE)
    }
  }

  # SET_COL is the source-of-truth column for the training-vs-prediction
  # split (set == "train" -> training, anything else -> prediction). It is
  # also excluded from features.
  COLS.id <- unique(c(COLS.id, SET_COL))

  FILES <- list.files(.path.input, pattern = "\\.csv$", full.names = TRUE)

  for (.file in FILES) {
    DatasetID <- tools::file_path_sans_ext(basename(.file))
    .DEBUG("Dataset:", DatasetID)

    # Stage 1: Read ----
    DATA <- fread(file = .file, na.strings = c("", "N/I", "N/A", "-"))
    DATA <- DATA[, !grepl("^\\*$", colnames(DATA)), with = FALSE]
    DATA <- unique(DATA)

    # Stage 2: Classify columns ----
    COLS.chr <- names(DATA)[sapply(DATA, is.character)]

    # Stage 3: Clean character columns ----
    if (length(COLS.chr) > 0) {
      DATA[, (COLS.chr) := lapply(.SD, function(x) gsub(" ", "", x)), .SDcols = COLS.chr]
    }

    # Stage 4: Convert character columns that are actually numeric ----
    for (col in COLS.chr) {
      .v <- toNumeric(DATA[[col]])
      if (all(!is.na(.v) | is.na(DATA[[col]]))) DATA[, (col) := .v]
    }

    # Stage 5: Shadow injection (R3 chainGate=shadow) ----
    # Gaussian noise column reproducible across iters (fixed seed) so that
    # caret's variable-importance estimate of "shadow" becomes a per-model
    # null floor: a chain feature is kept iff its importance exceeds shadow's.
    if (isTRUE(.shadow)) {
      .seed.kept <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
                      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
      set.seed(12345L)
      DATA[, shadow := rnorm(.N)]
      if (!is.null(.seed.kept)) assign(".Random.seed", .seed.kept, envir = .GlobalEnv)
    }

    # Stage 6: Report ----
    .DEBUG("Numeric cols:    ", sum(sapply(DATA, is.numeric)))
    .DEBUG("Categorical cols:", sum(sapply(DATA, is.character)))
    .DEBUG("Cols with NAs:   ", sum(sapply(DATA, anyNA)))

    # Stage 7: Split X / y ----
    COLS.y     <- names(DATA)[grepl(Y.PATTERN, names(DATA), ignore.case = TRUE)]
    Features   <- features
    if (!is.null(Features)) {
      Missing <- setdiff(Features, names(DATA))
      if (length(Missing)) {
        stop(sprintf("buildDataset: dataset %s is missing requested features: %s",
                     DatasetID, paste(Missing, collapse = ", ")), call. = FALSE)
      }
      Bad <- intersect(Features, COLS.y)
      if (length(Bad)) {
        stop(sprintf("buildDataset: features include response columns: %s",
                     paste(Bad, collapse = ", ")), call. = FALSE)
      }
      Bad <- intersect(Features, c(KEY, COLS.id))
      if (length(Bad)) {
        stop(sprintf("buildDataset: features include identifier columns: %s",
                     paste(Bad, collapse = ", ")), call. = FALSE)
      }
      if (!is.null(ITER_SUFFIX)) {
        Features <- unique(c(Features, intersect(paste0(COLS.y, ITER_SUFFIX),
                                                 names(DATA))))
      }
    }
    COLS.X     <- if (is.null(Features)) {
                    setdiff(names(DATA), c(COLS.y, KEY, COLS.id))
                  } else {
                    Features
                  }
    RESPONSES  <- COLS.y[sapply(DATA[, .SD, .SDcols = COLS.y], is.numeric)]
    stopifnot(SET_COL %in% names(DATA))
    .trainRows <- DATA[[SET_COL]] == "train"

    # Stage 8: Remove constant columns (training rows only) ----
    XCOL   <- COLS.X[sapply(DATA[.trainRows, .SD, .SDcols = COLS.X],
                            function(x) uniqueN(x[!is.na(x)]) <= 1)]
    COLS.X <- setdiff(COLS.X, XCOL)
    .DEBUG("COLS.X after Stage 8:", length(COLS.X))

    # Stage 9: Drop TEST rows whose features don't match TRAIN's support ----
    # TRAIN is na.omit'd in Stage 10, so models are fit on rows with no NA in
    # any predictor. TEST must mirror that invariant: rows with NA in any
    # predictor (categorical or numeric) can't be scored by the trained model
    # (earth/ranger/gbm either fail or silently drop them, breaking the
    # length contract downstream). Categorical columns also drop rows whose
    # level was never seen during training.
    COLS.cat  <- COLS.X[sapply(DATA[, .SD, .SDcols = COLS.X], is.character)]
    .dropMask <- logical(nrow(DATA))
    # (a) any NA in any predictor
    for (col in COLS.X) {
      .dropMask <- .dropMask | (!.trainRows & is.na(DATA[[col]]))
    }
    # (b) categorical level not seen in TRAIN
    for (col in COLS.cat) {
      .known    <- unique(DATA[[col]][.trainRows & !is.na(DATA[[col]])])
      .dropMask <- .dropMask | (!.trainRows & !is.na(DATA[[col]]) &
                                !(DATA[[col]] %in% .known))
    }
    if (any(.dropMask)) {
      .WARN(sprintf("Dropping %d TEST rows with unseen categorical levels (of %d TEST total)",
                    sum(.dropMask), sum(!.trainRows)))
      DATA       <- DATA[!.dropMask]
      .trainRows <- .trainRows[!.dropMask]
    }

    # Stage 10: Write TRAIN / TEST splits ----
    for (response in RESPONSES) {
      # Gauss-Seidel: exclude the previous prediction of this response from its
      # own feature set. When ITER_SUFFIX is NULL (standalone run), no-op.
      COLS.X.j <- if (!is.null(ITER_SUFFIX)) setdiff(COLS.X, paste0(response, ITER_SUFFIX)) else COLS.X
      # Track SampleIDs alongside the na.omit + outlier-removal filtering so
      # that oofEnsemble can recover SampleID from caret rowIndex directly
      # (the previous approach inferred it from the source dataset via
      # length-matching, which breaks when na.omit drops rows for feature
      # NAs -- common in chain iter >= 1 where some chain features carry NA).
      DT.train.full <- DATA[.trainRows, c(KEY, COLS.X.j, response), with = FALSE]
      keep <- complete.cases(DT.train.full[, c(COLS.X.j, response), with = FALSE])
      DT.train.full <- DT.train.full[keep]
      DT.train <- DT.train.full[, c(COLS.X.j, response), with = FALSE]
      setnames(DT.train, response, "y")
      if (OUTLIER_REMOVAL) {
        DT.train <- detectOutliers(DT.train)
        .INFO(response, ": flagged", sum(DT.train$.outlier), "outliers of", nrow(DT.train))
        outlier_mask <- DT.train$.outlier
        DT.train <- DT.train[!outlier_mask]
        DT.train.full <- DT.train.full[!outlier_mask]
        DT.train[, c(".score.mad", ".score.lof", ".outlier") := NULL]
      }
      DT.test <- DATA[!.trainRows, c(COLS.X.j, KEY), with = FALSE]
      fwrite(DT.train, file.path(.path.train, paste0(response, "_", DatasetID, "_TRAIN.csv")))
      fwrite(DT.test,  file.path(.path.train, paste0(response, "_", DatasetID, "_TEST.csv")))
      fwrite(DT.train.full[, .(SampleID = get(KEY))],
             file.path(.path.train, paste0(response, "_", DatasetID, "_TRAIN_ids.csv")))
      .DEBUG(response, " => TRAIN:", nrow(DT.train), "| TEST:", nrow(DT.test))
    }
  }

  invisible(NULL)
}
# nolint end
