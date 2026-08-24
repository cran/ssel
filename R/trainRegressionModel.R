# nolint start
# trainRegressionModel: fit caret regression models per (response,
# dataset, method) cell, write per-cell metrics, and emit per-row
# predictions. Reproducibility relies on the caller setting the PRNG
# seed before invocation (see modelPipeline()'s `seed` argument).

.checkResponseSupport <- function(DT, set) {
  if (!"y" %in% names(DT)) {
    return(sprintf("Skipping %s: training data is missing response column 'y' (n=%d).",
                   set, nrow(DT)))
  }
  Y <- DT$y[!is.na(DT$y)]
  if (!length(Y)) {
    return(sprintf("Skipping %s: response has no non-missing training values (n=%d).",
                   set, nrow(DT)))
  }
  NUnique <- uniqueN(Y)
  if (NUnique < 2L) {
    return(sprintf(
      paste0("Skipping %s: response is constant in training data ",
             "(n=%d, unique_y=%d, min=%s, max=%s)."),
      set, nrow(DT), NUnique,
      format(min(Y), digits = 6, trim = TRUE, scientific = FALSE),
      format(max(Y), digits = 6, trim = TRUE, scientific = FALSE)
    ))
  }
  NULL
}

.registerBackend <- function(parallel) {
  if (!parallel) {
    registerDoSEQ()
    .INFO("Serial mode.")
    return(NULL)
  }
  Cores <- detectCores(logical = TRUE)
  if (!length(Cores) || is.na(Cores)) Cores <- 1L
  Cores <- min(2L, max(1L, as.integer(Cores)))
  Cluster <- makePSOCKcluster(Cores)
  registerDoParallel(Cluster)
  .INFO("Parallel mode: using", Cores, "cores.")
  Cluster
}

.stopBackend <- function(cluster) {
  stopCluster(cluster)
  registerDoSEQ()
  invisible(NULL)
}

#' @export
trainRegressionModel <- function(
    .path.data,     
    .path.model,    
    .path.metrics,  
    .path.response,
    OVERRIDE       = FALSE,
    TRAIN          = TRUE,
    PARALLEL       = TRUE,
    .min = -Inf,
    .max = Inf,
    .numberCV      = 10,
    .responses     = NULL,
    .methods       = NULL,
    .datasets      = NULL,
    .tuneLength    = 10,
    .preProcess    = NULL,
    .metric        = "RMSE"
) {
  # ---------------------------------------------------------
  # Stage 1: TRAIN MODELS ONLY if (TRAIN == TRUE) ----
  # ---------------------------------------------------------
  if (TRAIN == TRUE) {
    .INFO("Training stage only. No metrics or ensemble.")
    
    Cluster <- .registerBackend(PARALLEL)
    if (!is.null(Cluster)) on.exit(.stopBackend(Cluster), add = TRUE)
    
    .trControl <- trainControl(
      method        = "cv",
      number        = .numberCV,
      verboseIter   = FALSE,
      allowParallel = PARALLEL,
      savePredictions = "final"# In order to compute ensembling weights
    )
    
    for (.response in .responses) {
      for (.dataset in .datasets) {
        
        .set <- paste0(.response, "_", .dataset)
        .file.data <- file.path(.path.data, paste0(.set, "_TRAIN.csv"))
        
        if (!file.exists(.file.data)) {
          .WARN("Skipping dataset:", .set, "- data file not found")
          next
        }
        DT <- fread(.file.data)
        for (.c in names(DT))
          if (is.integer(DT[[.c]])) DT[, (.c) := as.numeric(get(.c))]
        .INFO("Found training data for:", .set, "")
        Check <- .checkResponseSupport(DT, .set)
        if (!is.null(Check)) {
          .WARN(Check)
          next
        }
        
        # Feature hash: sorted column names of training data (excluding response).
        # Written alongside each .Rds as a .hash file.
        # OVERRIDE=FALSE skips retraining ONLY if both .Rds and .hash exist
        # AND the hash matches - ensuring chain iterations always retrain
        # when training features change (new chain columns).
        .feat.hash <- substr(
          digest::digest(sort(setdiff(names(DT), "y")), algo = "md5", serialize = TRUE),
          1L, 8L)

        # Train each method
        for (.method in .methods) {
          .file.model <- file.path(.path.model, paste0(.method, "_", .set, ".Rds"))
          .file.hash  <- paste0(.file.model, ".hash")

          if (!OVERRIDE && file.exists(.file.model) && file.exists(.file.hash)) {
            .stored.hash <- readLines(.file.hash, n = 1L, warn = FALSE)
            if (identical(.stored.hash, .feat.hash)) {
              .INFO("Model already exists (same features):", .file.model, "")
              next
            }
            .INFO("Features changed - retraining:", .file.model, "")
          }
          
          .INFO("Training .set:", .set, " using method:", .method, "")
          # Safe training with error handling
          model.train <- NULL
          model.train <- tryCatch(
            { result <- NULL
            capture.output(
              {
                result <- train(
                  y ~ .,
                  data       = DT,
                  method     = .method,
                  trControl  = .trControl,
                  tuneLength = .tuneLength,
                  preProcess = .preProcess,
                  metric     = .metric
                )
              },
              file = if (toupper(Sys.info()[["sysname"]]) == "WINDOWS") "NUL" else "/dev/null"
            )
            result  
            },
            error = function(e) {
              .WARN(.method, .set, "training failed:", e$message)
              return(NULL)
            }
          )
          
          # Save the model if successfully trained
          if (!is.null(model.train)) {
            # Diagnostic: surface caret's "missing values in resampled performance"
            # warning with attribution. NA in $results means some tune-grid combos
            # had folds returning NA; NA in $resample means the best-tune's CV folds
            # themselves failed. Both point at fitter fragility for that cell.
            if (!is.null(model.train$results)) {
              n.na.tune <- sum(is.na(model.train$results$RMSE))
              if (n.na.tune > 0L)
                .WARN(sprintf("%s %s: %d/%d tune combos returned NA RMSE (folds failed)",
                              .method, .set, n.na.tune, nrow(model.train$results)))
            }
            if (!is.null(model.train$resample)) {
              n.na.fold <- sum(is.na(model.train$resample$RMSE))
              if (n.na.fold > 0L)
                .WARN(sprintf("%s %s: %d/%d best-tune CV folds returned NA RMSE",
                              .method, .set, n.na.fold, nrow(model.train$resample)))
            }
            saveRDS(model.train, file = .file.model)
            writeLines(.feat.hash, .file.hash)
            .INFO("Model saved to:", .file.model, "")
          }
        } # end .methods
      } # end .datasets
    } # end .responses
    
    return(invisible(NULL)) # End function here if TRAIN=TRUE
  } # end if TRAIN==TRUE
  
  # ---------------------------------------------------------
  # Stage 2: BUILD METRICS .  (if TRAIN == FALSE) ----
  # ---------------------------------------------------------
  .INFO("Testing stage: computing metrics, building ensembles.")
  
  Cluster <- .registerBackend(PARALLEL)
  if (!is.null(Cluster)) on.exit(.stopBackend(Cluster), add = TRUE)
  
  .metrics <- data.table()
  OOF <- list() # store out-of-fold obs if needed
  Scores <- list()
  
  
  for (.response in .responses) {
    for (.dataset in .datasets) {
      .set <- paste0(.response, "_", .dataset)
      .file.data <- file.path(.path.data, paste0(.set, "_TRAIN.csv"))
      
      if (!file.exists(.file.data)) {
        .WARN("Skipping dataset:", .set, " - training data file not found")
        next
      }
      DT <- fread(.file.data)
      for (.c in names(DT))
        if (is.integer(DT[[.c]])) DT[, (.c) := as.numeric(get(.c))]
      Check <- .checkResponseSupport(DT, .set)
      if (!is.null(Check)) {
        .WARN(Check)
        next
      }
      y  <- DT$y
      n_samples <- nrow(DT)
      n_params  <- ncol(DT) - 1
      
      R2 <- list()
      RMSE <- list()
      Yp <- list()
      
      # Load each model & compute out-of-fold metrics
      for (.method in .methods) {
        .file.model <- file.path(.path.model, paste0(.method, "_", .set, ".Rds"))
        if (!file.exists(.file.model)) {
          .WARN(.method, .set, " => skipping; no model file found.")
          next
        }
        
        model.train <- readRDS(.file.model)
        # --------------------------------------------------------------------
        # 5) Proceed to compute metrics or out-of-fold predictions
        # --------------------------------------------------------------------
        pred_final <- tryCatch({
          merge(
            model.train$pred,
            model.train$bestTune,
            by = names(model.train$bestTune),
            sort = FALSE
          )
        }, error = function(e) {
          .WARN("Merging OOF predictions =>", .method, .set, ":", e$message, "")
          return(NULL)
        })
        
        if (is.null(pred_final)) next
        
        
        # Calculate "honest" cross-validated metrics from pred_final$obs (the out-of-fold "true" y) and pred_final$pred (the out-of-fold predictions)
        setorder(pred_final, rowIndex)
        
        yp <- as.numeric(pred_final$pred)
        # Apply transformations
        yp <- pmax(yp, .min)
        yp <- pmin(yp, .max)
        
        
        obs <- as.numeric(pred_final$obs)
        # Set the OOF targets as global targets
        
        if (is.null(OOF[[.set]])) {
          OOF[[.set]] <- obs
        }
        
        
        rmse <- sqrt(mean((OOF[[.set]] - yp)^2))
        mae  <- mean(abs(OOF[[.set]] - yp))
        r2   <- cor(OOF[[.set]], yp)^2
        
        .INFO(.method, .set, " => RMSE:", rmse, " R2:", r2)
        if (!any(is.na(c(r2, rmse)))) {
          R2[[.method]]   <- r2
          RMSE[[.method]] <- rmse
          Yp[[.method]]   <- yp
          
          .metrics <- rbindlist(list(
            .metrics,
            data.table(
              method   = .method,
              set      = .set,
              dataset  = .dataset,
              response = .response,
              rmse     = rmse,
              mae      = mae,
              r2       = r2,
              n        = n_samples,
              p        = n_params
            )
          ), use.names = TRUE)
        }
      } # end .methods
      Scores[[.set]] <- list(R2 = R2, RMSE = RMSE)
      # *********************************************************************************
      # SELECT & STACK MODELS
      # build Ensembles based on metrics
      if (length(RMSE) == 0 || length(R2) == 0) {
        .WARN("No valid methods => skipping ensemble for", .set, "")
        next
      }
      
      # Use OOF[[.set]] as obs
      obs <- OOF[[.set]]
      # R2 Weighted
      .INFO("Weighted average(R2) for dataset:", .set, "")
      
      
      # ==================== R2 Ensemble Averaging ====================
      
      w <- unlist(R2)
      if (anyNA(w) || is.na(sum(w)) || sum(w) == 0) {
        .WARN("Invalid R2 weight vector => skipping R2 ensemble.")
        next
      } 
      else {
        W <- w / sum(w)
        Y <- Reduce(`+`, Map(`*`, Yp, W))
        # If no valid ensemble predictions
        if (length(Y) == 0 || all(is.na(Y))) {
          .WARN("No valid ensemble predictions => skipping ensemble for", .set, "")
        } else {
          rmse <- sqrt(mean((obs - Y)^2))
          mae  <- mean(abs(obs - Y))
          r2   <- cor(obs, Y)^2
          .metrics <- rbindlist(list(
            .metrics,
            data.table(
              method   = "ensemble.R2",
              set      = .set,
              dataset  = .dataset,
              response = .response,
              rmse     = rmse,
              mae      = mae,
              r2       = r2,
              n        = n_samples,
              p        = n_params
            )
          ), use.names=TRUE)
        } # end if
      } # end if
      
      # ==================== RMSE Ensemble Averaging ====================
      w <- 1/unlist(RMSE)
      if (anyNA(w) || is.na(sum(w)) || sum(w) == 0) {
        .WARN("Invalid RMSE weight vector => skipping RMSE ensemble.")
      } else {
        W <- w / sum(w)
        Y <- Reduce(`+`, Map(`*`, Yp, W))
        # If no valid ensemble predictions
        if (length(Y) == 0 || all(is.na(Y))) {
          .WARN("No valid ensemble predictions => skipping RMSE ensemble for", .set, "")
        } else {
          rmse <- sqrt(mean((obs - Y)^2))
          mae  <- mean(abs(obs - Y))
          r2   <- cor(obs, Y)^2
          .metrics <- rbindlist(list(
            .metrics,
            data.table(
              method   = "ensemble.RMSE",
              set      = .set,
              dataset  = .dataset,
              response = .response,
              rmse     = rmse,
              mae      = mae,
              r2       = r2,
              n        = n_samples,
              p        = n_params
            )
          ), use.names=TRUE)
        } # end if
      } # end if
      
      
      # Save metrics
      .file.metrics <- file.path(.path.metrics, paste0( .set, ".csv"))
      fwrite(.metrics, .file.metrics)
      .INFO("Model metrics saved to:", .file.metrics, "")
    } # end .datasets
  } # end .responses
  
  
  # Bind metrics from different sets
  FILES <- list.files(.path.metrics, pattern = "\\.csv$", full.names = TRUE)
  DT.metrics <- rbindlist(lapply(FILES, fread), use.names = TRUE, fill = TRUE)
  
  
  
  # ---------------------------------------------------------
  # STAGE 3: TEST MODELS WITH NEW (UNSEEN) DATA ----
  # ---------------------------------------------------------
  .INFO("Predicting stage.")
  for (.response in .responses) {
    for (.dataset in .datasets) {
      .set <- paste0(.response, "_", .dataset)
      .file.test <- file.path(.path.data, paste0(.set, "_TEST.csv"))
      
      if (!file.exists(.file.test)) {
        .WARN("Skipping dataset:", .set, " - test data file not found")
        next
      }
      DT.test <- fread(.file.test, stringsAsFactors = TRUE,
                       na.strings = c("", "N/I", "N/A", "-"))
      # buildDataset() Stage 9 can drop every TEST row (NA in any predictor;
      # routine in spatial/chain datasets where a companion feature lacks
      # coverage). fread() types a header-only CSV all-logical, so predict()
      # would abort the whole run with a type mismatch. Skip the cell; the
      # Stage 2 metrics and OOF for it stand.
      if (nrow(DT.test) == 0L) {
        .WARN(sprintf("%s: TEST vac\u00edo - se omite predicci\u00f3n", .set))
        next
      }

      .file.train <- file.path(.path.data, paste0(.set, "_TRAIN.csv"))
      if (!file.exists(.file.train)) {
        next
      }
      DT.train <- fread(.file.train)
      # Type stability across the two split reads: fread infers integer vs
      # double per file, and gam/caret predict rejects features whose class
      # differs from the fit. Promote integers to double on both sides.
      for (.c in names(DT.test))
        if (is.integer(DT.test[[.c]]))  DT.test[,  (.c) := as.numeric(get(.c))]
      for (.c in names(DT.train))
        if (is.integer(DT.train[[.c]])) DT.train[, (.c) := as.numeric(get(.c))]
      Check <- .checkResponseSupport(DT.train, .set)
      if (!is.null(Check)) {
        .WARN(Check)
        next
      }
      DT.predict <- data.table()
      KEY <- setdiff(colnames(DT.test), colnames(DT.train))
      if(length(KEY) != 1){
        .ERROR("Missing keyID on",.file.test,". In order to bind parameters with results, a unique keyID column is required.")
      }
      
      
      # ==================== Single Model predictions====================
      R2 <- Scores[[.set]]$R2
      RMSE <- Scores[[.set]]$RMSE
      Yp <- list()
      # identify valid methods
      .valid_methods <- DT.metrics[set==.set & !is.na(r2) & !(method %in% c("ensemble.R2","ensemble.RMSE","ensemble"))]$method |> unique()
      for (.method in .valid_methods) {
        .file.model <- file.path(.path.model, paste0(.method, "_", .set, ".Rds"))
        if (!file.exists(.file.model)) {
          .WARN(.method, .set, " => skipping ensemble, no RDS file")
          # remove from ensemble weighting vectors
          R2[.method] <- NULL
          RMSE[.method] <- NULL
          # also remove from Yp
          Yp[[.method]] <- NULL
          # skip the rest of the code that calls readRDS
          next
        }
        model.train <- readRDS(.file.model)
        # Safe predicting with error handling
        yp <- tryCatch({
          # Safely predict and convert to numeric
          result <- predict(model.train, newdata = DT.test) |> as.numeric()
          # Return the result from the try block
          result
          
        }, error = function(e) {
          .ERROR(.method, .set, "predict failed:", e$message)
        })# end tryCatch
        
        # Check sonsistency
        # If NULL => skip method
        if (is.null(yp)) {
          .ERROR(.method, .set, " => predict() returned NULL.")
        }
        # If length mismatch => stop altogether
        if (length(yp) != nrow(DT.test)) {
          stop(sprintf("mismatch: got %d predictions but DT.test has %d",
                       length(yp), nrow(DT.test)))
        }
        
        # If partial NA => skip method
        if (anyNA(yp)) {
          .WARN(.method, .set, " => some NA predictions => skip method.")
          R2[.method]   <- NULL
          RMSE[.method] <- NULL
          Yp[[.method]] <- NULL
          next
        }
        
        
        # **************************************************************************************
        # Apply filters
        yp <- pmax(yp, .min)
        yp <- pmin(yp, .max)
        
        
        # **************************************************************************************
        
        # 1) Build a small "aux" table with the key, predicted 'value', and 'method'
        aux <- data.table(
          # e.g. "SampleID" for your KEY:
          keyID = DT.test[[KEY]],
          value = yp,
          method = .method,
          dataset = .dataset,
          response = .response
        )
        
        # 2) Append these rows to DT.predict in "long" format
        DT.predict <- rbindlist(list(DT.predict, aux), use.names = TRUE, fill = TRUE)
        # 3) Keep for potential weighting/ensemble calculations
        Yp[[.method]] <- yp
        
        
      }
      
      # ==================== R2 Ensemble Averaging ====================
      common_methods <- Reduce(intersect, list(names(Yp), names(R2), names(RMSE)))
      
      if (length(common_methods) == 0) {
        .WARN(.set, " => no valid methods for ensemble.")
        next
      }
      
      
      DT.Y <- as.data.table(Yp[common_methods])
      
      w <- unlist(R2[common_methods])
      if (any(is.na(w)) || sum(w) == 0) {
        W <- rep(1 / length(w), length(w))
      } else {
        W <- w / sum(w)
      }
      yp <- Reduce(`+`, Map(`*`, DT.Y, W))
      # Instead of: DT.predict[, ensemble.R2 := yp]
      aux <- data.table(
        keyID = DT.test[[KEY]],
        value = yp,
        method = "ensemble.R2",
        dataset = .dataset,
        response = .response
      )
      DT.predict <- rbindlist(list(DT.predict, aux), use.names = TRUE, fill = TRUE)
      .INFO("Weighted average(R2) ensemble predictions appended for:", .set, "")
      # ==================== RMSE Ensemble Averaging ====================
      
      w <- 1 / unlist(RMSE[common_methods]) # Invert RMSE to use it as weights
      if (any(is.na(w)) || sum(w) == 0) {
        W <- rep(1 / length(w), length(w))
      } else {
        W <- w / sum(w)
      }
      yp <- Reduce(`+`, Map(`*`, DT.Y, W))
      aux <- data.table(
        keyID = DT.test[[KEY]],
        value = yp,
        method = "ensemble.RMSE",
        dataset = .dataset,
        response = .response
      )
      
      DT.predict <- rbindlist(list(DT.predict, aux), use.names = TRUE, fill = TRUE)
      .INFO("Weighted average(RMSE) ensemble predictions appended for:", .set, "")
      
      # Rename keyID to SampleID
      setnames(DT.predict, "keyID", KEY)
      
      # Save predictions
      .file.response <- file.path(.path.response, paste0(.set, ".csv"))
      fwrite(DT.predict, .file.response)
      .INFO("Unseen predictions (R2/RMSE) saved to:", .file.response, "")
      
      
    } # end .datasets
  } # end .responses
  
  invisible(.metrics)
}
# nolint end

#' @export
which.nonnum <- function(x) {
  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
  which(badNum & !is.na(x))
}


#' @export
removeOutliersIQR <- function(.data, n = 3, q = 3) {
  # Drop rows that fall outside the IQR envelope of the most important numeric
  # predictors. A row is dropped when it is an outlier in ANY of them, so the
  # envelopes narrow the surviving set together rather than in isolation.
  MODEL <- train(
    y ~ .,
    data      = .data,
    method    = "ranger",
    trControl = trainControl(method = "none"),
    importance = "impurity"
  )
  IVCOL <- as.data.table(
    varImp(MODEL, useModel = TRUE, nonpara = FALSE, scale = FALSE)$importance,
    keep.rownames = TRUE
  )
  COLS <- head(IVCOL[order(-Overall)]$rn, n)
  COLS <- COLS[vapply(COLS, function(s) is.numeric(.data[[s]]), logical(1L))]

  OUT <- as.data.table(.data)
  for (s in COLS) {
    x <- OUT[[s]]
    Q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    Spread <- Q[2L] - Q[1L]
    # A missing value cannot be judged against the envelope, so its row stays:
    # this function removes outliers, not incomplete rows.
    OUT <- OUT[is.na(x) | (x >= Q[1L] - q * Spread & x <= Q[2L] + q * Spread)]
  }
  OUT
}


# *****************************************************************************
# detectOutliers: Industrial outlier detection for regression training data
#
# Three independent criteria (each can be disabled by setting threshold = Inf):
#
#   1. Modified Z-score on y  (Iglewicz & Hoaglin 1993)
#      M = 0.6745 * |y - median(y)| / MAD
#      Threshold 3.5 - published recommendation, no distributional assumption.
#
#   2. LOF on numeric feature matrix  (Breunig et al. 2000, ACM SIGMOD)
#      Local Outlier Factor: ratio of local density vs k-neighbor density.
#      LOF approx 1 = inlier; LOF >> 1 = local outlier.
#      Default threshold 2.0; minPts = 5.
#
#   3. Cook's distance  (Cook 1977)  - optional, use.cooks = FALSE by default
#      D_i = influence of observation i on all fitted values in a linear proxy.
#      Threshold: 4 / n (standard rule of thumb).
#      Requires fitting lm(y ~ .) - use only when a linear relationship is expected.
#
# Returns DT with added columns:
#   .score.mad, .score.lof, .score.cooks (if use.cooks), .outlier
# Does NOT delete rows - caller decides what to do with the flag.
#
# References:
#   Iglewicz & Hoaglin (1993). How to Detect and Handle Outliers. ASQC.
#   Breunig et al. (2000). LOF. ACM SIGMOD, 93-104.
#   Cook, R.D. (1977). Technometrics, 19(1), 15-18.
# *****************************************************************************
#' @export
detectOutliers <- function(DT,
                           threshold.mad   = 3.5,
                           threshold.lof   = 2.0,
                           threshold.cooks = Inf,    # Inf = disabled; 4/nrow(DT) = standard
                           minPts          = 5,
                           use.cooks       = FALSE) {
  DT <- data.table::copy(DT)
  .n <- nrow(DT)

  # 1. Modified Z-score on y (Iglewicz & Hoaglin 1993)
  .med       <- median(DT$y, na.rm = TRUE)
  .mad       <- mad(DT$y, center = .med, constant = 1, na.rm = TRUE)
  .score.mad <- if (.mad > 0) 0.6745 * abs(DT$y - .med) / .mad else rep(0, .n)

  # 2. LOF on numeric feature columns (Breunig 2000)
  .num_cols  <- names(DT)[sapply(DT, is.numeric) & names(DT) != "y"]
  .X         <- as.matrix(DT[, .num_cols, with = FALSE])
  .score.lof <- if (.n > minPts && ncol(.X) > 0)
    dbscan::lof(.X, minPts = minPts)
  else
    rep(1, .n)

  DT[, .score.mad := .score.mad]
  DT[, .score.lof := .score.lof]
  .flag <- .score.mad > threshold.mad | .score.lof > threshold.lof

  # 3. Cook's distance (Cook 1977) - optional linear proxy
  if (use.cooks && length(.num_cols) > 0) {
    .lm          <- tryCatch(lm(y ~ ., data = DT[, c(.num_cols, "y"), with = FALSE]),
                             error = function(e) NULL)
    .score.cooks <- if (!is.null(.lm)) cooks.distance(.lm) else rep(0, .n)
    .thr.cooks   <- if (is.finite(threshold.cooks)) threshold.cooks else 4 / .n
    DT[, .score.cooks := .score.cooks]
    .flag <- .flag | .score.cooks > .thr.cooks
  }

  DT[, .outlier := .flag]
  DT
}


.buildFigure <- function(PLOT, DATA, DELAY=0.3, build_dir="_build", params=list(background="white")) {
  FILE <- tempfile(tmpdir = build_dir, pattern="fig", fileext=".png")
  AUX <- tempfile(pattern="fig", fileext=".html")
  if(!dir.exists(build_dir)) {
    dir.create(build_dir)
  }
  # Ensure cleanup of temporary files
  on.exit({
    unlink(AUX)
  }, add = TRUE)
  
  XDELAY <- max(DELAY, round(nrow(DATA) / 1500 * DELAY, digits = 2))
  htmlwidgets::saveWidget(widget = PLOT, background=params$background, file = AUX, selfcontained = TRUE)
  webshot2::webshot(delay=XDELAY, url = AUX, file = FILE)
  FIG <- knitr::include_graphics(path=FILE)
  
  return(FIG)
}
