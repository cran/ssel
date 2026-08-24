test_that("trainRegressionModel skips constant-response cells clearly", {
  Root <- file.path(tempdir(), paste0("ssel-constant-", Sys.getpid(), "-",
                                      sample.int(1e6, 1L)))
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  Train    <- file.path(Root, "train")
  Model    <- file.path(Root, "model")
  Metrics  <- file.path(Root, "metrics")
  Response <- file.path(Root, "response")
  dir.create(Train, recursive = TRUE)
  dir.create(Model, recursive = TRUE)
  dir.create(Metrics, recursive = TRUE)
  dir.create(Response, recursive = TRUE)

  data.table::fwrite(
    data.table::data.table(x = seq_len(12L), y = 0),
    file.path(Train, "xrd.Const_CLAS_TRAIN.csv")
  )

  expect_warning(
    suppressMessages(
      trainRegressionModel(.path.data = Train, .path.model = Model,
                           .path.metrics = Metrics, .path.response = Response,
                           TRAIN = TRUE, PARALLEL = FALSE,
                           .responses = "xrd.Const", .datasets = "CLAS",
                           .methods = "ranger")
    ),
    "response is constant in training data"
  )
  expect_length(list.files(Model, pattern = "\\.Rds$"), 0L)
  expect_length(list.files(Metrics, pattern = "\\.csv$"), 0L)
  expect_length(list.files(Response, pattern = "\\.csv$"), 0L)
})

test_that("trainRegressionModel caps and cleans its parallel backend", {
  Workers <- NA_integer_
  Stopped <- FALSE
  Serial <- FALSE
  testthat::local_mocked_bindings(
    detectCores = function(...) 32L,
    makePSOCKcluster = function(n) {
      Workers <<- n
      "cluster"
    },
    registerDoParallel = function(cluster) invisible(cluster),
    stopCluster = function(cluster) {
      Stopped <<- identical(cluster, "cluster")
      invisible(NULL)
    },
    registerDoSEQ = function() {
      Serial <<- TRUE
      invisible(NULL)
    },
    .package = "ssel"
  )

  Messages <- testthat::capture_messages(
    trainRegressionModel(.path.data = tempdir(), .path.model = tempdir(),
                         .path.metrics = tempdir(), .path.response = tempdir(),
                         TRAIN = TRUE, PARALLEL = TRUE,
                         .responses = character(0))
  )
  expect_match(paste(Messages, collapse = "\n"), "using 2 cores")
  expect_equal(Workers, 2L)
  expect_true(Stopped)
  expect_true(Serial)
})

test_that("trainRegressionModel rejects a missing prediction key", {
  Root <- file.path(tempdir(), paste0("ssel-key-", Sys.getpid(), "-",
                                      sample.int(1e6, 1L)))
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  Data <- file.path(Root, "data")
  Model <- file.path(Root, "model")
  Metrics <- file.path(Root, "metrics")
  Response <- file.path(Root, "response")
  for (Dir in c(Data, Model, Metrics, Response)) dir.create(Dir, recursive = TRUE)

  DT <- data.table::data.table(x = seq_len(6L), y = c(1, 2, 1, 2, 1, 2))
  data.table::fwrite(DT, file.path(Data, "y_D_TRAIN.csv"))
  data.table::fwrite(DT, file.path(Data, "y_D_TEST.csv"))

  expect_error(
    suppressMessages(suppressWarnings(
      trainRegressionModel(.path.data = Data, .path.model = Model,
                           .path.metrics = Metrics, .path.response = Response,
                           TRAIN = FALSE, PARALLEL = FALSE,
                           .responses = "y", .datasets = "D",
                           .methods = character(0))
    )),
    "unique keyID column is required"
  )
})

test_that("trainRegressionModel skips prediction when TEST has no rows", {
  Root <- tempfile("ssel-empty-test-")
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  Data <- file.path(Root, "data")
  Model <- file.path(Root, "model")
  Metrics <- file.path(Root, "metrics")
  Response <- file.path(Root, "response")
  for (Dir in c(Data, Model, Metrics, Response)) dir.create(Dir, recursive = TRUE)

  Observed <- c(0, 1, 2, 3)
  data.table::fwrite(
    data.table::data.table(x = seq_along(Observed), y = Observed),
    file.path(Data, "y_D_TRAIN.csv")
  )
  # Header-only TEST: buildDataset() Stage 9 drops every TEST row when each
  # one carries an NA in some predictor (routine in spatial/chain datasets).
  data.table::fwrite(
    data.table::data.table(x = numeric(0), SampleID = character(0)),
    file.path(Data, "y_D_TEST.csv")
  )

  Fit <- stats::lm(y ~ 1, data = data.frame(y = Observed))
  Fit$pred <- data.frame(
    tune = 1L,
    pred = Observed + c(1, -1, 1, -1),
    obs = Observed,
    rowIndex = seq_along(Observed)
  )
  Fit$bestTune <- data.frame(tune = 1L)
  saveRDS(Fit, file.path(Model, "left_y_D.Rds"))

  DT.metrics <- NULL
  expect_warning(
    DT.metrics <- suppressMessages(
      trainRegressionModel(.path.data = Data, .path.model = Model,
                           .path.metrics = Metrics, .path.response = Response,
                           TRAIN = FALSE, PARALLEL = FALSE,
                           .responses = "y", .datasets = "D",
                           .methods = "left")
    ),
    "TEST vac\u00edo - se omite predicci\u00f3n"
  )

  # Stage 2 metrics for the cell survive; only the prediction is skipped.
  expect_equal(nrow(DT.metrics[method == "left" & set == "y_D"]), 1L)
  expect_true(file.exists(file.path(Metrics, "y_D.csv")))
  expect_length(list.files(Response, pattern = "\\.csv$"), 0L)
})

test_that("trainRegressionModel keeps Stage 3 weights cell-specific", {
  runFixture <- function(Responses) {
    Root <- tempfile("ssel-stage3-weights-")
    on.exit(unlink(Root, recursive = TRUE), add = TRUE)

    Data <- file.path(Root, "data")
    Model <- file.path(Root, "model")
    Metrics <- file.path(Root, "metrics")
    Response <- file.path(Root, "response")
    for (Dir in c(Data, Model, Metrics, Response)) {
      dir.create(Dir, recursive = TRUE)
    }

    Methods <- c("left", "right")
    Observed <- c(0, 1, 2, 3)
    Errors <- list(
      alpha = list(left = c(1, -1, 1, -1),
                   right = c(3, -3, 3, -3)),
      beta = list(left = c(4, -4, 4, -4),
                  right = c(1, -1, 1, -1))
    )
    TestValues <- c(left = 10, right = 30)

    for (ResponseID in names(Errors)) {
      data.table::fwrite(
        data.table::data.table(x = seq_along(Observed), y = Observed),
        file.path(Data, paste0(ResponseID, "_D_TRAIN.csv"))
      )
      data.table::fwrite(
        data.table::data.table(x = c(5, 6), SampleID = c("P1", "P2")),
        file.path(Data, paste0(ResponseID, "_D_TEST.csv"))
      )

      for (MethodID in Methods) {
        Fit <- stats::lm(
          y ~ 1,
          data = data.frame(y = rep(TestValues[[MethodID]], 4))
        )
        Fit$pred <- data.frame(
          tune = 1L,
          pred = Observed + Errors[[ResponseID]][[MethodID]],
          obs = Observed,
          rowIndex = seq_along(Observed)
        )
        Fit$bestTune <- data.frame(tune = 1L)
        saveRDS(Fit, file.path(Model, paste0(MethodID, "_", ResponseID, "_D.Rds")))
      }
    }

    DT.metrics <- suppressMessages(
      trainRegressionModel(
        .path.data = Data,
        .path.model = Model,
        .path.metrics = Metrics,
        .path.response = Response,
        TRAIN = FALSE,
        PARALLEL = FALSE,
        .responses = Responses,
        .datasets = "D",
        .methods = Methods
      )
    )

    for (ResponseID in names(Errors)) {
      RMSE <- vapply(
        Errors[[ResponseID]],
        function(x) sqrt(mean(x^2)),
        numeric(1)
      )
      R2 <- vapply(
        Errors[[ResponseID]],
        function(x) stats::cor(Observed, Observed + x)^2,
        numeric(1)
      )
      Prediction <- lapply(Methods, function(MethodID) {
        Observed + Errors[[ResponseID]][[MethodID]]
      })
      names(Prediction) <- Methods

      ExpectedOOFR2 <- Reduce(`+`, Map(`*`, Prediction, R2 / sum(R2)))
      ExpectedOOFRMSE <- Reduce(`+`, Map(`*`, Prediction, (1 / RMSE) / sum(1 / RMSE)))

      AUX <- DT.metrics[
        response == ResponseID & method %in% Methods
      ][match(Methods, method)]
      expect_equal(AUX$rmse, unname(RMSE[Methods]))
      expect_equal(AUX$r2, unname(R2[Methods]))
      expect_equal(
        DT.metrics[response == ResponseID & method == "ensemble.R2", rmse],
        sqrt(mean((Observed - ExpectedOOFR2)^2))
      )
      expect_equal(
        DT.metrics[response == ResponseID & method == "ensemble.RMSE", rmse],
        sqrt(mean((Observed - ExpectedOOFRMSE)^2))
      )

      DT <- data.table::fread(file.path(Response, paste0(ResponseID, "_D.csv")))
      expect_equal(
        DT[method == "ensemble.R2", value],
        rep(sum(TestValues * R2 / sum(R2)), 2)
      )
      expect_equal(
        DT[method == "ensemble.RMSE", value],
        rep(sum(TestValues * (1 / RMSE) / sum(1 / RMSE)), 2)
      )
    }
  }

  runFixture(c("alpha", "beta"))
  runFixture(c("beta", "alpha"))
})
