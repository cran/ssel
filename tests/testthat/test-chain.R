test_that("chainPipeline reports incomplete resume state clearly", {
  Root <- file.path(tempdir(), paste0("ssel-chain-", Sys.getpid(), "-",
                                      sample.int(1e6, 1L)))
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  Datasets <- file.path(Root, "datasets")
  Train    <- file.path(Root, "train")
  Model    <- file.path(Root, "model")
  Metrics  <- file.path(Root, "metrics")
  Response <- file.path(Root, "response")
  Summary  <- file.path(Root, "summary")
  Export   <- file.path(Root, "export")
  Iter     <- file.path(Root, "iter")
  dir.create(Iter, recursive = TRUE)

  data.table::fwrite(data.table::data.table(iter = 0L, delta = 0),
                     file.path(Iter, "convergence.csv"))
  data.table::fwrite(data.table::data.table(SampleID = "S1", y = 1),
                     file.path(Iter, "Y.csv"))

  expect_error(
    chainPipeline(.path.datasets = Datasets, .path.train = Train,
                  .path.model = Model, .path.metrics = Metrics,
                  .path.response = Response, .path.summary = Summary,
                  .path.export = Export, .path.iter = Iter,
                  key = "SampleID", responses = "y", datasets = "D",
                  maxIter = 1L, tol = 0.01, patience = 1L,
                  resume = TRUE),
    "resume=TRUE but resume state is incomplete"
  )
})

test_that("chain summary stitch keeps response artifacts on the selected iter", {
  Root <- file.path(tempdir(), paste0("ssel-chain-stitch-", Sys.getpid(), "-",
                                      sample.int(1e6, 1L)))
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  Summary <- file.path(Root, "summary")
  Iter    <- file.path(Root, "iter")
  dir.create(Summary, recursive = TRUE)
  dir.create(.chainSummaryDir(Iter, 0L), recursive = TRUE)
  dir.create(.chainSummaryDir(Iter, 1L), recursive = TRUE)

  Responses <- c("xrd.A", "xrd.B")
  Best      <- c("xrd.A" = 0L, "xrd.B" = 1L)

  writeSnapshot <- function(iter) {
    Dir <- .chainSummaryDir(Iter, iter)
    data.table::fwrite(
      data.table::data.table(response = Responses, dataset = "D",
                             method = "ensemble.RMSE", rmse = iter,
                             r2 = iter + seq_along(Responses)),
      file.path(Dir, "metrics.csv")
    )
    data.table::fwrite(
      data.table::data.table(response = Responses, dataset = "D",
                             SampleID = paste0("S", seq_along(Responses)),
                             method = "ensemble.RMSE", value = iter),
      file.path(Dir, "response_long.csv")
    )
    data.table::fwrite(
      data.table::data.table(response = Responses, dataset = "D",
                             rowIndex = seq_along(Responses),
                             SampleID = paste0("S", seq_along(Responses)),
                             y_obs = iter, y_hat_oof = iter,
                             residual = 0),
      file.path(Dir, "residuals_oof.csv")
    )
    data.table::fwrite(
      data.table::data.table(response = Responses, dataset = "D",
                             n = 2L, q_50 = iter, q_90 = iter, q_95 = iter),
      file.path(Dir, "prediction_quantiles.csv")
    )
    data.table::fwrite(
      data.table::data.table(response = Responses, dataset = "D",
                             method = "ranger", rel_optimism = iter),
      file.path(Dir, "overfit.csv")
    )
  }
  writeSnapshot(0L)
  writeSnapshot(1L)

  .stitchChainSummary(Summary, Iter, Responses, Best)

  for (File in .chainSummaryFiles) {
    DT <- data.table::fread(file.path(Summary, File))
    expect_equal(DT[response == "xrd.A"][["dataset"]], "D")
    expect_equal(DT[response == "xrd.B"][["dataset"]], "D")
  }
  expect_equal(data.table::fread(file.path(Summary, "metrics.csv"))[
    response == "xrd.A", rmse], 0)
  expect_equal(data.table::fread(file.path(Summary, "metrics.csv"))[
    response == "xrd.B", rmse], 1)
  expect_equal(data.table::fread(file.path(Summary, "prediction_quantiles.csv"))[
    response == "xrd.A", q_50], 0)
  expect_equal(data.table::fread(file.path(Summary, "prediction_quantiles.csv"))[
    response == "xrd.B", q_50], 1)
})

test_that(".chainBestIters excludes responses without iteration-0 metrics", {
  M0 <- data.table::data.table(response = "a", dataset = "D",
                               method = "ensemble.RMSE", rmse = 1, r2 = 0.5)
  Conv <- data.table::data.table(iter = 1L, r2_a = 0.7, r2_b = 0.9)

  expect_warning(
    Best <- .chainBestIters(c("a", "b"), M0, Conv),
    "no iteration-0 metrics for b"
  )
  expect_identical(Best, c(a = 1L))

  expect_error(
    suppressWarnings(.chainBestIters("b", M0[0L], Conv)),
    "nothing to stitch"
  )
})
