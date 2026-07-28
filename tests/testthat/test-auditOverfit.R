test_that("auditOverfit discovers mixed-case caret methods", {
  Root <- file.path(tempdir(), paste0("ssel-overfit-", Sys.getpid(), "-",
                                      sample.int(1e6, 1L)))
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  Model <- file.path(Root, "model")
  Train <- file.path(Root, "train")
  Summary <- file.path(Root, "summary")
  dir.create(Model, recursive = TRUE)
  dir.create(Train, recursive = TRUE)

  DT <- data.table::data.table(x = seq_len(8L), y = c(1, 2, 2, 4, 5, 5, 7, 8))
  Fit <- stats::lm(y ~ x, data = DT)
  Fit$resample <- data.frame(RMSE = c(0.2, 0.3), Rsquared = c(0.8, 0.9))
  saveRDS(Fit, file.path(Model, "xgbTree_xrd.A_D.Rds"))
  data.table::fwrite(DT, file.path(Train, "xrd.A_D_TRAIN.csv"))

  OUT <- auditOverfit(.path.model = Model, .path.train = Train,
                      .path.summary = Summary,
                      .response_pattern = "xrd[.]A", verbose = FALSE)

  expect_equal(OUT$overfit$method, "xgbTree")
  expect_equal(OUT$overfit$response, "xrd.A")
  expect_true(file.exists(file.path(Summary, "overfit.csv")))
})
