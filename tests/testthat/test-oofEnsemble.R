test_that("oofEnsemble discovers mixed-case method identifiers", {
  Root <- file.path(tempdir(), paste0("ssel-oof-method-", Sys.getpid(), "-",
                                      sample.int(1e6, 1L)))
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  Model <- file.path(Root, "model")
  Train <- file.path(Root, "train")
  dir.create(Model, recursive = TRUE)
  dir.create(Train, recursive = TRUE)

  data.table::fwrite(data.table::data.table(y = c(1, 3)),
                     file.path(Train, "target_D_TRAIN.csv"))
  data.table::fwrite(data.table::data.table(SampleID = c("S1", "S2")),
                     file.path(Train, "target_D_TRAIN_ids.csv"))

  writeModel <- function(method, prediction) {
    saveRDS(
      list(
        pred = data.frame(tune = 1L, pred = prediction,
                          obs = c(1, 3), rowIndex = 1:2),
        bestTune = data.frame(tune = 1L)
      ),
      file.path(Model, paste0(method, "_target_D.Rds"))
    )
  }
  writeModel(method = "lower", prediction = c(0, 2))
  writeModel(method = "mixedCase", prediction = c(0.5, 2.5))

  DT <- oofEnsemble(.path.model = Model, .path.train = Train,
                    .response_pattern = "target")
  data.table::setorder(DT, rowIndex)

  expect_equal(DT$SampleID, c("S1", "S2"))
  expect_equal(DT$y_hat_oof, c(1 / 3, 7 / 3))
})
