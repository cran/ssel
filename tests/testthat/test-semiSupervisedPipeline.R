test_that("semi final delivery covers promoted keys for multiple responses", {
  library(data.table)
  Root <- tempfile("ssel-semi-")
  dir.create(file.path(Root, "datasets"), recursive = TRUE)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  Options <- options(aR.quiet = TRUE)
  on.exit(options(Options), add = TRUE)

  # Public chain example: two targets share two original prediction keys.
  DT <- data.table(
    SampleID = c(paste0("S", 1:18), "P1", "P2"),
    set = c(rep("train", 18), "predict", "predict"),
    x = 1:20, z = rep(c(0, 1), 10),
    targetA = c(1, 2.4, 1.8, 3.7, 2.9, 5.1, 4, 6.2, 5.4,
                7, 6.1, 8.3, 7.2, 9.1, 8, 10.5, 9.4, 11.2, NA, NA),
    targetB = c(2.2, 1.3, 3.6, 2.7, 4.1, 3.8, 5.7, 4.6, 6.5,
                5.9, 7.8, 6.4, 8.6, 7.5, 9.9, 8.7, 10.4, 9.6, NA, NA)
  )
  fwrite(DT, file.path(Root, "datasets/demo.csv"))
  Result <- semiSupervisedPipeline(
    .path.datasets = file.path(Root, "datasets"),
    .path.work = file.path(Root, "work"), key = "SampleID",
    responses = c("targetA", "targetB"), datasets = "demo",
    methods = c("lm", "glm"), seed = 2026, tau = 0.1,
    maxRounds = 1, epsRevert = Inf, promotionCap = 0.5
  )
  expect_identical(tail(Result$history$action, 1L), "accept")
  expect_equal(uniqueN(Result$promoted$SampleID), 1L)
  for (Response in c("targetA", "targetB")) {
    DT <- fread(file.path(Root, "work/round/summary/response_long.csv"))
    expect_setequal(DT[response == Response, SampleID], c("P1", "P2"))
    DT <- fread(file.path(Root, "work/round/export/Yo.csv"))
    expect_equal(nrow(DT[response == Response]), 6L)
    expect_setequal(DT[response == Response, SampleID], c("P1", "P2"))
    COLS <- names(fread(file.path(Root, "work/round/train",
                                  paste0(Response, "_demo_TRAIN.csv")), nrows = 0L))
    expect_identical(names(fread(file.path(Root, "work/round/train",
                                         paste0(Response, "_demo_TEST.csv")), nrows = 0L)),
                     c("SampleID", setdiff(COLS, "y")))
  }
  expect_setequal(fread(file.path(Root, "work/round/export/Xo.csv"))$SampleID, c("P1", "P2"))
  expect_equal(nrow(fread(file.path(Root, "work/round/export/X.csv"))), 18L)
})
