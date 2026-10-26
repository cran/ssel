# An empty or partial audit must be distinguishable from a clean one. The
# response pattern is a deliberate filter, so a name it excludes stays silent;
# a file it matches that cannot be audited is a hole and is reported.

makeAuditFixture <- function() {
  Root <- file.path(tempdir(), paste0("ssel-auditdiag-", Sys.getpid(), "-",
                                      sample.int(1e6, 1L)))
  Model <- file.path(Root, "model")
  Train <- file.path(Root, "train")
  dir.create(Model, recursive = TRUE)
  dir.create(Train, recursive = TRUE)

  data.table::fwrite(data.table::data.table(x = c(1, 2, 3, 4),
                                            y = c(1, 3, 5, 7)),
                     file.path(Train, "targetA_demo_TRAIN.csv"))
  data.table::fwrite(data.table::data.table(SampleID = c("S1", "S2", "S3", "S4")),
                     file.path(Train, "targetA_demo_TRAIN_ids.csv"))
  saveRDS(
    list(pred = data.frame(tune = 1L, pred = c(1.1, 2.9, 5.2, 6.8),
                           obs = c(1, 3, 5, 7), rowIndex = 1:4),
         bestTune = data.frame(tune = 1L)),
    file.path(Model, "lm_targetA_demo.Rds")
  )
  list(root = Root, model = Model, train = Train)
}

test_that("a response pattern that excludes every model is reported, not silent", {
  FIX <- makeAuditFixture()
  on.exit(unlink(FIX$root, recursive = TRUE), add = TRUE)

  expect_warning(
    OUT <- oofEnsemble(.path.model = FIX$model, .path.train = FIX$train,
                       .response_pattern = "target"),
    "no filename matched the response pattern 'target'"
  )
  expect_equal(nrow(OUT), 0L)
})

test_that("a matching model that cannot be audited is named with its reason", {
  FIX <- makeAuditFixture()
  on.exit(unlink(FIX$root, recursive = TRUE), add = TRUE)
  # Matches the pattern, but its TRAIN companion does not exist.
  saveRDS(list(pred = data.frame(), bestTune = data.frame()),
          file.path(FIX$model, "lm_targetB_demo.Rds"))

  expect_warning(
    oofEnsemble(.path.model = FIX$model, .path.train = FIX$train,
                .response_pattern = "targetA|targetB"),
    "lm_targetB_demo\\.Rds \\(TRAIN file not found\\)"
  )
})

test_that("a complete audit warns about nothing", {
  FIX <- makeAuditFixture()
  on.exit(unlink(FIX$root, recursive = TRUE), add = TRUE)

  expect_no_warning(
    OUT <- oofEnsemble(.path.model = FIX$model, .path.train = FIX$train,
                       .response_pattern = "targetA")
  )
  expect_equal(OUT$SampleID, c("S1", "S2", "S3", "S4"))
})

test_that("auditOverfit reports an unreadable model and leaves a prior file", {
  FIX <- makeAuditFixture()
  on.exit(unlink(FIX$root, recursive = TRUE), add = TRUE)
  Summary <- file.path(FIX$root, "summary")
  dir.create(Summary)
  writeLines("previous,run", file.path(Summary, "overfit.csv"))
  Before <- file.size(file.path(Summary, "overfit.csv"))
  # Matches the pattern and has its TRAIN companion, so the audit reaches the
  # model and fails to read it.
  data.table::fwrite(data.table::data.table(x = c(1, 2), y = c(1, 3)),
                     file.path(FIX$train, "targetB_demo_TRAIN.csv"))
  writeLines("not an RDS", file.path(FIX$model, "lm_targetB_demo.Rds"))

  # Three conditions are the contract here: the named skip, the empty-audit
  # report, and fwrite() declining a column-less table.
  Warnings <- capture_warnings(
    auditOverfit(.path.model = FIX$model, .path.train = FIX$train,
                 .path.summary = Summary,
                 .response_pattern = "targetB", verbose = FALSE)
  )
  expect_match(Warnings, "lm_targetB_demo\\.Rds \\(model could not be read\\)",
               all = FALSE)
  expect_match(Warnings, "nothing was audited from 2 model file\\(s\\)",
               all = FALSE)
  expect_match(Warnings, "Input has no columns", all = FALSE)
  # fwrite() refuses a column-less table, so the earlier verdict survives.
  expect_equal(file.size(file.path(Summary, "overfit.csv")), Before)
})

test_that("auditQuantiles names the directory, the file count and the pattern", {
  FIX <- makeAuditFixture()
  on.exit(unlink(FIX$root, recursive = TRUE), add = TRUE)
  Summary <- file.path(FIX$root, "summary")

  expect_error(
    suppressWarnings(
      auditQuantiles(.path.model = FIX$model, .path.train = FIX$train,
                     .path.summary = Summary,
                     .response_pattern = "target", verbose = FALSE)
    ),
    "1 model file\\(s\\) in .*under the response pattern 'target'"
  )
})
