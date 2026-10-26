test_that("modelPipeline refuses to wipe the datasets directory", {
  Options <- options(aR.quiet = TRUE)
  on.exit(options(Options), add = TRUE)
  Root <- tempfile("ssel-wipeguard-")
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  Datasets <- file.path(Root, "datasets")
  dir.create(Datasets, recursive = TRUE)
  data.table::fwrite(data.frame(SampleID = c("S1", "P1"), set = c("train", "predict"),
                                x = 1:2, y = c(1, NA)),
                     file.path(Datasets, "demo.csv"))

  expect_error(
    modelPipeline(.path.datasets = Datasets, .path.train = file.path(Root, "train"),
                  .path.model = file.path(Root, "model"),
                  .path.metrics = file.path(Root, "metrics"),
                  .path.response = file.path(Root, "response"),
                  .path.summary = file.path(Root, "summary"),
                  .path.export = Datasets,
                  key = "SampleID", responses = "y", datasets = "demo",
                  methods = "lm", seed = 1),
    "Refusing to wipe"
  )
  expect_true(file.exists(file.path(Datasets, "demo.csv")))
})

test_that("modelPipeline refuses a wipe destination inside the datasets directory", {
  Options <- options(aR.quiet = TRUE)
  on.exit(options(Options), add = TRUE)
  Root <- tempfile("ssel-wipeguard-")
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  Datasets <- file.path(Root, "datasets")
  dir.create(Datasets, recursive = TRUE)

  expect_error(
    modelPipeline(.path.datasets = Datasets, .path.train = file.path(Root, "train"),
                  .path.model = file.path(Root, "model"),
                  .path.metrics = file.path(Root, "metrics"),
                  .path.response = file.path(Root, "response"),
                  .path.summary = file.path(Datasets, "summary"),
                  .path.export = file.path(Root, "export"),
                  key = "SampleID", responses = "y", datasets = "demo",
                  methods = "lm", seed = 1),
    "inside the datasets directory"
  )
})

test_that("modelPipeline refuses coinciding declared paths", {
  Options <- options(aR.quiet = TRUE)
  on.exit(options(Options), add = TRUE)
  Root <- tempfile("ssel-wipeguard-")
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  Datasets <- file.path(Root, "datasets")
  dir.create(Datasets, recursive = TRUE)
  Same <- file.path(Root, "same")

  expect_error(
    modelPipeline(.path.datasets = Datasets, .path.train = Same,
                  .path.model = file.path(Root, "model"),
                  .path.metrics = Same,
                  .path.response = file.path(Root, "response"),
                  .path.summary = file.path(Root, "summary"),
                  .path.export = file.path(Root, "export"),
                  key = "SampleID", responses = "y", datasets = "demo",
                  methods = "lm", seed = 1),
    "declared paths coincide"
  )
})

test_that("modelPipeline refuses to wipe a directory containing the working directory", {
  Options <- options(aR.quiet = TRUE)
  on.exit(options(Options), add = TRUE)
  Root <- tempfile("ssel-wipeguard-")
  dir.create(Root, recursive = TRUE)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  Inputs <- tempfile("ssel-wipeguard-inputs-")
  dir.create(file.path(Inputs, "datasets"), recursive = TRUE)
  on.exit(unlink(Inputs, recursive = TRUE), add = TRUE)
  Work <- file.path(Root, "work")
  dir.create(Work)
  Old <- setwd(Work)
  on.exit(setwd(Old), add = TRUE)

  expect_error(
    modelPipeline(.path.datasets = file.path(Inputs, "datasets"),
                  .path.train = file.path(Inputs, "train"),
                  .path.model = file.path(Inputs, "model"),
                  .path.metrics = file.path(Inputs, "metrics"),
                  .path.response = file.path(Inputs, "response"),
                  .path.summary = file.path(Inputs, "summary"),
                  .path.export = Root,
                  key = "SampleID", responses = "y", datasets = "demo",
                  methods = "lm", seed = 1),
    "working directory"
  )
})
