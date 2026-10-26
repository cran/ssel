test_that(".checkBound accepts single numeric values, including Inf and -Inf", {
  expect_identical(.checkBound(100, "boundsMax"), 100)
  expect_identical(.checkBound(0, "boundsMin"), 0)
  expect_identical(.checkBound(-1.5, ".min"), -1.5)
  expect_identical(.checkBound(-Inf, ".min"), -Inf)
  expect_identical(.checkBound(Inf, ".max"), Inf)
})

test_that(".checkBound refuses strings, NA, vectors and NULL, naming the argument", {
  expect_error(.checkBound("100", "boundsMax"),    "boundsMax")
  expect_error(.checkBound(NA_real_, "boundsMin"), "boundsMin")
  expect_error(.checkBound(NaN, ".max"),           "[.]max")
  expect_error(.checkBound(numeric(0), ".min"),    "[.]min")
  expect_error(.checkBound(c(1, 2), ".max"),       "[.]max")
  expect_error(.checkBound(NULL, "boundsMin"),     "boundsMin")
  expect_error(.checkBound(TRUE, ".min"),          "[.]min")
})

test_that("postProcess refuses a string bound before touching inputs", {
  Root <- tempfile("ssel-bounds-")
  dir.create(Root, recursive = TRUE)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  expect_error(
    postProcess(.path.summary = file.path(Root, "summary"),
                .path.export  = file.path(Root, "export"),
                key = "SampleID", responses = "y", location = "x",
                boundsMax = "100"),
    "boundsMax"
  )
})

test_that("modelPipeline refuses a non-numeric bound before any effect", {
  Options <- options(aR.quiet = TRUE)
  on.exit(options(Options), add = TRUE)
  Root <- tempfile("ssel-bounds-")
  dir.create(file.path(Root, "datasets"), recursive = TRUE)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  expect_error(
    modelPipeline(.path.datasets = file.path(Root, "datasets"),
                  .path.train    = file.path(Root, "train"),
                  .path.model    = file.path(Root, "model"),
                  .path.metrics  = file.path(Root, "metrics"),
                  .path.response = file.path(Root, "response"),
                  .path.summary  = file.path(Root, "summary"),
                  .path.export   = file.path(Root, "export"),
                  key = "SampleID", responses = "y", datasets = "demo",
                  methods = "lm", seed = 1, boundsMin = "low"),
    "boundsMin"
  )
  expect_false(dir.exists(file.path(Root, "train")))
  expect_false(dir.exists(file.path(Root, "model")))
})
