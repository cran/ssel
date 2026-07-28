test_that("buildDataset legacy feature inference is unchanged", {
  Root <- file.path(tempdir(), paste0("ssel-buildDataset-legacy-",
                                      Sys.getpid(), "-",
                                      sample.int(1e6, 1L)))
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  Datasets <- file.path(Root, "datasets")
  Train    <- file.path(Root, "train")
  dir.create(Datasets, recursive = TRUE)
  dir.create(Train, recursive = TRUE)

  data.table::fwrite(
    data.table::data.table(
      SampleID = paste0("S", 1:4),
      set = c("train", "train", "predict", "predict"),
      f1 = c(1, 2, 3, 4),
      f2 = c(4, 3, 2, 1),
      xrd.A = c(10, 20, NA, NA),
      xrd.B = c(1, 2, NA, NA)
    ),
    file.path(Datasets, "D.csv")
  )

  buildDataset(.path.input = Datasets, .path.train = Train,
               Y.PATTERN = "^(xrd.A)$", KEY = "SampleID",
               SET_COL = "set")

  DT <- data.table::fread(file.path(Train, "xrd.A_D_TRAIN.csv"))
  expect_setequal(names(DT), c("f1", "f2", "xrd.B", "y"))
})

test_that("buildDataset uses exact features when supplied", {
  Root <- file.path(tempdir(), paste0("ssel-buildDataset-features-",
                                      Sys.getpid(), "-",
                                      sample.int(1e6, 1L)))
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  Datasets <- file.path(Root, "datasets")
  Train    <- file.path(Root, "train")
  dir.create(Datasets, recursive = TRUE)
  dir.create(Train, recursive = TRUE)

  data.table::fwrite(
    data.table::data.table(
      SampleID = paste0("S", 1:4),
      set = c("train", "train", "predict", "predict"),
      f1 = c(1, 2, 3, 4),
      f2 = c(4, 3, 2, 1),
      xrd.A = c(10, 20, NA, NA),
      xrd.B = c(1, 2, NA, NA)
    ),
    file.path(Datasets, "D.csv")
  )

  buildDataset(.path.input = Datasets, .path.train = Train,
               Y.PATTERN = "^(xrd.A)$", KEY = "SampleID",
               features = c("f2", "f1"), SET_COL = "set")

  DT <- data.table::fread(file.path(Train, "xrd.A_D_TRAIN.csv"))
  expect_identical(names(DT), c("f2", "f1", "y"))
})

test_that("buildDataset reports invalid exact features clearly", {
  Root <- file.path(tempdir(), paste0("ssel-buildDataset-missing-",
                                      Sys.getpid(), "-",
                                      sample.int(1e6, 1L)))
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  Datasets <- file.path(Root, "datasets")
  Train    <- file.path(Root, "train")
  dir.create(Datasets, recursive = TRUE)
  dir.create(Train, recursive = TRUE)

  data.table::fwrite(
    data.table::data.table(
      SampleID = paste0("S", 1:2),
      set = "train",
      f1 = c(1, 2),
      xrd.A = c(10, 20)
    ),
    file.path(Datasets, "D.csv")
  )

  expect_error(
    buildDataset(.path.input = Datasets, .path.train = Train,
                 Y.PATTERN = "^(xrd.A)$", KEY = "SampleID",
                 features = "missingFeature", SET_COL = "set"),
    "missing requested features"
  )
  expect_error(
    buildDataset(.path.input = Datasets, .path.train = Train,
                 Y.PATTERN = "^(xrd.A)$", KEY = "SampleID",
                 features = "xrd.A", SET_COL = "set"),
    "features include response columns"
  )
  expect_error(
    buildDataset(.path.input = Datasets, .path.train = Train,
                 Y.PATTERN = "^(xrd.A)$", KEY = "SampleID",
                 features = "SampleID", SET_COL = "set"),
    "features include identifier columns"
  )
})

test_that("buildDataset exact features preserve chain self-lag exclusion", {
  Root <- file.path(tempdir(), paste0("ssel-buildDataset-chain-",
                                      Sys.getpid(), "-",
                                      sample.int(1e6, 1L)))
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)

  Datasets <- file.path(Root, "datasets")
  Train    <- file.path(Root, "train")
  dir.create(Datasets, recursive = TRUE)
  dir.create(Train, recursive = TRUE)

  data.table::fwrite(
    data.table::data.table(
      SampleID = paste0("S", 1:3),
      set = "train",
      f1 = c(1, 2, 3),
      xrd.A = c(10, 20, 30),
      xrd.B = c(1, 2, 3),
      xrd.A.o = c(9, 19, 29),
      xrd.B.o = c(1, 2, 3)
    ),
    file.path(Datasets, "D.csv")
  )

  buildDataset(.path.input = Datasets, .path.train = Train,
               Y.PATTERN = "^(xrd.A|xrd.B)$", KEY = "SampleID",
               features = "f1",
               SET_COL = "set", ITER_SUFFIX = ".o")

  DT <- data.table::fread(file.path(Train, "xrd.A_D_TRAIN.csv"))
  expect_setequal(names(DT), c("f1", "xrd.B.o", "y"))
  expect_false("xrd.A.o" %in% names(DT))
})
