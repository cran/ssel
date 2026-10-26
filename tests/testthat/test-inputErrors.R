# Input errors must stop at the public boundary naming path, argument and
# cause.
# Baselines being replaced: 'x' must be atomic, invalid subscript type
# 'list', and silent exit 0 for missing or empty input directories.

test_that("buildDataset names a missing input directory", {
  expect_error(
    buildDataset(file.path(tempdir(), "no-such-dir"), tempfile(),
                 Y.PATTERN = "^target", KEY = "SampleID", SET_COL = "set"),
    "buildDataset: .path.input is not an existing directory", fixed = TRUE)
})

test_that("buildDataset names a directory without CSV files", {
  Empty <- tempfile()
  dir.create(Empty)
  expect_error(
    buildDataset(Empty, tempfile(), Y.PATTERN = "^target", KEY = "SampleID", SET_COL = "set"),
    "buildDataset: no .csv files found in .path.input", fixed = TRUE)
})

test_that("buildDataset refuses '_' in dataset names", {
  Src <- tempfile()
  dir.create(Src)
  utils::write.csv(data.frame(SampleID = "S1", set = "train", x = 1, target = 2),
                   file.path(Src, "my_data.csv"), row.names = FALSE)
  expect_error(
    buildDataset(Src, tempfile(), Y.PATTERN = "^target", KEY = "SampleID", SET_COL = "set"),
    "buildDataset: dataset names must not contain '_'", fixed = TRUE)
})

test_that("buildDataset refuses '_' in response names", {
  Src <- tempfile()
  dir.create(Src)
  utils::write.csv(data.frame(SampleID = "S1", set = "train", x = 1, target_a = 2),
                   file.path(Src, "ok.csv"), row.names = FALSE)
  expect_error(
    buildDataset(Src, tempfile(), Y.PATTERN = "^target", KEY = "SampleID", SET_COL = "set"),
    "buildDataset: response names must not contain '_'", fixed = TRUE)
})

test_that("trainModel names a missing split directory", {
  expect_error(
    trainModel(file.path(tempdir(), "no-such-dir"), tempfile(), tempfile(), tempfile(),
               METHODS = "lm"),
    "trainModel: .path.train is not an existing directory", fixed = TRUE)
})

test_that("trainModel names an empty split directory", {
  Empty <- tempfile()
  dir.create(Empty)
  expect_error(
    trainModel(Empty, tempfile(), tempfile(), tempfile(), METHODS = "lm"),
    "trainModel: no *_TRAIN.csv splits found in .path.train", fixed = TRUE)
})

test_that("trainModel names responses without a TRAIN split", {
  Dir <- tempfile()
  dir.create(Dir)
  utils::write.csv(data.frame(x = 1, y = 2), file.path(Dir, "Zn_AR_TRAIN.csv"),
                   row.names = FALSE)
  expect_error(
    trainModel(Dir, tempfile(), tempfile(), tempfile(), METHODS = "lm", .responses = "Cu"),
    "trainModel: no TRAIN split matches .responses", fixed = TRUE)
})

test_that("predictModel names a missing split directory", {
  expect_error(
    predictModel(file.path(tempdir(), "no-such-dir"), tempfile(), tempfile(), tempfile(),
                 tempfile(), tempfile(), METHODS = "lm"),
    "predictModel: .path.train is not an existing directory", fixed = TRUE)
})

test_that("predictModel names responses without a TRAIN split", {
  Dir <- tempfile()
  dir.create(Dir)
  utils::write.csv(data.frame(x = 1, y = 2), file.path(Dir, "Zn_AR_TRAIN.csv"),
                   row.names = FALSE)
  expect_error(
    predictModel(Dir, tempfile(), tempfile(), tempfile(), tempfile(), tempfile(),
                 METHODS = "lm", .responses = "Cu"),
    "predictModel: no TRAIN split matches .responses", fixed = TRUE)
})
