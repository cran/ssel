## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = FALSE)
library(ssel)

## ----diagnose-----------------------------------------------------------------
RawValues <- c(
  "1 250",
  "2,50",
  "not recorded",
  NA
)
Bad <- which.nonnum(RawValues)

Diagnostic <- data.frame(
  position = Bad,
  value = RawValues[Bad]
)
Diagnostic

Normalized <- data.frame(
  raw = RawValues,
  numeric = toNumeric(RawValues)
)
Normalized

## ----assemble-----------------------------------------------------------------
Root <- tempfile("ssel-quickstart-")
InputDir <- file.path(
  Root,
  "assembled"
)
OutputDir <- file.path(
  Root,
  "splits"
)
dir.create(InputDir, recursive = TRUE)
dir.create(OutputDir)

SampleID <- c(
  "S1", "S2", "P1", "P2"
)
RowRole <- c(
  "train", "train",
  "predict", "predict"
)
ThicknessMm <- c(
  "1 250", "1 500",
  "1 750", "2 000"
)
MoisturePct <- c(
  "2,50", "3,00",
  "3,50", "4,00"
)
StrengthMpa <- c(10, 12, NA, NA)

Assembled <- data.frame(
  SampleID,
  set = RowRole,
  thickness_mm = ThicknessMm,
  moisture_pct = MoisturePct,
  strength_mpa = StrengthMpa
)

InputFile <- file.path(
  InputDir,
  "specimens.csv"
)

utils::write.csv(
  Assembled,
  InputFile,
  row.names = FALSE,
  na = ""
)

buildDataset(
  .path.input = InputDir,
  .path.train = OutputDir,
  Y.PATTERN = "^strength_mpa$",
  KEY = "SampleID",
  features = c(
    "thickness_mm",
    "moisture_pct"
  ),
  SET_COL = "set"
)

TestFile <- list.files(
  OutputDir,
  pattern = "_TEST[.]csv$"
)
TrainIDFile <- list.files(
  OutputDir,
  pattern = "_TRAIN_ids[.]csv$"
)
TrainFile <- list.files(
  OutputDir,
  pattern = "_TRAIN[.]csv$"
)

Products <- data.frame(
  product = c(
    "TEST",
    "TRAIN-ID",
    "TRAIN"
  ),
  filename = c(
    TestFile,
    TrainIDFile,
    TrainFile
  )
)

knitr::kable(
  Products,
  caption = "Files created"
)

## ----inspect------------------------------------------------------------------
TrainPath <- file.path(
  OutputDir,
  TrainFile
)
TestPath <- file.path(
  OutputDir,
  TestFile
)
TrainIDPath <- file.path(
  OutputDir,
  TrainIDFile
)

Train <- utils::read.csv(
  TrainPath
)
Test <- utils::read.csv(
  TestPath
)
TrainIDs <- utils::read.csv(
  TrainIDPath
)

knitr::kable(
  Train,
  caption = "TRAIN"
)
knitr::kable(
  Test,
  caption = "TEST"
)
knitr::kable(
  TrainIDs,
  caption = "TRAIN-ID"
)

## ----cleanup, include = FALSE-------------------------------------------------
unlink(Root, recursive = TRUE)

