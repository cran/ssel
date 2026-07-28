## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(ssel)

## ----residual-offset-example--------------------------------------------------
root <- tempfile("ssel-offsets-")
model_dir <- file.path(root, "model")
train_dir <- file.path(root, "train")
summary_dir <- file.path(root, "summary")
dir.create(model_dir, recursive = TRUE)
dir.create(train_dir)

data.table::fwrite(
  data.table::data.table(y = c(1, 3)),
  file.path(train_dir, "target_demo_TRAIN.csv")
)
data.table::fwrite(
  data.table::data.table(SampleID = c("S1", "S2")),
  file.path(train_dir, "target_demo_TRAIN_ids.csv")
)
fit <- list(
  pred = data.frame(
    tune = 1L, pred = c(0, 2), obs = c(1, 3), rowIndex = 1:2
  ),
  bestTune = data.frame(tune = 1L)
)
saveRDS(fit, file.path(model_dir, "lm_target_demo.Rds"))

offsets <- auditQuantiles(
  .path.model = model_dir,
  .path.train = train_dir,
  .path.summary = summary_dir,
  .response_pattern = "target",
  verbose = FALSE
)
offsets$quantiles
stopifnot(all(offsets$quantiles[, c("q_50", "q_90", "q_95")] == 1))

unlink(root, recursive = TRUE)

