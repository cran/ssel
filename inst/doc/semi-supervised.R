## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(ssel)

## ----range-ratio--------------------------------------------------------------
base_predictions <- c(lm = 10, glm = 11)
ensemble_last <- 10.5
score <- diff(range(base_predictions)) / max(1, abs(ensemble_last))
c(score = score, promotes_at_tau_0.1 = score < 0.1)

## ----key-budget---------------------------------------------------------------
U <- c("P1", "P2")
promotion_cap <- 0.5
accepted_keys <- character()
floor(promotion_cap * length(U)) - length(unique(accepted_keys))

## ----semi-example, eval = FALSE-----------------------------------------------
# root <- tempfile("ssel-semi-")
# datasets_dir <- file.path(root, "datasets")
# dir.create(datasets_dir, recursive = TRUE)
# 
# n_train <- 30L
# assembled <- data.frame(
#   SampleID = c(paste0("S", seq_len(n_train)), "P1", "P2"),
#   set = c(rep("train", n_train), "predict", "predict"),
#   x = seq_len(n_train + 2L),
#   z = rep(c(0, 1), length.out = n_train + 2L),
#   target = c(
#     1.5 * seq_len(n_train) + rep(c(-0.4, 0.2, 0.6), 10),
#     NA, NA
#   )
# )
# utils::write.csv(
#   assembled, file.path(datasets_dir, "demo.csv"),
#   row.names = FALSE, na = ""
# )
# 
# old_quiet <- getOption("aR.quiet")
# options(aR.quiet = TRUE)
# result <- semiSupervisedPipeline(
#   .path.datasets = datasets_dir,
#   .path.work = file.path(root, "work"),
#   key = "SampleID",
#   responses = "target",
#   allResponses = "target",
#   datasets = "demo",
#   methods = c("lm", "glm"),
#   seed = 2026,
#   tau = 0.1,
#   maxRounds = 1,
#   epsRevert = Inf,
#   promotionCap = 0.5
# )
# options(aR.quiet = old_quiet)
# 
# result$history
# result$promoted
# c(baseline = result$baselineR2, historical_best = result$finalR2)
# # The one-round policy promotes at most one of the two original keys.
# # iterStar is NA because no chain iteration directory was supplied.
# 
# unlink(root, recursive = TRUE)

