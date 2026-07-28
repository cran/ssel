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

## ----call-shape, eval = FALSE-------------------------------------------------
# result <- semiSupervisedPipeline(
#   .path.datasets = datasets_dir,
#   .path.work = work_dir,
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
# result$history

