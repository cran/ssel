## ----prepare-cli-example, eval = FALSE----------------------------------------
# Root <- tempfile("ssel-cli-example-")
# dir.create(file.path(Root, "datasets"), recursive = TRUE)
# setwd(Root)
# Data <- data.frame(
#   SampleID = c(paste0("S", 1:18), "P1", "P2"),
#   set = c(rep("train", 18), "predict", "predict"),
#   x = 1:20, z = rep(c(0, 1), 10),
#   targetA = c(1, 2.4, 1.8, 3.7, 2.9, 5.1, 4, 6.2, 5.4,
#               7, 6.1, 8.3, 7.2, 9.1, 8, 10.5, 9.4, 11.2, NA, NA),
#   targetB = c(2.2, 1.3, 3.6, 2.7, 4.1, 3.8, 5.7, 4.6, 6.5,
#               5.9, 7.8, 6.4, 8.6, 7.5, 9.9, 8.7, 10.4, 9.6, NA, NA)
# )
# utils::write.csv(Data, "datasets/demo.csv", row.names = FALSE, na = "")
# Spec <- list(
#   .path.datasets = "datasets", .path.train = "train", .path.model = "model",
#   .path.metrics = "metrics", .path.response = "response",
#   .path.summary = "summary", .path.export = "export",
#   key = "SampleID", responses = c("targetA", "targetB"), datasets = "demo",
#   methods = "lm", seed = 2026, features = c("x", "z")
# )
# jsonlite::write_json(Spec, "model.json", auto_unbox = TRUE, pretty = TRUE)
# Spec$.path.iter <- "iter"
# Spec$maxIter <- 1
# Spec$tol <- 0
# Spec$patience <- 2
# Spec$sweep <- "jacobi"
# jsonlite::write_json(Spec, "chain.json", auto_unbox = TRUE, pretty = TRUE)
# jsonlite::write_json(list(
#   .path.summary = "summary", .path.export = "export", key = "SampleID",
#   responses = c("targetA", "targetB"), location = c("x", "z"),
#   locationSources = "datasets/demo.csv"
# ), "post.json", auto_unbox = TRUE, pretty = TRUE)
# jsonlite::write_json(list(x = c("1,25", "1 234", "x")),
#                      "numeric.json", auto_unbox = TRUE)
# saveRDS(list(
#   .path.datasets = "datasets", .path.work = "semi", key = "SampleID",
#   responses = c("targetA", "targetB"), datasets = "demo",
#   methods = c("lm", "glm"), seed = 2026, tau = 0.1, maxRounds = 1,
#   epsRevert = Inf, promotionCap = 0.5
# ), "semi.rds")
# Root

## ----read-cli-results, eval = FALSE-------------------------------------------
# readRDS("numeric-result.rds")
# readRDS("semi-result.rds")$history
# jsonlite::read_json("semi-portable/result.json")$result$kind  # "list"
# utils::read.csv("semi-portable/history.csv")
# utils::read.csv("export/predictions.csv")
# utils::read.csv("semi/round/export/Yo.csv")

