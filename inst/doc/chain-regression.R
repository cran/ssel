## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(ssel)

## ----fixed-gate---------------------------------------------------------------
importance <- data.table::data.table(
  feature = c("beta.o", "alpha.o", "beta.o", "alpha.o", "gamma.o"),
  normImportance = c(1, 2, 3, NA, NA)
)
activeByImportance(importance, suffix = ".o", thresh = 2)

## ----shadow-gate--------------------------------------------------------------
importance <- data.table::data.table(
  feature = c("alpha.o", "alpha.o", "alpha.o",
              "beta.o", "beta.o", "gamma.o", "gamma.o"),
  normImportance = c(2, 3, 0, 2, 0, 2, 2),
  shadowImportance = c(1, 1, 1, 1, 1, 1, NA)
)
activeByShadow(importance, suffix = ".o")

