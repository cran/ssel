makeFrame <- function() {
  set.seed(1)
  DT <- data.frame(a = rnorm(60), b = rnorm(60), c = rnorm(60), y = rnorm(60))
  DT$a[1] <- 50
  DT$b[2] <- 50
  DT$c[3] <- 50
  DT
}

test_that("the envelopes apply together, not one at a time", {
  DT <- suppressWarnings(removeOutliersIQR(makeFrame(), n = 3))

  expect_s3_class(DT, "data.table")
  expect_false(any(DT$a == 50))
  expect_false(any(DT$b == 50))
  expect_false(any(DT$c == 50))
  expect_identical(nrow(DT), 57L)
})

test_that("a wide envelope keeps every row of well-behaved data", {
  set.seed(2)
  DT <- as.data.frame(matrix(rnorm(600), 60, 10))
  names(DT) <- c(paste0("v", 1:9), "y")

  for (n in c(1L, 3L, 9L)) {
    expect_identical(nrow(suppressWarnings(removeOutliersIQR(DT, n = n))), 60L)
  }
})

test_that("data whose ranked predictors are all non-numeric comes back whole", {
  set.seed(3)
  DT <- data.frame(
    f = factor(sample(letters[1:3], 60, TRUE)),
    g = factor(sample(letters[4:6], 60, TRUE)),
    y = rnorm(60)
  )

  expect_identical(nrow(suppressWarnings(removeOutliersIQR(DT, n = 2))), 60L)
})

test_that("a wider multiplier keeps at least as many rows", {
  DT <- makeFrame()
  Narrow <- nrow(suppressWarnings(removeOutliersIQR(DT, n = 3, q = 1)))
  Wide <- nrow(suppressWarnings(removeOutliersIQR(DT, n = 3, q = 6)))

  expect_gte(Wide, Narrow)
})
