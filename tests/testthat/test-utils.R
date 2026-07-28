test_that("toNumeric handles common decimal and thousands formats", {
  expect_equal(toNumeric(c("1,25", "1 234", "1.234.567")), c(1.25, 1234, 1234567))
})

test_that("which.nonnum returns non-coercible non-missing positions", {
  expect_equal(which.nonnum(c("1", "x", NA, "2.5")), 2L)
})

test_that("activeByImportance thresholds chain responses", {
  DT <- data.table::data.table(
    feature = c("a.o", "a.o", "b.o"),
    normImportance = c(1, 3, 0.1)
  )
  expect_equal(activeByImportance(DT, suffix = ".o", thresh = 1), "a")
})

test_that("activeByShadow requires majority above shadow", {
  DT <- data.table::data.table(
    feature = c("a.o", "a.o", "b.o", "b.o"),
    normImportance = c(2, 3, 1, 1),
    shadowImportance = c(1, 2, 2, 2)
  )
  expect_equal(activeByShadow(DT, suffix = ".o"), "a")
})
