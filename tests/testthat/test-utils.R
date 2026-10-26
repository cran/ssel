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

test_that("portable order folds ASCII case and sorts separators first", {
  expect_identical(
    .portableSort(c("Zn", "al", "ppaL_QDS", "ppa_QDS", "x.o", "x", "B2", "b10")),
    c("al", "b10", "B2", "ppa_QDS", "ppaL_QDS", "x", "x.o", "Zn")
  )
  expect_identical(.portableOrder(c("b", "A", "a_1", "a")), c(2L, 4L, 3L, 1L))
  expect_identical(.portableSort(character(0)), character(0))
})

test_that("portable order ignores the session collation", {
  Names <- c("Zn", "al", "ppaL_QDS", "ppa_QDS", "Beta", "alpha")
  Collation <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", Collation), add = TRUE)

  Sys.setlocale("LC_COLLATE", "C")
  OUT <- .portableSort(Names)
  # A collation the host lacks returns "" with a warning and changes nothing.
  skip_if(!nzchar(suppressWarnings(Sys.setlocale("LC_COLLATE", "en_US.UTF-8"))),
          "en_US.UTF-8 collation is unavailable")
  expect_identical(.portableSort(Names), OUT)
})

test_that(".listFiles returns matches in portable order", {
  Root <- tempfile("ssel-files-")
  dir.create(Root)
  on.exit(unlink(Root, recursive = TRUE), add = TRUE)
  file.create(file.path(Root, c("Zn_Beta_TRAIN.csv", "al_alpha_TRAIN.csv",
                                "ppaL_QDS_TRAIN.csv", "ppa_QDS_TRAIN.csv",
                                "ppa_QDS_TEST.csv")))

  expect_identical(
    .listFiles(Root, pattern = "_TRAIN\\.csv$"),
    c("al_alpha_TRAIN.csv", "ppa_QDS_TRAIN.csv", "ppaL_QDS_TRAIN.csv",
      "Zn_Beta_TRAIN.csv")
  )
})
