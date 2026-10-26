test_that("chain and delivery products do not depend on the session collation", {
  Options <- options(aR.quiet = TRUE)
  on.exit(options(Options), add = TRUE)
  Collation <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", Collation), add = TRUE)
  # A collation the host lacks returns "" with a warning and changes nothing.
  skip_if(!nzchar(suppressWarnings(Sys.setlocale("LC_COLLATE", "en_US.UTF-8"))),
          "en_US.UTF-8 collation is unavailable")

  # Mixed-case dataset, response and feature names order differently under
  # the C and en_US.UTF-8 collations.
  i <- seq_len(28L)
  DATA <- data.frame(
    SampleID = sprintf("%s%02d", ifelse(i <= 24L, "S", "P"), i),
    set = ifelse(i <= 24L, "train", "predict"),
    x = i,
    Z = (i * 7L) %% 5L,
    Zn = ifelse(i <= 24L, 1.5 * i + 2 * ((i * 7L) %% 5L) + 3 * sin(i), NA),
    al = ifelse(i <= 24L, 40 - 0.8 * i + 1.2 * ((i * 7L) %% 5L) + 2 * cos(0.7 * i), NA)
  )

  runChain <- function(collation) {
    Sys.setlocale("LC_COLLATE", collation)
    Root <- tempfile("ssel-collation-")
    on.exit(unlink(Root, recursive = TRUE), add = TRUE)
    Datasets <- file.path(Root, "datasets")
    Summary  <- file.path(Root, "summary")
    Export   <- file.path(Root, "export")
    dir.create(Datasets, recursive = TRUE)
    data.table::fwrite(DATA, file.path(Datasets, "alpha.csv"))
    DATA$Z <- DATA$Z + 1L
    data.table::fwrite(DATA, file.path(Datasets, "Beta.csv"))

    chainPipeline(
      .path.datasets = Datasets, .path.train = file.path(Root, "train"),
      .path.model = file.path(Root, "model"),
      .path.metrics = file.path(Root, "metrics"),
      .path.response = file.path(Root, "response"),
      .path.summary = Summary, .path.export = Export,
      .path.iter = file.path(Root, "iter"),
      key = "SampleID", responses = c("Zn", "al"),
      datasets = c("alpha", "Beta"), features = c("x", "Z"),
      methods = "lm", seed = 2026,
      maxIter = 1, tol = 0, patience = 2, sweep = "gauss-seidel"
    )
    postProcess(.path.summary = Summary, .path.export = Export,
                key = "SampleID", responses = c("Zn", "al"), location = "x",
                locationSources = file.path(Datasets, "alpha.csv"))

    FILES <- list.files(Root, pattern = "[.](csv|hash)$", recursive = TRUE)
    OUT <- lapply(file.path(Root, FILES), readLines)
    names(OUT) <- FILES
    OUT[sort(FILES, method = "radix")]
  }

  Reference <- runChain("C")
  Result <- runChain("en_US.UTF-8")
  FILES <- union(names(Reference), names(Result))
  FILES <- FILES[!vapply(FILES, function(x) {
    identical(Reference[[x]], Result[[x]])
  }, logical(1L))]
  expect_true(identical(Reference, Result), info = paste(
    "Files differing between C and en_US.UTF-8:", paste(FILES, collapse = "\n"),
    sep = "\n"
  ))
})
