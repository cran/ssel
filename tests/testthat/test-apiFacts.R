test_that("installed API facts match the public surface", {
  File <- system.file("api", "operations.json", package = "ssel")
  expect_true(nzchar(File))
  Facts <- jsonlite::read_json(File, simplifyVector = TRUE)

  # Operations are exactly the package exports.
  expect_identical(sort(Facts$operations, method = "radix"),
                   sort(getNamespaceExports("ssel"), method = "radix"))
  expect_true(all(unlist(Facts$aliases) %in% Facts$operations))
  expect_true(all(Facts$fileOrientedOperations %in% Facts$operations))

  # Every listed argument name belongs to some exported operation.
  Formals <- unique(unlist(lapply(Facts$operations, function(Op)
    names(formals(getExportedValue("ssel", Op))))))
  expect_true(all(Facts$vectorArguments %in% Formals))
  expect_true(all(Facts$tableArguments %in% Formals))
  expect_true(all(Facts$historyArgument %in% Formals))

  # Every formal with a vector default is listed as a vector argument.
  for (Op in Facts$operations) {
    Defaults <- formals(getExportedValue("ssel", Op))
    Missing <- vapply(Defaults, identical, logical(1L), quote(expr = ))
    for (Arg in names(Defaults)[!Missing]) {
      D <- Defaults[[Arg]]
      Vectorial <- (is.call(D) && identical(as.character(D[[1L]]), "c")) ||
        { V <- tryCatch(eval(D), error = function(e) NULL)
          is.atomic(V) && length(V) != 1L }
      if (Vectorial) {
        expect_true(Arg %in% Facts$vectorArguments,
                    label = paste(Op, "argument", Arg, "has a vector default"))
      }
    }
  }
})
