local({
  source("inst/package.R", local = TRUE)
  Args <- commandArgs(TRUE)
  if (length(Args) != 2L) stop("Required arguments: TARBALL OUTPUT_DIR", call. = FALSE)
  checkPackage(file = Args[1L], output = Args[2L])
})
