# Sourcing defines the library installer; it never installs a product or CLI.
installLibrary <- function(path, library, build = NULL, tarball = NULL, dependencies = character()) {
  source(file.path(path, "inst/package.R"), local = TRUE)
  Package <- .packageInfo(path)
  if (is.null(build) == is.null(tarball)) stop("Select exactly one build directory or tarball", call. = FALSE)
  Files <- c(dependencies, tarball)
  Artifacts <- lapply(Files, .readArtifact)
  if (anyDuplicated(vapply(Artifacts, `[[`, character(1L), "package"))) stop("Repeated package archive", call. = FALSE)
  if (!is.null(tarball)) {
    Artifact <- tail(Artifacts, 1L)[[1L]]
    if (!identical(Artifact$package, Package$package)) stop("Selected archive is not package ", Package$package, call. = FALSE)
  }
  if (length(dependencies) && any(vapply(head(Artifacts, length(dependencies)),
      function(x) identical(x$package, Package$package), logical(1L)))) stop("The product package cannot also be a dependency", call. = FALSE)
  installRequirements(path, library = library, packages = c("digest", if (!is.null(build)) "pkgbuild"), dependencies = FALSE)
  for (Artifact in head(Artifacts, length(dependencies))) installPackage(Artifact$file, library = library)
  if (!is.null(build)) {
    installRequirements(path, library = library, packages = character(), dependencies = NA)
    tarball <- buildPackage(path, output = build, documents = FALSE)
  }
  Artifact <- .readArtifact(tarball)
  Installed <- installPackage(Artifact$file, library = library)
  list(package = Installed$package, version = Installed$version,
       library = dirname(Installed$path), artifact = Artifact$sha256)
}
