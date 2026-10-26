# R-hub owns these environment variables and the platform's repository options.
source("inst/package.R", local = TRUE)
Library <- Sys.getenv("R_LIBS_USER")
if (!nzchar(Library)) stop("R-hub must select R_LIBS_USER", call. = FALSE)
Dependencies <- Sys.getenv("RHUB_ACTIONS_DEPS_DEPENDENCIES", unset = '"all"')
Dependencies <- eval(str2lang(Dependencies))
if (identical(Dependencies, "all")) Dependencies <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
if (identical(Dependencies, "strong")) Dependencies <- NA
Install <- identical(Sys.getenv("RHUB_ACTIONS_INSTALL_LOCAL_PACKAGE"), "true")
Tools <- c("rcmdcheck", "sessioninfo", if (Install) c("pkgbuild", "digest"))
installRequirements(path = ".", library = Library, packages = Tools, dependencies = Dependencies)
.libPaths(c(Library, .libPaths()))
if (Install) {
  Artifact <- buildPackage(path = ".", output = tempfile("rhub-build-"), documents = FALSE)
  installPackage(file = Artifact, library = Library)
}
