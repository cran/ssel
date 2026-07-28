# Unified logging helpers (internal).
#
# Severity
#   .INFO    - normal progress (blue)
#   .WARN    - recoverable issue (yellow)
#   .ERROR   - failed operation (red)
#   .OK      - success/checkpoint (green)
#
# Structure
#   .HEAD    - top-level section header, bold-bordered "=== ... ==="
#   .SUB     - sub-section marker, "> ..."
#   .KV      - key/value alignment helper
#
# Verbosity
#   .DEBUG   - low-signal detail; emitted only when
#              options(aR.verbose = TRUE).
#
# Quiet mode silences progress and structure, but never warnings or errors.

.aR.verbose <- function() isTRUE(getOption("aR.verbose", default = FALSE))
.aR.quiet   <- function() isTRUE(getOption("aR.quiet",   default = FALSE))

.aR.message <- function(prefix, ...) {
  message(prefix, " ", paste(...), domain = NA)
}

.INFO  <- function(...) if (!.aR.quiet()) .aR.message(crayon::blue("[INFO]"), ...)
.WARN  <- function(...) warning(paste(...), call. = FALSE, domain = NA)
.ERROR <- function(...) stop(paste(...), call. = FALSE, domain = NA)
.OK    <- function(...) if (!.aR.quiet()) .aR.message(crayon::green("[OK]"), ...)
.DEBUG <- function(...) {
  if (!.aR.quiet() && .aR.verbose()) .aR.message(crayon::silver("[DBG]"), ...)
}

.HEAD <- function(...) {
  if (!.aR.quiet()) {
    message(crayon::bold(sprintf("\n=== %s ===", paste(...))), domain = NA)
  }
}

.SUB <- function(...) {
  if (!.aR.quiet()) message("  ", crayon::bold("> "), paste(...), domain = NA)
}

.KV <- function(k, v) {
  if (!.aR.quiet()) {
    message(sprintf("  %-14s %s", paste0(k, ":"), v), domain = NA)
  }
}
