# nolint start
# utils.R - cross-cutting helpers (internal).

# %||% - null-coalesce. Returns b if a is NULL, otherwise a.
# Used at the manifest -> R config boundary (e.g. optional response bounds).
`%||%` <- function(a, b) if (is.null(a)) b else a

# toNumeric: normalize number formatting in character vectors
# Handles: space thousands, dot thousands, comma thousands, comma decimal
#' @export
toNumeric <- function(x) {
  x <- gsub(" ", "", x)
  x <- ifelse(grepl("([.][0-9]+){2,}", x), gsub("[.]", "", x), x)
  x <- ifelse(grepl("(,[0-9]+){2,}",   x), gsub(",",  "", x), x)
  x <- ifelse(grepl("^-?[0-9]+,[0-9]+$", x), gsub(",", ".", x), x)
  suppressWarnings(as.numeric(x))
}
# nolint end
