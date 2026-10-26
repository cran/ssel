# nolint start
# utils.R - cross-cutting helpers (internal).

# %||% - null-coalesce. Returns b if a is NULL, otherwise a.
# Used at the manifest -> R config boundary (e.g. optional response bounds).
`%||%` <- function(a, b) if (is.null(a)) b else a

# Portable name order: the single owner of every base-R text order that reaches
# a product, a hash or the RNG stream (data.table orders its keys in the C
# locale on its own). sort(), order() and list.files() follow the session
# collation, which differs between environments. Here ASCII case is folded, "_"
# and "." sort before any other character (a name precedes its own extensions)
# and exact ties fall back to byte order; method = "radix" collates as
# LC_COLLATE=C whatever the session locale is.
.portableOrder <- function(x) {
  order(chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ_.", "abcdefghijklmnopqrstuvwxyz\001\001", x),
        x, method = "radix")
}

.portableSort <- function(x) x[.portableOrder(x)]

.listFiles <- function(...) .portableSort(list.files(...))

# trainModel/predictModel resolve the <response>_<dataset>_TRAIN.csv splits
# written by buildDataset. An empty directory or a .responses filter that
# matches nothing used to die downstream as 'x' must be atomic or invalid
# subscript type 'list'; stop at the boundary naming path, argument and
# cause. '_' is the field separator, so names containing it cannot
# round-trip (buildDataset refuses to write them).
.splitFiles <- function(.path.train, .responses, Caller) {
  if (!dir.exists(.path.train)) {
    stop(Caller, ": .path.train is not an existing directory: ", .path.train,
         call. = FALSE)
  }
  FILES <- .listFiles(.path.train, pattern = "_TRAIN\\.csv$", full.names = FALSE)
  FILES <- sub("\\.csv$", "", FILES)
  if (!length(FILES)) {
    stop(Caller, ": no *_TRAIN.csv splits found in .path.train: ", .path.train,
         call. = FALSE)
  }
  if (!is.null(.responses)) {
    FILES <- FILES[sapply(FILES, function(.x) strsplit(.x, "_")[[1]][1]) %in% .responses]
    if (!length(FILES)) {
      stop(Caller, ": no TRAIN split matches .responses (",
           paste(.responses, collapse = ", "), ") in .path.train: ", .path.train,
           call. = FALSE)
    }
  }
  FILES
}

# Canonical absolute path even when the path does not exist yet: the deepest
# existing ancestor is normalized (symlinks resolved) and the missing tail is
# appended textually.
.canonicalPath <- function(path) {
  Tail <- character()
  while (!file.exists(path) && !dir.exists(path)) {
    Parent <- dirname(path)
    if (Parent == path) break
    Tail <- c(basename(path), Tail)
    path <- Parent
  }
  Base <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (length(Tail)) Base <- do.call(file.path, c(list(Base), as.list(Tail)))
  Base
}

# Wipe destinations are user-declared paths: a mistake there deletes inputs or
# the caller's project. Refuse when a destination coincides with, contains or
# is inside the datasets directory, when it contains the working directory, or
# when two declared paths coincide.
.guardWipe <- function(paths, datasets) {
  Paths  <- vapply(paths, .canonicalPath, character(1L))
  Source <- .canonicalPath(datasets)
  Work   <- .canonicalPath(getwd())
  Inside <- function(path, ancestor) {
    ancestor == "/" || startsWith(path, paste0(ancestor, "/"))
  }
  for (i in seq_along(Paths)) {
    if (Paths[[i]] == Source) {
      stop("Refusing to wipe ", paths[[i]],
           ": it is the datasets directory (", datasets, ")", call. = FALSE)
    }
    if (Inside(Paths[[i]], Source)) {
      stop("Refusing to wipe ", paths[[i]],
           ": it is inside the datasets directory (", datasets, ")", call. = FALSE)
    }
    if (Inside(Source, Paths[[i]])) {
      stop("Refusing to wipe ", paths[[i]],
           ": it contains the datasets directory (", datasets, ")", call. = FALSE)
    }
    if (Paths[[i]] == Work || Inside(Work, Paths[[i]])) {
      stop("Refusing to wipe ", paths[[i]],
           ": it contains the working directory (", Work, ")", call. = FALSE)
    }
  }
  Dup <- anyDuplicated(Paths)
  if (Dup) {
    stop("Refusing to wipe: declared paths coincide: ", paths[[match(Paths[[Dup]], Paths)]],
         " and ", paths[[Dup]], call. = FALSE)
  }
  invisible(NULL)
}

# Response bounds reach pmax()/pmin() clamps across the pipelines. A bound with
# the wrong shape (a string from a JSON spec, a vector, NA) used to clamp
# silently or fail far from its cause. Validate at the public boundary: a
# single numeric value, not NA; Inf and -Inf are allowed (they are the
# defaults). Bound order and finiteness remain the caller's contract.
.checkBound <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
    stop(name, " must be a single numeric value, not NA; Inf and -Inf are allowed",
         call. = FALSE)
  }
  invisible(value)
}

# The audits walk a model directory and drop what they cannot use. Two of
# those drops mean opposite things to the caller, and reporting them alike
# would make every ordinary pipeline run noisy: a filename that does not match
# the response pattern is the caller's own filter (a chain iteration leaves
# models of responses it no longer audits), while a file that matched and
# still could not be read is a hole in the audit the caller asked for. Only
# the second is reported, plus the case where nothing at all was audited --
# otherwise an empty audit is indistinguishable from a clean one.
.reportAudit <- function(caller, modelDir, pattern, scanned, matched, failed,
                         cells) {
  # .WARN() joins its arguments with spaces, so each message is composed here
  # and handed over whole.
  if (length(failed)) {
    .WARN(paste0(
      caller, ": ", length(failed), " of ", matched,
      " model file(s) matching the response pattern could not be audited: ",
      paste0(names(failed), " (", unlist(failed, use.names = FALSE), ")",
             collapse = "; ")
    ))
  }
  if (cells == 0L) {
    .WARN(paste0(
      caller, ": nothing was audited from ", scanned, " model file(s) in ",
      modelDir, ": ",
      if (matched == 0L) {
        paste0("no filename matched the response pattern '", pattern,
               "', which is matched against the whole response in ",
               "<method>_<response>_<dataset>.Rds")
      } else {
        paste0("all ", matched, " matching file(s) were skipped")
      },
      "."
    ))
  }
  invisible(NULL)
}

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
