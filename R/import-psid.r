# Internal helpers for importing locally downloaded PSID files.

#' Replace non-ASCII and NUL bytes with spaces in a SAS file
#'
#' Reads a SAS file as raw bytes and writes it out with every byte above 127
#' and every NUL byte replaced by a space (0x20). Some downloaded PSID .sas
#' files contain stray non-ASCII bytes that would otherwise break
#' `SAScii::read.SAScii()`.
#'
#' @param path Path to the input .sas file.
#' @param out Path to write the cleaned file to. Defaults to `path`, i.e.
#'   rewriting the file in place.
#' @return Invisibly, `out`.
#' @noRd
clean_sas <- function(path, out = path) {
  bytes <- readBin(path, what = "raw", n = file.size(path))
  bytes[as.integer(bytes) > 127] <- as.raw(0x20)
  bytes[bytes == as.raw(0x00)] <- as.raw(0x20)
  writeBin(bytes, out)
  invisible(out)
}

#' Does a SAS file contain an INPUT block?
#'
#' Reads a .sas file in chunks and returns TRUE as soon as a line matching
#' `^\s*INPUT\b` (case-insensitive, byte-wise) is found.
#'
#' @param path Path to the .sas file.
#' @return A logical scalar.
#' @noRd
has_input_block <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)

  repeat {
    lines <- readLines(con, n = 1000L, warn = FALSE, skipNul = TRUE)
    if (length(lines) == 0L) {
      return(FALSE)
    }
    if (any(grepl("^\\s*INPUT\\b", lines, ignore.case = TRUE,
                  useBytes = TRUE))) {
      return(TRUE)
    }
  }
}

#' Pick the .txt/.sas data pair from a vector of file paths
#'
#' Selects one .txt data file and one .sas file (containing an INPUT block)
#' from the supplied paths, ignoring directories, `__MACOSX` paths and
#' dot-prefixed base names. Matching base names are preferred; otherwise the
#' largest .txt and the first valid .sas are used, with a warning.
#'
#' @param files Character vector of file paths.
#' @return A list with elements `txt` and `sas`.
#' @noRd
pick_pair <- function(files) {
  files <- files[!dir.exists(files)]
  base <- basename(files)

  drop <- grepl("__MACOSX", files) | grepl("^\\.", base)
  files <- files[!drop]
  base <- base[!drop]

  b <- tolower(base)

  txt <- files[grepl("\\.txt$", b) &
                 !grepl("_vdm|format|readme|codebook|errata|intro|doc", b)]
  sas <- files[grepl("\\.sas$", b) & !grepl("format", b)]

  if (length(sas) > 0) {
    sas <- sas[vapply(sas, has_input_block, logical(1))]
  }

  if (length(txt) == 0 || length(sas) == 0) {
    stop("No valid .txt/.sas pair found. Files found: ",
         paste(basename(files), collapse = ", "), call. = FALSE)
  }

  tb <- tolower(sub("\\.[^.]*$", "", basename(txt)))
  sb <- tolower(sub("\\.[^.]*$", "", basename(sas)))
  hit <- which(sb %in% tb)

  if (length(hit) > 0) {
    s <- sas[hit[1]]
    t <- txt[match(sb[hit[1]], tb)]
  } else {
    futile.logger::flog.warn(
      "no matching .txt/.sas base names; using largest .txt and first valid .sas"
    )
    s <- sas[1]
    t <- txt[which.max(file.size(txt))]
  }

  list(txt = t, sas = s)
}

#' Process one PSID job into a .rda or .rds file
#'
#' Reads a job (one zip, one directory, or several loose files), picks the
#' data/SAS pair, imports it with `SAScii::read.SAScii()`, and saves the
#' resulting data frame under the name `x` to `out`. The output is written to
#' `out.part` first and then renamed so an interrupted run cannot leave a
#' corrupt file behind.
#'
#' @param files One zip path, one directory path, or a character vector of
#'   loose file paths.
#' @param out Path to write the output file to.
#' @param format Either `"rda"` or `"rds"`.
#' @return Invisibly, `out`.
#' @noRd
process_one <- function(files, out, format = c("rda", "rds")) {
  format <- match.arg(format)

  tmp <- tempfile("psid_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  if (length(files) == 1L && dir.exists(files)) {
    search <- list.files(files, recursive = TRUE, full.names = TRUE)
  } else if (length(files) == 1L && grepl("\\.zip$", tolower(files))) {
    entries <- utils::unzip(files, list = TRUE)$Name
    if (length(entries) > 0L) {
      unsafe <- vapply(entries, function(entry) {
        grepl("^[/\\\\]", entry) ||
          grepl("^[A-Za-z]:", entry) ||
          any(strsplit(entry, "[/\\\\]", perl = TRUE)[[1]] == "..")
      }, logical(1))

      if (any(unsafe)) {
        stop("zip contains an unsafe entry: ",
             entries[which(unsafe)[1]], call. = FALSE)
      }
    }

    extracted <- utils::unzip(files, exdir = tmp)
    if (length(extracted) == 0L) {
      stop("no files extracted from ", basename(files), call. = FALSE)
    }

    tmp_norm <- normalizePath(tmp, mustWork = TRUE)
    extracted_norm <- normalizePath(extracted, mustWork = TRUE)
    outside <- !startsWith(extracted_norm,
                           paste0(tmp_norm, .Platform$file.sep))
    if (any(outside)) {
      stop("zip contains entries that extract outside the target directory",
           call. = FALSE)
    }

    search <- list.files(tmp, recursive = TRUE, full.names = TRUE)
  } else {
    search <- files
  }

  pair <- pick_pair(search)
  futile.logger::flog.info(
    "importing data file %s with sas file %s",
    basename(pair$txt), basename(pair$sas)
  )

  cleaned_sas <- tempfile(fileext = ".sas")
  on.exit(unlink(cleaned_sas), add = TRUE)
  clean_sas(pair$sas, out = cleaned_sas)

  x <- SAScii::read.SAScii(pair$txt, cleaned_sas)
  if (!is.data.frame(x) || nrow(x) == 0L) {
    stop("SAScii::read.SAScii did not return a non-empty data.frame",
         call. = FALSE)
  }

  part <- paste0(out, ".part")
  on.exit({
    if (file.exists(part)) {
      unlink(part)
    }
  }, add = TRUE)

  if (format == "rda") {
    save(x, file = part)
  } else {
    saveRDS(x, part)
  }

  if (!file.rename(part, out)) {
    stop("failed to rename temporary output file to ", out, call. = FALSE)
  }

  invisible(out)
}

#' Find import jobs in a directory
#'
#' Lists the top-level entries of `inputdir` whose lower-cased base name
#' matches `^<prefix>([0-9]+)` and groups them by the captured number. For
#' each number, the preferred kind is kept: a zip over a directory over loose
#' files. A warning is logged when several kinds exist for the same number.
#'
#' @param inputdir Directory to search.
#' @param prefix Job prefix (e.g. `"fam"` or `"ind"`).
#' @return A list of `list(num, files)` entries.
#' @noRd
psid_find_jobs <- function(inputdir, prefix) {
  entries <- list.files(inputdir, full.names = TRUE, include.dirs = TRUE)
  base <- tolower(basename(entries))

  matched <- grepl(paste0("^", prefix, "([0-9]+)"), base, ignore.case = TRUE)

  nums <- rep(NA_character_, length(base))
  num_pattern <- paste0("^", prefix, "([0-9]+).*$")
  nums[matched] <- sub(num_pattern, "\\1", base[matched], ignore.case = TRUE)

  entries <- entries[matched]
  nums <- nums[matched]

  if (length(entries) == 0L) {
    return(list())
  }

  lapply(unique(nums), function(num) {
    group <- entries[nums == num]

    is_dir <- dir.exists(group)
    is_zip <- !is_dir & grepl("\\.zip$", tolower(group))
    is_loose <- !is_dir & !is_zip

    if (sum(any(is_dir), any(is_zip), any(is_loose)) > 1L) {
      futile.logger::flog.warn(
        "multiple job kinds for %s%s; using zip over directory over loose files",
        prefix, num
      )
    }

    if (any(is_zip)) {
      files <- group[which(is_zip)[1]]
    } else if (any(is_dir)) {
      files <- group[which(is_dir)[1]]
    } else {
      files <- group[is_loose]
    }

    list(num = num, files = files)
  })
}

#' Build a target file name from a prefix, number and suffix
#'
#' @param prefix Job prefix.
#' @param num Job number.
#' @param suffix Name suffix.
#' @return The uppercase target name.
#' @noRd
psid_target_name <- function(prefix, num, suffix) {
  paste0(toupper(prefix), num, suffix)
}

#' Does an output file already exist?
#'
#' Returns TRUE only when `checkdir` (non-recursive) holds a file whose full
#' base name equals `<name>.rda` or `<name>.RData` (case-insensitive) for
#' `format == "rda"`, or `<name>.rds` for `format == "rds"`. A non-existent
#' `checkdir` means FALSE.
#'
#' @param checkdir Directory to check.
#' @param name Target base name.
#' @param format Either `"rda"` or `"rds"`.
#' @return A logical scalar.
#' @noRd
psid_already_exists <- function(checkdir, name, format) {
  if (!dir.exists(checkdir)) {
    return(FALSE)
  }

  files <- list.files(checkdir)

  if (format == "rda") {
    target <- tolower(c(paste0(name, ".rda"), paste0(name, ".RData")))
  } else {
    target <- tolower(paste0(name, ".rds"))
  }

  any(tolower(files) %in% target)
}

#' Import locally downloaded PSID files
#'
#' @description
#' Imports PSID data files that the user downloaded manually (as zips,
#' folders, or loose \code{.txt} + \code{.sas} pairs) into \code{.rda} or
#' \code{.rds} files named \code{<PREFIX><year><suffix>.<format>} (for example
#' \code{FAM2021ER.rda} or \code{IND2023ER.rda}). These are the names that
#' \code{\link{build.panel}} expects.
#'
#' @details
#' Output files use the \code{format} argument; \code{format = "rds"} files
#' are not readable by \code{\link{build.panel}} in this version, so use the
#' default \code{"rda"} when the files are meant for \code{\link{build.panel}}.
#' Raw PSID files stay wherever the user keeps them; this package never
#' uploads or transmits them.
#'
#' @param inputdir Directory holding the downloaded files.
#' @param outputdir Directory where the imported files are written (created if
#'   missing).
#' @param prefix Job prefix, for example \code{"fam"} or \code{"ind"}.
#' @param checkdir Directory checked for already-imported files. Defaults to
#'   \code{outputdir}.
#' @param overwrite Logical; re-import jobs whose target file already exists.
#' @param format Either \code{"rda"} or \code{"rds"}.
#' @param suffix Name suffix appended after the year.
#'
#' @return
#' An invisible data.frame with columns \code{file}, \code{status}
#' (\code{created}, \code{skipped} or \code{error}) and \code{message}.
#'
#' @seealso \code{\link{build.panel}}
#' @export
#' @examples
#' \dontrun{
#' processPSID("path/to/downloaded-psid", "path/to/psid-output", "fam")
#' }
processPSID <- function(inputdir, outputdir, prefix,
                        checkdir = outputdir, overwrite = FALSE,
                        format = c("rda", "rds"), suffix = "ER") {
  format <- match.arg(format)

  if (!dir.exists(inputdir)) {
    stop("inputdir does not exist: ",
         normalizePath(inputdir, mustWork = FALSE), call. = FALSE)
  }

  dir.create(outputdir, showWarnings = FALSE, recursive = TRUE)

  jobs <- psid_find_jobs(inputdir, prefix)

  if (length(jobs) == 0L) {
    return(invisible(data.frame(file = character(0), status = character(0),
                                message = character(0))))
  }

  res <- lapply(jobs, function(job) {
    name <- psid_target_name(prefix, job$num, suffix)
    out <- file.path(outputdir, paste0(name, ".", format))
    label <- paste(basename(job$files), collapse = ", ")

    if (!overwrite && psid_already_exists(checkdir, name, format)) {
      futile.logger::flog.info("skipping %s: already exists", name)
      return(data.frame(file = label, status = "skipped", message = "",
                        stringsAsFactors = FALSE))
    }

    futile.logger::flog.info("importing %s", label)

    tryCatch({
      process_one(job$files, out, format)
      data.frame(file = label, status = "created", message = "",
                 stringsAsFactors = FALSE)
    }, error = function(e) {
      futile.logger::flog.warn("failed to import %s: %s", label,
                               conditionMessage(e))
      data.frame(file = label, status = "error",
                 message = conditionMessage(e), stringsAsFactors = FALSE)
    })
  })

  invisible(do.call(rbind, res))
}

#' Import locally downloaded PSID family files
#'
#' @description
#' Imports family files (prefix \code{"fam"}) via \code{\link{processPSID}}.
#'
#' @param inputdir Directory holding the downloaded files.
#' @param outputdir Directory where the imported files are written.
#' @param ... Further arguments passed to \code{\link{processPSID}}.
#'
#' @return
#' An invisible data.frame with columns \code{file}, \code{status} and
#' \code{message}.
#'
#' @seealso \code{\link{processPSID}}, \code{\link{build.panel}}
#' @export
#' @examples
#' \dontrun{
#' getFamilyPSID("path/to/downloaded-psid", "path/to/psid-output")
#' }
getFamilyPSID <- function(inputdir, outputdir, ...) {
  processPSID(inputdir, outputdir, prefix = "fam", ...)
}

#' Import locally downloaded PSID individual files
#'
#' @description
#' Imports individual files (prefix \code{"ind"}) via
#' \code{\link{processPSID}}.
#'
#' @param inputdir Directory holding the downloaded files.
#' @param outputdir Directory where the imported files are written.
#' @param ... Further arguments passed to \code{\link{processPSID}}.
#'
#' @return
#' An invisible data.frame with columns \code{file}, \code{status} and
#' \code{message}.
#'
#' @seealso \code{\link{processPSID}}, \code{\link{build.panel}}
#' @export
#' @examples
#' \dontrun{
#' getIndPSID("path/to/downloaded-psid", "path/to/psid-output")
#' }
getIndPSID <- function(inputdir, outputdir, ...) {
  processPSID(inputdir, outputdir, prefix = "ind", ...)
}

#' Import locally downloaded PSID family and individual files
#'
#' @description
#' Imports one or more prefixes (default \code{"fam"} and \code{"ind"}) via
#' \code{\link{processPSID}} and combines the per-prefix results.
#'
#' @param inputdir Directory holding the downloaded files.
#' @param outputdir Directory where the imported files are written.
#' @param prefixes Character vector of prefixes to process.
#' @param ... Further arguments passed to \code{\link{processPSID}}.
#'
#' @return
#' An invisible data.frame with columns \code{file}, \code{status} and
#' \code{message}.
#'
#' @seealso \code{\link{processPSID}}, \code{\link{build.panel}}
#' @export
#' @examples
#' \dontrun{
#' getPSID("path/to/downloaded-psid", "path/to/psid-output")
#' }
getPSID <- function(inputdir, outputdir, prefixes = c("fam", "ind"), ...) {
  out <- lapply(prefixes, function(p) {
    processPSID(inputdir, outputdir, prefix = p, ...)
  })
  invisible(do.call(rbind, out))
}
