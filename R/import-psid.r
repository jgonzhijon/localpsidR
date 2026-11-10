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

#' Parse variable names and labels from a SAS file
#'
#' Reads a `.sas` file in chunks, replaces every byte above 127 with a
#' space, and extracts the `VAR LABEL="..."` pairs. The label is only
#' trimmed, so its internal spacing is preserved.
#'
#' @param path Path to the `.sas` file.
#' @return A data.frame with columns `var` and `label` in file order, or a
#'   zero-row data.frame with the same columns when no line matches.
#' @noRd
psid_parse_sas_labels <- function(path) {
  if (!file.exists(path)) {
    stop("file does not exist: ", path, call. = FALSE)
  }

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)

  vars <- character()
  labels <- character()
  pat <- "^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s+LABEL\\s*=\\s*\"([^\"]*)\""

  repeat {
    lines <- readLines(con, n = 1000L, warn = FALSE, skipNul = TRUE)
    if (length(lines) == 0L) {
      break
    }

    # Replace every byte above 127 with a space before any other regex.
    lines <- gsub("[^\x01-\x7F]", " ", lines, useBytes = TRUE)

    m <- regexec(pat, lines, ignore.case = TRUE, useBytes = TRUE)
    parts <- regmatches(lines, m)
    matched <- lengths(parts) == 3L

    if (any(matched)) {
      vars <- c(vars, vapply(parts[matched], function(x) x[2], character(1)))
      labels <- c(labels,
                  vapply(parts[matched], function(x) x[3], character(1)))
    }
  }

  data.frame(var = vars, label = trimws(labels), stringsAsFactors = FALSE)
}


#' Merge detected waves into the detected-waves CSV
#'
#' Merges a data.frame of newly detected waves field by field into
#' `<dir>/psid_waves_detected.csv`, keeping only waves that are not built in.
#'
#' @param dir Directory holding the CSV.
#' @param new A data.frame with a `year` column and any subset of
#'   `ind.interview`, `ind.seq`, `ind.head`, `fam.interview`.
#' @return Invisibly, NULL.
#' @noRd
psid_merge_detected <- function(dir, new) {
  path <- file.path(dir, "psid_waves_detected.csv")
  cols <- c("year", "ind.interview", "ind.seq", "ind.head", "fam.interview")
  char_cols <- cols[-1L]

  new_full <- data.frame(
    year = as.integer(new$year),
    ind.interview = if ("ind.interview" %in% names(new))
      as.character(new$ind.interview) else NA_character_,
    ind.seq = if ("ind.seq" %in% names(new))
      as.character(new$ind.seq) else NA_character_,
    ind.head = if ("ind.head" %in% names(new))
      as.character(new$ind.head) else NA_character_,
    fam.interview = if ("fam.interview" %in% names(new))
      as.character(new$fam.interview) else NA_character_,
    stringsAsFactors = FALSE
  )

  old <- NULL
  if (file.exists(path)) {
    old <- tryCatch(data.table::fread(path, na.strings = "",
                                      data.table = FALSE),
                    error = function(e) NULL)
    if (is.null(old) || !all(cols %in% names(old))) {
      futile.logger::flog.warn(
        "existing %s is unreadable or lacks the expected columns; left untouched",
        basename(path)
      )
      return(invisible(NULL))
    }
    old <- old[, cols, drop = FALSE]
    old$year <- as.integer(old$year)
    for (c in char_cols) old[[c]] <- as.character(old[[c]])
  }

  new_full <- new_full[!(new_full$year %in% makeids()$year), , drop = FALSE]
  if (nrow(new_full) == 0L) {
    return(invisible(NULL))
  }

  merged <- if (is.null(old)) new_full else old

  for (i in seq_len(nrow(new_full))) {
    y <- new_full$year[i]
    idx <- which(merged$year == y)
    if (length(idx) == 0L) {
      merged <- rbind(merged, new_full[i, , drop = FALSE])
    } else {
      for (c in char_cols) {
        val <- new_full[[c]][i]
        if (!is.na(val)) {
          merged[[c]][idx] <- val
        }
      }
    }
  }

  merged <- merged[order(merged$year), ]
  rownames(merged) <- NULL

  part <- paste0(path, ".part")
  if (file.exists(part)) unlink(part)
  data.table::fwrite(merged, part, na = "")
  if (!file.rename(part, path)) {
    unlink(part)
    stop("failed to rename temporary file to ", path, call. = FALSE)
  }

  invisible(NULL)
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

#' Record detected waves for one imported file
#'
#' Runs the wave detectors on parsed labels and merges the result into the
#' detected-waves CSV, logging warnings for waves that cannot be used.
#'
#' @param labels A data.frame with columns `var` and `label`.
#' @param kind Either `"ind"` or `"fam"`.
#' @param dir Directory holding the detected-waves CSV.
#' @param out_name Output file base name, used to read the file year.
#' @return Invisibly, NULL.
#' @noRd
psid_record_detected <- function(labels, kind, dir, out_name) {
  fy <- sub("^[A-Za-z]+([0-9]+)ER.*$", "\\1", basename(out_name))
  fy <- suppressWarnings(as.integer(fy))
  if (length(fy) == 0L || is.na(fy)) fy <- NA_integer_

  built_in <- makeids()$year

  if (kind == "ind") {
    res <- psid_detect_ind_waves(labels)

    rej <- res$rejected[!(res$rejected$year %in% built_in), , drop = FALSE]
    for (i in seq_len(nrow(rej))) {
      futile.logger::flog.warn("rejected wave %s: %s",
                               rej$year[i], rej$reason[i])
    }

    if (!is.na(fy) && !(fy %in% built_in) && !(fy %in% res$waves$year)) {
      futile.logger::flog.warn("no detected wave for the file year %s", fy)
    }

    recorded <- sort(setdiff(res$waves$year, built_in))
    if (length(recorded) > 0L) {
      psid_merge_detected(dir, res$waves)
      futile.logger::flog.info("recorded detected waves: %s",
                               paste(recorded, collapse = ", "))
    }
  } else {
    res <- psid_detect_fam_wave(labels)

    if (is.null(res$wave)) {
      if (!is.na(fy) && !(fy %in% built_in)) {
        futile.logger::flog.warn("no family interview variable: %s",
                                 res$reason)
      }
    } else if (!is.na(fy) && res$wave$year != fy) {
      futile.logger::flog.warn(
        "detected family year %s differs from file year %s; not stored",
        res$wave$year, fy
      )
    } else {
      psid_merge_detected(dir, res$wave)
      if (!(res$wave$year %in% built_in)) {
        futile.logger::flog.info("recorded detected wave: %s",
                                 res$wave$year)
      }
    }
  }

  invisible(NULL)
}

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
process_one <- function(files, out, format = c("rda", "rds"),
                        detect = c("none", "ind", "fam"),
                        detect_dir = dirname(out)) {
  format <- match.arg(format)
  detect <- match.arg(detect)

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

  if (detect != "none") {
    tryCatch({
      labels <- psid_parse_sas_labels(cleaned_sas)
      psid_record_detected(labels, kind = detect, dir = detect_dir,
                           out_name = basename(out))
    }, error = function(e) {
      futile.logger::flog.warn("wave detection failed for %s: %s",
                               basename(out), conditionMessage(e))
    })
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
#' uploads or transmits them. For \code{fam} and \code{ind} jobs it also
#' writes \code{psid_waves_detected.csv} (variable names and years only, no
#' respondent data) into \code{outputdir} for waves that are not built in.
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

  detect <- if (tolower(prefix) %in% c("fam", "ind")) tolower(prefix) else "none"

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
      process_one(job$files, out, format, detect = detect,
                  detect_dir = outputdir)
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
