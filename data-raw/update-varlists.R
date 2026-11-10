# Update the shipped PSID variable lists to new waves using the crosswalk.
#
# Usage (from the repository root):
#   Rscript data-raw/update-varlists.R          dry run: print everything, write nothing
#   Rscript data-raw/update-varlists.R write    also write inst/psid-lists/{famvars,indvars}.txt

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0L && !(length(args) == 1L && args[1] == "write")) {
  stop("usage: Rscript data-raw/update-varlists.R [write]", call. = FALSE)
}
write_mode <- length(args) == 1L && args[1] == "write"

# Must be run from the repository root.
if (!file.exists("DESCRIPTION")) {
  stop("run this script from the repository root", call. = FALSE)
}
desc_lines <- readLines("DESCRIPTION")
if (!any(trimws(desc_lines) == "Package: localpsidR")) {
  stop("DESCRIPTION does not declare 'Package: localpsidR'; run from the repository root",
       call. = FALSE)
}

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("the 'devtools' package is required; install it first", call. = FALSE)
}
devtools::load_all(quiet = TRUE)

options(width = 200)

# --- read inputs ----------------------------------------------------------

cwf <- openxlsx::read.xlsx("inst/psid-lists/psid.xlsx")
fam <- data.table::fread("inst/psid-lists/famvars.txt", na.strings = "")
ind <- data.table::fread("inst/psid-lists/indvars.txt", na.strings = "")

# --- round-trip check (always, dry run included) -------------------------

roundtrip_check <- function(path) {
  d <- data.table::fread(path, na.strings = "")
  d$name <- trimws(d$name)
  tmp <- tempfile(fileext = ".txt")
  data.table::fwrite(d, tmp, quote = "auto", sep = ",")

  orig <- readChar(path, file.info(path)$size)
  new  <- readChar(tmp, file.info(tmp)$size)
  # The shipped files do not end with a newline; fwrite always adds one.
  orig <- sub("\n$", "", orig)
  new  <- sub("\n$", "", new)

  if (identical(orig, new)) {
    cat("round-trip identical: TRUE\n")
    return(invisible(NULL))
  }

  ol <- strsplit(orig, "\n", fixed = TRUE)[[1]]
  nl <- strsplit(new, "\n", fixed = TRUE)[[1]]
  cat("round-trip identical: FALSE\n")
  if (length(ol) != length(nl)) {
    cat("  line count differs:", length(ol), "vs", length(nl), "\n")
    return(invisible(NULL))
  }
  idx <- which(ol != nl)
  cat("  differing lines:", length(idx), "\n")
  affected <- unique(trimws(sub("^.*,", "", ol[idx])))
  cat("  affected concepts:", paste(affected, collapse = ", "), "\n")
  invisible(NULL)
}

cat("== round-trip check (unextended, name trimmed) ==\n")
cat("[famvars] "); roundtrip_check("inst/psid-lists/famvars.txt")
cat("[indvars] "); roundtrip_check("inst/psid-lists/indvars.txt")
cat("\n")

# --- extend --------------------------------------------------------------

extend_one <- function(vars, label) {
  warns <- character()
  res <- withCallingHandlers(
    extendVarsPSID(vars, cwf),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (length(warns) > 0L) {
    cat("WARNINGS for", label, ":\n")
    for (w in warns) cat("  -", w, "\n")
  }
  res
}

fam_ext <- extend_one(fam, "famvars")
ind_ext <- extend_one(ind, "indvars")

print_review <- function(label, res, old_n) {
  cat("==", label, "==\n")
  cat("rows added:", nrow(res$vars) - old_n, "\n")
  cat("status counts:\n")
  print(table(res$review$status))
  cat("review (all rows):\n")
  print(res$review, row.names = FALSE)
  cat("\n")
}

print_review("famvars", fam_ext, nrow(fam))
print_review("indvars", ind_ext, nrow(ind))

# --- anchor context -------------------------------------------------------

anchor_context <- function(vars) {
  out <- list()
  for (cn in unique(vars$name)) {
    sub <- vars[vars$name == cn, , drop = FALSE]
    anchor <- sub[which.max(sub$year), , drop = FALSE]
    av <- anchor$variable
    m <- which(cwf == av, arr.ind = TRUE)
    if (nrow(m) > 0L) {
      r <- m[1, "row"]
      out[[length(out) + 1L]] <- data.frame(
        name = cn, anchor_year = anchor$year, anchor_variable = av,
        TEXT = cwf$TEXT[r], HEAD_WIFE = cwf$HEAD_WIFE[r],
        CATEGORY = cwf$CATEGORY[r], stringsAsFactors = FALSE)
    } else {
      out[[length(out) + 1L]] <- data.frame(
        name = cn, anchor_year = anchor$year, anchor_variable = av,
        TEXT = NA_character_, HEAD_WIFE = NA_character_,
        CATEGORY = NA_character_, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}

cat("== anchor context (famvars) ==\n")
print(anchor_context(fam), row.names = FALSE)
cat("\n== anchor context (indvars) ==\n")
print(anchor_context(ind), row.names = FALSE)
cat("\n")

# --- extra checks ---------------------------------------------------------

cat("== extra checks ==\n")
for (nm in c("famvars", "indvars")) {
  res <- if (nm == "famvars") fam_ext else ind_ext
  old_n <- if (nm == "famvars") nrow(fam) else nrow(ind)

  added <- res$vars[seq.int(old_n + 1L, nrow(res$vars)), ]
  bad_var <- added$variable[!grepl("^(ER|V)[0-9]+$", added$variable)]
  cat(nm, "- added variables not matching ^(ER|V)[0-9]+$:", length(bad_var), "\n")
  if (length(bad_var)) print(bad_var)

  v <- as.data.frame(res$vars)
  v_real <- v[grepl("^(ER|V)[0-9]+$", v$variable), ]
  dup <- v_real[duplicated(v_real[, c("dataset", "year", "variable")]), ]
  cat(nm, "- duplicate (dataset, year, variable) rows (real variables):",
      nrow(dup), "\n")

  years <- c(2019L, 2021L, 2023L)
  grid <- sum(vapply(unique(v$name), function(cn) {
    all(years %in% v$year[v$name == cn])
  }, logical(1)))
  cat(nm, "- concepts with rows for 2019, 2021 and 2023:", grid, "\n")
}

# --- write ----------------------------------------------------------------

write_list <- function(d, path) {
  tmp <- tempfile(fileext = ".txt")
  data.table::fwrite(d, tmp, quote = "auto", sep = ",", na = "")
  txt <- readChar(tmp, file.info(tmp)$size)
  txt <- sub("\n$", "", txt)
  writeBin(charToRaw(txt), path)
}

if (write_mode) {
  write_list(fam_ext$vars, "inst/psid-lists/famvars.txt")
  write_list(ind_ext$vars, "inst/psid-lists/indvars.txt")
  cat("\nWROTE inst/psid-lists/famvars.txt and inst/psid-lists/indvars.txt\n")
  cat("git diff --numstat hints:\n")
  system2("git", c("diff", "--numstat", "--",
                   "inst/psid-lists/famvars.txt", "inst/psid-lists/indvars.txt"))
} else {
  cat("\nDRY RUN: no files written\n")
}

