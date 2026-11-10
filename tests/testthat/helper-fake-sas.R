# Synthetic fixed-width .txt + .sas fixture and zip helper (test-only).
#
# `write_fixed_width()` writes a tiny, fully synthetic fixed-width data file
# and the matching SAS import code so tests never touch real PSID microdata.
# `zip_files()` packs files into a zip using the `zip` package.

write_fixed_width <- function(df, dir, stem, labels = NULL) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  cols <- lapply(df, as.character)
  widths <- vapply(cols, function(col) {
    n <- nchar(col)
    n <- n[!is.na(n)]
    if (length(n) == 0L) 1L else max(n)
  }, integer(1))
  widths <- pmax(widths, 1L)

  lines <- vapply(seq_len(nrow(df)), function(r) {
    paste0(vapply(seq_along(cols), function(j) {
      sprintf(paste0("%", widths[j], "s"), cols[[j]][r])
    }, character(1)), collapse = "")
  }, character(1))

  writeLines(lines, file.path(dir, paste0(stem, ".txt")))

  starts <- cumsum(c(1L, widths[-length(widths)]))
  ends <- cumsum(widths)

  sas <- c(
    paste0("DATA ", stem, ";"),
    paste0("INFILE \"", stem, ".txt\";"),
    "INPUT",
    paste0("  ", names(df), " ", starts, "-", ends),
    ";"
  )

  if (!is.null(labels)) {
    label_lines <- paste0("      ", labels$var, "    LABEL=\"", labels$label,
                          "\"    format=f8.")
    sas <- c(sas, "ATTRIB", label_lines, ";")
  }

  writeLines(sas, file.path(dir, paste0(stem, ".sas")))

  invisible(c(txt = file.path(dir, paste0(stem, ".txt")),
              sas = file.path(dir, paste0(stem, ".sas"))))
}

zip_files <- function(files, zipfile) {
  zip::zip(zipfile, files, mode = "cherry-pick")
  invisible(zipfile)
}
