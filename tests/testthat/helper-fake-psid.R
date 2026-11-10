# Synthetic PSID fixture (test-only helper).
#
# `fake_psid_datadir()` writes a tiny, fully synthetic PSID datadir to disk so
# tests never touch real PSID microdata. The layout mirrors what `build.panel()`
# reads: one cross-year individual index `IND<ind_year>ER.rda` and one family
# file `FAM<year>ER.rda` per requested year, each saved as a single object
# named `x`. All values are non-negative integers (so the same frames can be
# reused for fixed-width writing in later steps), and every PSID variable name
# comes from `makeids()` rather than being hard-coded.

fake_psid_datadir <- function(years, n = 20L, dir = tempfile("psid_fake_"),
                              ind_year = NULL, extra = NULL) {
  stopifnot(is.numeric(years), length(years) >= 1L)

  ids <- localpsidR:::makeids(extra)

  if (is.null(ind_year)) {
    ind_year <- max(ids$year)
  }

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Cross-year individual index: the 1968 id/person columns plus, for every
  # requested year, the interview, sequence and head columns named by makeids().
  ind <- data.frame(
    ER30001 = seq_len(n),
    ER30002 = rep(1L, n)
  )
  for (y in years) {
    row <- ids[list(y)]
    ind[[row$ind.interview]] <- seq_len(n)
    if (!is.na(row$ind.seq)) {
      ind[[row$ind.seq]] <- rep(1L, n)
    }
    ind[[row$ind.head]] <- rep(as.integer(row$ind.head.num), n)
  }

  # One family file per year: the family interview number plus one synthetic
  # variable FAMVAR1. Save every frame as a single object named "x".
  fam <- list()
  for (y in years) {
    fam_df <- data.frame(
      interview = seq_len(n),
      FAMVAR1 = seq_len(n)
    )
    names(fam_df)[names(fam_df) == "interview"] <- ids[list(y)]$fam.interview
    fam[[as.character(y)]] <- fam_df

    x <- fam_df
    save(x, file = file.path(dir, paste0("FAM", y, "ER.rda")))
  }

  x <- ind
  save(x, file = file.path(dir, paste0("IND", ind_year, "ER.rda")))

  list(dir = dir, ind = ind, fam = fam)
}

# Synthetic six-column data.frame for makeids(extra = ), using obviously fake
# variable names (never real PSID names, unique across years).
fake_extra_wave <- function(years, head_num = 10) {
  years <- as.double(years)
  data.frame(
    year = years,
    ind.interview = sprintf("ERT%05d", years * 10L + 1L),
    ind.seq = sprintf("ERT%05d", years * 10L + 2L),
    ind.head = sprintf("ERT%05d", years * 10L + 3L),
    fam.interview = sprintf("ERT%05d", years * 10L + 4L),
    ind.head.num = rep(as.double(head_num), length(years)),
    stringsAsFactors = FALSE
  )
}

# Write a psid_waves_detected.csv with the five columns read by
# psid_read_detected (ind.head.num is not written). Accepts NA in any field.
write_fake_detected <- function(dir, extra) {
  data.table::fwrite(
    extra[, c("year", "ind.interview", "ind.seq", "ind.head", "fam.interview")],
    file.path(dir, "psid_waves_detected.csv"),
    na = "", row.names = FALSE, quote = FALSE
  )
}
