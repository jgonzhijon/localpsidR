#' Extend a PSID variable list to new waves using the crosswalk
#'
#' \code{extendVarsPSID()} reads a long variable list (one row per concept and
#' wave) and appends the rows for waves that are missing from it, taking the
#' variable name of each new wave from the UMich crosswalk. Existing rows are
#' never overwritten or deleted.
#'
#' @param vars A long data.frame (or data.table) with columns \code{dataset},
#'   \code{year}, \code{variable}, \code{label} and \code{name}, as read from the
#'   \code{inst/psid-lists/*vars.txt} files.
#' @param cwf The crosswalk data.frame (the content of \code{psid.xlsx}), with one
#'   \code{Y<year>} column per wave.
#' @param years Waves to add. The default is every wave in
#'   \code{psid_supported_waves()} later than \code{max(vars$year)} that also has a
#'   \code{Y<year>} column in \code{cwf}.
#' @param names Concepts to extend. The default is the concepts whose latest
#'   year equals \code{max(vars$year)}. Concepts that ended earlier are skipped
#'   unless they are listed here.
#'
#' @return A list with two components: \code{vars}, the old rows (with \code{name}
#'   trimmed) followed by the newly added rows, and \code{review}, a data.frame
#'   with columns \code{name}, \code{year}, \code{variable}, \code{anchor_year},
#'   \code{anchor_variable} and \code{status} describing what was done for each
#'   requested year.
#'
#' @details
#' A concept is "alive" when its latest year equals \code{max(vars$year)}, so a
#' concept with no variable in the newest wave counts as discontinued on
#' later runs unless it is listed in \code{names}. \code{years} default to the waves of
#' this package later than \code{max(vars$year)} that exist in the crosswalk.
#'
#' For each concept the function takes its latest row as the anchor and looks
#' up the anchor's variable name in \code{cwf} with \code{\link{getNamesPSID}}. Years that
#' already have a row for that concept are reported as \code{already present} and
#' are never overwritten. Years whose crosswalk value is missing are reported
#' as \code{no variable in crosswalk for this year}. A concept whose anchor is not
#' in \code{cwf} is reported as \code{anchor not found in crosswalk}. The \code{review}
#' status column also contains \code{added} and \code{skipped: discontinued}.
#'
#' @examples
#' cwf <- data.frame(
#'   Y2017 = c("v2017a", "v2017b"),
#'   Y2019 = c("v2019a", "v2019b"),
#'   Y2021 = c("v2021a", "v2021b"),
#'   stringsAsFactors = FALSE
#' )
#' vars <- data.frame(
#'   dataset = "fam",
#'   year = 2017L,
#'   variable = c("v2017a", "v2017b"),
#'   label = c("Concept A", "Concept B"),
#'   name = c("conceptA", "conceptB"),
#'   stringsAsFactors = FALSE
#' )
#' extendVarsPSID(vars, cwf)
#' @export
extendVarsPSID <- function(vars, cwf, years = NULL, names = NULL) {
  required <- c("dataset", "year", "variable", "label", "name")
  if (!all(required %in% base::names(vars))) {
    stop("vars must have columns: ", paste(required, collapse = ", "),
         call. = FALSE)
  }

  is_dt <- data.table::is.data.table(vars)
  vars <- as.data.frame(vars, stringsAsFactors = FALSE)
  vars$name <- trimws(vars$name)

  cast_year <- function(y) {
    if (is.integer(vars$year)) as.integer(y) else as.numeric(y)
  }

  supported <- psid_supported_waves()
  cwf_year_cols <- grep("^Y[0-9]{4}$", base::names(cwf), value = TRUE)
  cwf_years <- as.integer(sub("^Y", "", cwf_year_cols))

  max_year <- max(vars$year)

  if (is.null(years)) {
    years <- sort(intersect(supported[supported > max_year], cwf_years))
  } else {
    years <- sort(unique(as.integer(years)))
  }

  all_names <- unique(vars$name)
  alive <- unique(vars$name[vars$year == max_year])
  if (is.null(names)) {
    target <- alive
  } else {
    target <- unique(names)
  }

  review_rows <- list()
  new_rows <- list()

  push_review <- function(r) {
    review_rows[[length(review_rows) + 1L]] <<- r
  }
  push_new <- function(r) {
    new_rows[[length(new_rows) + 1L]] <<- r
  }

  # Concepts that ended earlier and were not requested explicitly.
  discontinued <- setdiff(all_names, alive)
  for (cn in setdiff(discontinued, target)) {
    sub <- vars[vars$name == cn, , drop = FALSE]
    anchor <- sub[which.max(sub$year), , drop = FALSE]
    push_review(data.frame(
      name = cn,
      year = NA_integer_,
      variable = NA_character_,
      anchor_year = anchor$year,
      anchor_variable = anchor$variable,
      status = "skipped: discontinued",
      stringsAsFactors = FALSE
    ))
  }


  for (cn in target) {
    sub <- vars[vars$name == cn, , drop = FALSE]
    if (nrow(sub) == 0L) next
    anchor <- sub[which.max(sub$year), , drop = FALSE]
    anchor_year <- anchor$year
    anchor_variable <- anchor$variable

    if (nrow(which(cwf == anchor_variable, arr.ind = TRUE)) == 0L) {
      push_review(data.frame(
        name = cn,
        year = NA_integer_,
        variable = NA_character_,
        anchor_year = anchor_year,
        anchor_variable = anchor_variable,
        status = "anchor not found in crosswalk",
        stringsAsFactors = FALSE
      ))
      next
    }

    # Years without a crosswalk column are flagged without calling
    # getNamesPSID(), so its own warning is never triggered.
    years_no_col <- setdiff(years, cwf_years)
    for (y in years_no_col) {
      push_review(data.frame(
        name = cn,
        year = cast_year(y),
        variable = NA_character_,
        anchor_year = anchor_year,
        anchor_variable = anchor_variable,
        status = "no variable in crosswalk for this year",
        stringsAsFactors = FALSE
      ))
    }

    years_lookup <- intersect(years, cwf_years)
    if (length(years_lookup) == 0L) next

    res <- tryCatch(
      getNamesPSID(anchor_variable, cwf, years = years_lookup),
      error = function(e) NULL
    )

    if (is.null(res) || nrow(res) == 0L) {
      for (y in years_lookup) {
        push_review(data.frame(
          name = cn,
          year = cast_year(y),
          variable = NA_character_,
          anchor_year = anchor_year,
          anchor_variable = anchor_variable,
          status = "no variable in crosswalk for this year",
          stringsAsFactors = FALSE
        ))
      }
      next
    }

    for (i in seq_len(nrow(res))) {
      y <- as.integer(sub("^Y", "", res$year[i]))
      val <- as.character(res$variable[i])

      if (y %in% sub$year) {
        push_review(data.frame(
          name = cn,
          year = cast_year(y),
          variable = sub$variable[sub$year == y][1],
          anchor_year = anchor_year,
          anchor_variable = anchor_variable,
          status = "already present",
          stringsAsFactors = FALSE
        ))
      } else if (is.na(val) || !nzchar(val)) {
        push_review(data.frame(
          name = cn,
          year = cast_year(y),
          variable = NA_character_,
          anchor_year = anchor_year,
          anchor_variable = anchor_variable,
          status = "no variable in crosswalk for this year",
          stringsAsFactors = FALSE
        ))
      } else {
        push_new(data.frame(
          dataset = anchor$dataset,
          year = cast_year(y),
          variable = val,
          label = anchor$label,
          name = cn,
          stringsAsFactors = FALSE
        ))
        push_review(data.frame(
          name = cn,
          year = cast_year(y),
          variable = val,
          anchor_year = anchor_year,
          anchor_variable = anchor_variable,
          status = "added",
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  review <- if (length(review_rows) > 0L) {
    do.call(rbind, review_rows)
  } else {
    data.frame(
      name = character(), year = integer(), variable = character(),
      anchor_year = integer(), anchor_variable = character(),
      status = character(), stringsAsFactors = FALSE
    )
  }

  out_vars <- if (length(new_rows) > 0L) rbind(vars, do.call(rbind, new_rows)) else vars
  if (is_dt) out_vars <- data.table::as.data.table(out_vars)

  bad <- review[review$status %in%
                  c("no variable in crosswalk for this year",
                    "anchor not found in crosswalk"), , drop = FALSE]
  if (nrow(bad) > 0L) {
    parts <- paste0(bad$name, " (",
                    ifelse(is.na(bad$year), "anchor", as.character(bad$year)),
                    "): ", bad$status)
    warning("extendVarsPSID: ", nrow(bad), " item(s) not added: ",
            paste(parts, collapse = "; "), call. = FALSE)
  }

  list(vars = out_vars, review = review)
}

