context("individual file columns")

test_that("build.panel errors when the index lacks the requested year columns", {
  n <- 20L
  d <- fake_psid_datadir(c(1985, 1986), n = n)

  ids <- psidR:::makeids()
  row1986 <- ids[list(1986)]
  drop_cols <- c(row1986$ind.interview, row1986$ind.seq, row1986$ind.head)

  env <- new.env()
  load(file.path(d$dir, paste0(psidR:::psid_ind_name(), ".rda")), envir = env)
  ind <- get(ls(env), env)
  ind <- ind[, !(toupper(names(ind)) %in% drop_cols)]
  x <- ind
  save(x, file = file.path(d$dir, paste0(psidR:::psid_ind_name(), ".rda")))

  fam.vars <- data.frame(year = c(1985, 1986), v = c("FAMVAR1", "FAMVAR1"))

  err <- tryCatch(
    build.panel(datadir = d$dir, fam.vars = fam.vars,
                design = "all", heads.only = FALSE, loglevel = WARN),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, row1986$ind.interview, fixed = TRUE)
  expect_match(err, "1986", fixed = TRUE)
})

test_that("build.panel builds for years whose columns are all present", {
  n <- 20L
  d <- fake_psid_datadir(c(1985, 1986), n = n)

  fam.vars <- data.frame(year = 1985, v = "FAMVAR1")
  out <- build.panel(datadir = d$dir, fam.vars = fam.vars,
                     design = "all", heads.only = FALSE, loglevel = WARN)

  expect_equal(nrow(out), n)
})
