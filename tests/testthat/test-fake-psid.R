context("synthetic fixture helper")

test_that("fake_psid_datadir creates expected files, each loading as one object", {
  n <- 20L
  d <- fake_psid_datadir(c(1985, 1986), n = n)

  expect_true(dir.exists(d$dir))

  ind_year <- max(locapsidR:::makeids()$year)
  expected <- c("FAM1985ER.rda", "FAM1986ER.rda", paste0("IND", ind_year, "ER.rda"))
  expect_true(all(expected %in% list.files(d$dir)))

  for (f in expected) {
    env <- new.env()
    load(file.path(d$dir, f), envir = env)
    expect_identical(ls(env), "x")
    expect_true(is.data.frame(env$x))
  }

  expect_identical(names(d), c("dir", "ind", "fam"))
  expect_true(is.data.frame(d$ind))
  expect_identical(names(d$fam), as.character(c(1985, 1986)))
  expect_true(all(vapply(d$fam, is.data.frame, logical(1))))
})

test_that("build.panel runs on the synthetic fixture", {
  n <- 20L
  d <- fake_psid_datadir(c(1985, 1986), n = n)

  out <- build.panel(
    datadir = d$dir,
    fam.vars = data.frame(year = c(1985, 1986), v = c("FAMVAR1", "FAMVAR1")),
    heads.only = FALSE,
    design = "all",
    loglevel = WARN
  )

  expect_equal(nrow(out), 2 * n)
  expect_true(all(c("pid", "year", "interview", "sequence", "relation.head", "v") %in%
                    names(out)))
})
