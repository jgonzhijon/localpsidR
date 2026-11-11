# Adding a new PSID wave

Waves later than `psid_latest_wave()` are rejected by
[`build.panel()`](reference/build.panel.md) by default. After importing
the zips of a new wave with [`getPSID()`](reference/getPSID.md), you can
build it right away with `build.panel(..., detect_waves = TRUE)`, which
reads `psid_waves_detected.csv` (written by
[`getPSID()`](reference/getPSID.md)) and uses the detected variable
names with a warning. The checklist below stays the reference and the
permanent route: it makes the wave work for everyone without
`detect_waves = TRUE`. Detection does not update `psid.xlsx`, so the
variable lists still have to be extended yourself with
[`extendVarsPSID()`](reference/extendVarsPSID.md).

1.  `R/makeids.r`, [`makeids()`](reference/makeids.md): extend `year`;
    append ONE element to each of `ind.interview`, `ind.seq`, `ind.head`
    and `fam.interview` (names from the new IND and FAM codebooks, never
    guessed — the crosswalk can only suggest candidates such as
    `ERxxxxx`); extend the `ind.head.num`
    [`rep()`](https://rdrr.io/r/base/rep.html) count, and confirm the
    head code in the codebook (the label may have changed). This manual
    edit is the permanent route: once it is released,
    [`build.panel()`](reference/build.panel.md) supports the new wave
    without `detect_waves = TRUE`.

2.  `psid_family_table()` in `R/makeids.r`: extend `year` and append
    `NA_integer_` to `file`.

3.  `inst/psid-lists/`: replace `psid.xlsx` with the latest official
    PSID cross-year workbook (PSID Data Center, “Cross-Year Index”; do
    not edit cells by hand), then run
    `Rscript data-raw/update-varlists.R` (dry run). Review the review
    table against the codebooks (definitions can change between waves;
    check every status other than `added`), and only then run
    `Rscript data-raw/update-varlists.R write`.

4.  Sample ranges: check whether new samples were added to the PSID
    sample definitions in [`build.panel()`](reference/build.panel.md)
    (SRC/SEO/immigrant/latino ranges in `R/build.panel.r`) and in the
    synthetic fixture [`testPSID()`](reference/testPSID.md)
    (`R/makeids.r`).

5.  `devtools::test()`: `test-waves.R`, `test-wave-latest.R` and
    `test-psid-lists.R` fail loudly if a vector has the wrong length, a
    name is duplicated or a list contains a year that is not supported;
    no test needs editing.

6.  Add a NEWS bullet.

7.  Never commit real PSID data; tests use synthetic fixtures only.
