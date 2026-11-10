# Adding a new PSID wave

Waves later than `psid_latest_wave()` are rejected by `build.panel()` until
this checklist has been applied and released, even if a user extends the
variable lists themselves with `extendVarsPSID()`.

1. `R/makeids.r`, `makeids()`: extend `year`; append ONE element to each of
   `ind.interview`, `ind.seq`, `ind.head` and `fam.interview` (names from the
   new IND and FAM codebooks, never guessed — the crosswalk can only suggest
   candidates such as `ERxxxxx`); extend the `ind.head.num` `rep()` count,
   and confirm the head code in the codebook (the label may have changed).

2. `psid_family_table()` in `R/makeids.r`: extend `year` and append
   `NA_integer_` to `file`.

3. `inst/psid-lists/`: replace `psid.xlsx` with the latest official PSID
   cross-year workbook (PSID Data Center, "Cross-Year Index"; do not edit
   cells by hand), then run `Rscript data-raw/update-varlists.R` (dry run).
   Review the review table against the codebooks (definitions can change
   between waves; check every status other than `added`), and only then run
   `Rscript data-raw/update-varlists.R write`.

4. Sample ranges: check whether new samples were added to the PSID sample
   definitions in `build.panel()` (SRC/SEO/immigrant/latino ranges in
   `R/build.panel.r`) and in the synthetic fixture `testPSID()`
   (`R/makeids.r`).

5. `devtools::test()`: `test-waves.R`, `test-wave-latest.R` and
   `test-psid-lists.R` fail loudly if a vector has the wrong length, a name
   is duplicated or a list contains a year that is not supported; no test
   needs editing.

6. Add a NEWS bullet.

7. Never commit real PSID data; tests use synthetic fixtures only.
