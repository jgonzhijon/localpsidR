# localpsidR

localpsidR builds long-format panel data sets (one row per person and
wave) from Panel Study of Income Dynamics (PSID) raw data. It is a fork of
[psidR](https://github.com/floswald/psidR) by Florian Oswald, and the
original authors keep full credit for the package.

[![R-CMD-check](https://github.com/jgonzhijon/localpsidR/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/jgonzhijon/localpsidR/actions/workflows/check-standard.yaml)

## Installation

```r
remotes::install_github("jgonzhijon/localpsidR")
```

`install.packages("psidR")` installs the original package, not this fork.

## Data requirements

The package does not provide or download data. Obtain the files from the PSID
Data Center, under "Packaged Data" > "Main and Supplemental Studies"
(https://simba.isr.umich.edu/Zips/ZipMain.aspx), where the Family Files and the
Cross-year Individual file are listed. Access requires registering with the
PSID and having authorization to download the files; the package never handles
credentials. The package needs the Cross-year Individual file and the Family
File of every survey year requested in the panel.

## Workflow

1. Import the PSID files with `getPSID()`.
2. Choose the variables (`fam.vars` and `ind.vars`).
3. Build the panel with `build.panel()`.
4. Keep the rows you need (the panel is in long format).

### Import

Place the downloaded zips in one folder and import them:

```r
getPSID("path/to/downloaded-psid", "path/to/psid-output")
```

`getPSID()` imports the family and individual files as `.rda` files named
`FAM<year>ER.rda` and `IND<year>ER.rda` in the output folder. `getFamilyPSID()`
and `getIndPSID()` import a single prefix, and `processPSID()` is the
underlying function, which also accepts folders or loose `.txt` and `.sas`
pairs.

### Choosing the variables

`fam.vars` takes Family File variables ("PSID Family-level") and `ind.vars` Cross-year Individual variables ("PSID Individual-level").

`fam.vars` and `ind.vars` are data.frames with an integer `year` column and one
column per variable. Each cell holds the PSID variable name for that wave, and
the column name becomes the variable name in the output. Do not use column names that contain `year` or `variable` (for example `birth_year` or `my_variable`): `build.panel()` stops with an error. `ind.vars` holds
individual-file variables such as survey weights and must not include the
identifier variables `ER30001` and `ER30002`. The format is:

```r
> head(i)  # individual file example
   year     age    educ empstat  weight
1: 1968 ER30004 ER30010    <NA> ER30019
2: 1969 ER30023    <NA>    <NA> ER30042        # NOTICE THE NA for educ HERE
3: 1970 ER30046 ER30052    <NA> ER30066
4: 1971 ER30070 ER30076    <NA> ER30090
5: 1972 ER30094 ER30100    <NA> ER30116
6: 1973 ER30120 ER30126    <NA> ER30137

> head(f)  # family file example
   year age_youngest_child debt empstat_ faminc hours hvalue ...
1: 1968               V120 <NA>     V196    V81   V47     V5 ...
2: 1969              V1013 <NA>     V639   V529  V465   V449 ...
3: 1970              V1243 <NA>    V1278  V1514 V1138  V1122 ...
4: 1971              V1946 <NA>    V1983  V2226 V1839  V1823 ...
5: 1972              V2546 <NA>    V2581  V2852 V2439  V2423 ...
6: 1973              V3099 <NA>    V3114  V3256 V3027  V3021 ...
```

A cell that is `NA` means the variable does not exist in that wave; the returned column
is `NA` for that wave. `ind.vars` is optional. Find the names in the PSID Data Center
Variable Search (https://simba.isr.umich.edu/DC/s.aspx), which searches name, label, question
and explanation text and can be filtered by data type and year, and in the PSID codebooks.

`build.panel()` uses exactly the names given; the same concept has a different
name in each wave (total family income is V81 in 1968 and V529 in 1969). The
crosswalk `psid.xlsx` lists, for each variable, its name in every wave, and
`getNamesPSID()` reads it: given the name in any wave, including a wave you do
not request, it returns the names in the others. It gives names, not
definitions; check the codebook for changes of definition between waves. A
variable with the same name in every wave (for example ER32000, sex of the
individual) is repeated in every row.

```r
cwf <- openxlsx::read.xlsx(system.file("psid-lists", "psid.xlsx", package = "localpsidR"))
getNamesPSID("ER17013", cwf, years = c(2003, 2005, 2007))
```

To build `fam.vars` from one name per variable, look the name up with `getNamesPSID()`. It returns the year as text such as `"Y2003"`, and `years` must be given in ascending order (otherwise the names are paired with the wrong years):

```r
yrs <- c(2019, 2021, 2023)
get_names <- function(x) getNamesPSID(x, cwf, years = yrs)$variable
fam.vars <- data.frame(year   = yrs,
                       faminc = get_names("ER71426"),
                       hours  = get_names("ER71233"))
```

Or write the tables by hand and build the panel:

```r
fam.vars <- data.frame(year = c(1968, 1969), faminc = c("V81", "V529"), hours = c("V47", "V465"))
ind.vars <- data.frame(year = c(1968, 1969), age = c("ER30004", "ER30023"), weight = c("ER30019", "ER30042"))
build.panel(datadir = "path/to/psid-output", fam.vars = fam.vars, ind.vars = ind.vars,
            design = "all")
```

The result always contains `ID1968`, `pernum`, `interview`, `sequence`, `relation.head`,
`pid` and `year`, plus one column per requested variable. The order of the columns of
`fam.vars` matters: in each wave `build.panel()` takes the first family variable that has a name in that wave and drops the rows where it is `NA` (families that did not respond). Put first a variable that is filled for every responding family in every wave, such as the number of people in the family unit.

### Building the panel

`build.panel()` reads the `FAM<year>ER` and `IND<year>ER` files from a data
directory and returns a long-format `data.table` (one row per person and wave)
with a `pid` column (the person identifier, constructed from `ID1968` and
`pernum`) and a `year` column, plus the requested variables.

```r
build.panel(datadir = "path/to/psid-output",
            fam.vars = fam.vars,
            ind.vars = ind.vars)
```

The merge matches the interview number of each family file to the interview
number of the same year in the individual file. `fam.vars` supplies the
substantive variables and `ind.vars` the person-level variables.

`heads.only = TRUE` keeps heads only (`relation.head` equal to the head code
and `sequence` 1 to 20; in 1968, which has no sequence number, the head code
alone). `current.heads.only = TRUE` keeps current heads only (`relation.head`
equal to the head code and `sequence` 1; in 1968, the head code alone).
`sample` selects a sample (`"SRC"`, `"SEO"`, `"immigrant"` or `"latino"`;
`NULL` does no subsetting). `design` controls which individuals are kept:
`"balanced"` keeps individuals present in every requested wave, `"all"` keeps
everyone, and an integer keeps individuals present in at least that many
consecutive waves.

For example, heads of the SRC sample only:

```r
build.panel(datadir = "~/psid", fam.vars = f, ind.vars = i,
            heads.only = TRUE, sample = "SRC", design = "all")
```

### Long format: which rows to keep

The `sequence` column describes the status of the person at the interview of
that wave (PSID individual codebook, verified on real data for waves 2011 to
2023): 1 to 20, in the family at the time of the interview; 51 to 59, in an
institution; 71 to 80, moved out of the family and established an own household
since the previous wave; 81 to 89, died since the previous wave; 0, born or
moved in after the interview. Persons with `sequence` equal to 0 carry the
family interview number of that wave but have `relation.head` equal to 0 and no
individual information.

Which rows to keep. Members of the family at the time of the interview:
`sequence` from 1 to 20. Current heads (reference persons): `relation.head`
equal to 10 together with `sequence` equal to 1; code 10 alone also includes
heads of the previous wave who died, are in an institution or moved out.
Spouses or partners: `relation.head` equal to 20 or 22 among the members. On
real data for waves 2011 to 2023, all member rows (`sequence` 1 to 20) with
these codes have `sequence` 2.

Family-file variables are repeated on every row of the family. For analyses at
the family level keep one row per family, that is the head (code 10 and
sequence 1); otherwise a family is counted once per member.

For the waves 2011 to 2023, the number of rows with sequence 1 to 20 per wave
and the number of heads with sequence 1 per wave equal the figures in the PSID
individual codebook (number of persons in the family and number of families
interviewed). For earlier waves the user should check the codebook.

```r
d <- build.panel(datadir = "path/to/psid-output",
                 fam.vars = fam.vars,
                 ind.vars = ind.vars,
                 heads.only = FALSE,
                 current.heads.only = FALSE,
                 design = "all")

# keep family members
d[d$sequence >= 1 & d$sequence <= 20, ]

# keep current heads
d[d$relation.head == 10 & d$sequence == 1, ]

# keep spouses or partners
d[d$sequence >= 1 & d$sequence <= 20 & d$relation.head %in% c(20, 22), ]

# keep heads and spouses together
d[d$sequence >= 1 & d$sequence <= 20 &
    (d$relation.head == 10 & d$sequence == 1 |
       d$relation.head %in% c(20, 22)), ]
```

`heads.only` and `current.heads.only` keep heads only (see "Building the panel"). To keep heads and spouses together, set both to `FALSE` and use the filter above.

### Variable lists and the crosswalk

The package ships variable lists (`inst/psid-lists/famvars.txt` and
`indvars.txt`). The crosswalk `psid.xlsx` and `cwf` are defined in "Choosing the variables".

`extendVarsPSID()` extends a variable list to waves that are missing from it,
using the crosswalk:

```r
f <- data.table::fread(system.file("psid-lists", "famvars.txt", package = "localpsidR"), na.strings = "")
ext <- extendVarsPSID(f, cwf)
ext$review  # inspect: every line that is not "added"
```

Check `ext$review` against the PSID codebooks before using the result, because
a variable can change definition between waves even when its crosswalk name
changes consistently. The crosswalk can be your own newer `psid.xlsx` — for
example one with corrected variable names for waves this package already
supports. Waves later than the latest one this package supports also need a
package update, because `build.panel()` stops for unsupported waves even when
the lists have been extended.

## New waves

`getPSID()` detects the variable names of waves that are not built in and
writes them to `psid_waves_detected.csv` (variable names and years only, no
respondent data) in the output folder. Import both the IND and the FAM zip,
then build with `detect_waves = TRUE`:

```r
getPSID(inputdir = "path/to/downloaded-psid", outputdir = "path/to/psid-output")
build.panel(datadir = "path/to/psid-output",
            fam.vars = data.frame(year = <year>, v = "<variable name>"),
            detect_waves = TRUE, head_code = <head code>)
```

`datadir` must contain `psid_waves_detected.csv`. `build.panel()` logs a
warning listing the variable names it uses. The head code is `head_code` if
given, otherwise it is carried over from the latest built-in wave with a
warning (`head_code` is ignored unless `detect_waves = TRUE`). For each
detected year, `build.panel()` stops if fewer than 80% of the nonzero
individual interview numbers appear among the family interview numbers. Only
the modern family-interview label form (`<YYYY> FAMILY INTERVIEW (ID) NUMBER`)
is detected. Detection is a convenience for a first look: verify the detected
variable names and the head code against the PSID codebook, and make the wave
permanent by editing `makeids()` (see [ADDING_A_WAVE.md](ADDING_A_WAVE.md)).

## Differences from the original

This repository is a fork of [psidR](https://github.com/floswald/psidR) by Florian Oswald.
The original authors keep full credit; the package is licensed under GPL-3.

- Fixed the deprecated `data.table` `with = FALSE` syntax, which broke `build.panel()` when a requested family variable was `NA`.
- Replaced the interactive download with local import (`getPSID()`, `getFamilyPSID()`, `getIndPSID()`, `processPSID()`); the package no longer downloads data or handles credentials.
- Added support for the PSID 2023 wave and a wave framework.
- Added `extendVarsPSID()` and `data-raw/update-varlists.R` to extend the variable lists to new waves.
- Added detection of new waves (`detect_waves`, `head_code`, `psid_waves_detected.csv`).
- Renamed the package to localpsidR.

## Citation

`citation("localpsidR")` returns the original psidR reference first and the
localpsidR reference second:

```r
citation("localpsidR")
```

Please cite the original psidR package. If you also use the adaptations added
by localpsidR, cite localpsidR as well.

The original package's reference is kept as it is:

```text
To cite the 'psidR' package in publications use:

  Florian Oswald (2021). psidR: Build Panel Data Sets from PSID Raw Data.
  R package version 2.1.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {psidR: Build Panel Data Sets from PSID Raw Data},
    author = {Florian Oswald},
    year = {2021},
    note = {R package version 2.1},
    url = {https://github.com/floswald/psidR},
  }
```

The original package's Zenodo record is <https://zenodo.org/badge/latestdoi/9247059>.