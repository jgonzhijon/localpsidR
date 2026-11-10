# localpsidR

localpsidR builds wide-format panel data sets from Panel Study of Income
Dynamics (PSID) raw data. It is a fork of
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
credentials.

The package needs the Cross-year Individual file and the Family File of every
survey year requested in the panel. `getPSID()` recognises them by their names,
which begin with `fam` for the Family Files and `ind` for the Cross-year
Individual file, each followed by the survey year, and it writes one `.rda`
file per input named `FAM<year>ER.rda` and `IND<year>ER.rda` (or `.rds` with
`format = "rds"`). For a wave built with `detect_waves = TRUE`, both the new
Cross-year Individual file and the new Family File must be imported.

The files are imported once with `getPSID()`, and `build.panel()` reads the
`.rda` files it writes.

## Workflow

### Import

Place the downloaded zips in one folder and import them:

```r
getPSID("path/to/downloaded-psid", "path/to/psid-output")
```

`getPSID()` imports the family and individual files as `.rda` files named
`FAM<year>ER.rda` and `IND<year>ER.rda` in the output folder. `getFamilyPSID()`
and `getIndPSID()` import a single prefix, and `processPSID()` is the
underlying function, which also accepts folders or loose `.txt` and `.sas`
pairs. When `getPSID()` imports a wave that is not built in, it writes
`psid_waves_detected.csv` (variable names and years only, no respondent data)
into the output folder.

### Building the panel

`build.panel()` reads the `FAM<year>ER` and `IND<year>ER` files from a data
directory and returns a wide-format `data.table` with a `pid` column (the
person identifier, constructed from `ID1968` and `pernum`) and a `year` column,
plus the requested variables.

```r
build.panel(datadir = "path/to/psid-output",
            fam.vars = fam.vars,
            ind.vars = ind.vars)
```

`fam.vars` and `ind.vars` are data.frames with an integer `year` column and one
column per variable. Each cell holds the PSID variable name for that wave, and
the column name becomes the variable name in the output. `ind.vars` holds
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

The merge matches the interview number of each family file to the interview
number of the same year in the individual file. `fam.vars` supplies the
substantive variables and `ind.vars` the person-level variables.

`heads.only = TRUE` keeps household heads only; `current.heads.only = TRUE`
keeps current heads only and distinguishes movers-out heads. `sample` selects a
sample (`"SRC"`, `"SEO"`, `"immigrant"` or `"latino"`; `NULL` does no
subsetting). `design` controls which individuals are kept: `"balanced"` keeps
individuals present in every requested wave, `"all"` keeps everyone, and an
integer keeps individuals present in at least that many consecutive waves.

An example run and its output:

```r
build.panel(datadir = "~/psid", fam.vars = f, ind.vars = i,
            heads.only = TRUE, sample = "SRC", design = "all")
```

```text
INFO [2018-10-10 11:28:25] localpsidR: Loading Family data from .rda files
INFO [2018-10-10 11:28:34] localpsidR: loaded individual file: ~/psid/IND2015ER.rda
INFO [2018-10-10 11:28:34] localpsidR: total memory load in MB: 1252
INFO [2018-10-10 11:28:34]
INFO [2018-10-10 11:28:34] localpsidR: currently working on data for year 2003
INFO [2018-10-10 11:28:36]
INFO [2018-10-10 11:28:36] localpsidR: currently working on data for year 2005
INFO [2018-10-10 11:28:37]
INFO [2018-10-10 11:28:37] localpsidR: currently working on data for year 2007
INFO [2018-10-10 11:28:39] balanced design reduces sample from 97377 to 89571
INFO [2018-10-10 11:28:39] End of build.panel
```

### Variable lists and the crosswalk

The package ships variable lists (`inst/psid-lists/famvars.txt` and
`indvars.txt`) and the UMich crosswalk (`psid.xlsx`). `getNamesPSID()` returns
the name of a variable in other waves:

```r
cwf <- openxlsx::read.xlsx(system.file("psid-lists", "psid.xlsx", package = "localpsidR"))
getNamesPSID("ER17013", cwf, years = c(2003, 2005, 2007))
```

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
writes them to `psid_waves_detected.csv` (metadata only, no respondent data) in
the output folder. Import both the IND and the FAM zip of the new wave, then
build it with `detect_waves = TRUE`:

```r
getPSID(inputdir = "path/to/downloaded-psid", outputdir = "path/to/psid-output")
build.panel(datadir = "path/to/psid-output",
            fam.vars = data.frame(year = <year>, v = "<variable name>"),
            detect_waves = TRUE, head_code = <head code>)
```

`datadir` must contain `psid_waves_detected.csv` (in the same folder as the
imported files). `build.panel()` logs a warning listing the variable names it
uses. The head code is `head_code` if given, otherwise it is carried over from
the latest built-in wave with a warning (`head_code` is ignored unless
`detect_waves = TRUE`). For each requested year that was detected,
`build.panel()` stops if fewer than 80% of the nonzero individual interview
numbers appear among the family interview numbers. Only the modern
family-interview label form (`<YYYY> FAMILY INTERVIEW (ID) NUMBER`) is
detected. Detection is a convenience for a first look: verify the detected
variable names and the head code against the PSID codebook when the result
matters, and make the wave permanent by editing `makeids()` (see
[ADDING_A_WAVE.md](ADDING_A_WAVE.md)).

## Differences from the original

- Fixed the deprecated `data.table` `with = FALSE` syntax, which broke
  `build.panel()` when a requested family variable was `NA` for a year.
- Replaced the interactive download with the import of locally downloaded files
  (`getPSID()`, `getFamilyPSID()`, `getIndPSID()`, `processPSID()`); the package
  no longer downloads data and does not handle credentials.
- Added support for the PSID 2023 wave and a wave framework.
- Added `extendVarsPSID()` and `data-raw/update-varlists.R` to extend the
  variable lists to new waves.
- Added detection of new waves (`detect_waves`, `head_code`,
  `psid_waves_detected.csv`).
- Renamed the package to localpsidR.

## Origin and credits

This repository is a fork of [psidR](https://github.com/floswald/psidR) by
Florian Oswald. The original authors keep full credit for the package. The
package is licensed under GPL-3.

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
