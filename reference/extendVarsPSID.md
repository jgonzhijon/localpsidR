# Extend a PSID variable list to new waves using the crosswalk

`extendVarsPSID()` reads a long variable list (one row per concept and
wave) and appends the rows for waves that are missing from it, taking
the variable name of each new wave from the UMich crosswalk. Existing
rows are never overwritten or deleted.

## Usage

``` r
extendVarsPSID(vars, cwf, years = NULL, names = NULL)
```

## Arguments

- vars:

  A long data.frame (or data.table) with columns `dataset`, `year`,
  `variable`, `label` and `name`, as read from the
  `inst/psid-lists/*vars.txt` files.

- cwf:

  The crosswalk data.frame (the content of `psid.xlsx`), with one
  `Y<year>` column per wave.

- years:

  Waves to add. The default is every wave in `psid_supported_waves()`
  later than `max(vars$year)` that also has a `Y<year>` column in `cwf`.

- names:

  Concepts to extend. The default is the concepts whose latest year
  equals `max(vars$year)`. Concepts that ended earlier are skipped
  unless they are listed here.

## Value

A list with two components: `vars`, the old rows (with `name` trimmed)
followed by the newly added rows, and `review`, a data.frame with
columns `name`, `year`, `variable`, `anchor_year`, `anchor_variable` and
`status` describing what was done for each requested year.

## Details

A concept is "alive" when its latest year equals `max(vars$year)`, so a
concept with no variable in the newest wave counts as discontinued on
later runs unless it is listed in `names`. `years` default to the waves
of this package later than `max(vars$year)` that exist in the crosswalk.

For each concept the function takes its latest row as the anchor and
looks up the anchor's variable name in `cwf` with
[`getNamesPSID`](getNamesPSID.md). Years that already have a row for
that concept are reported as `already present` and are never
overwritten. Years whose crosswalk value is missing are reported as
`no variable in crosswalk for this year`. A concept whose anchor is not
in `cwf` is reported as `anchor not found in crosswalk`. The `review`
status column also contains `added` and `skipped: discontinued`.

## Examples

``` r
cwf <- data.frame(
  Y2017 = c("v2017a", "v2017b"),
  Y2019 = c("v2019a", "v2019b"),
  Y2021 = c("v2021a", "v2021b"),
  stringsAsFactors = FALSE
)
vars <- data.frame(
  dataset = "fam",
  year = 2017L,
  variable = c("v2017a", "v2017b"),
  label = c("Concept A", "Concept B"),
  name = c("conceptA", "conceptB"),
  stringsAsFactors = FALSE
)
extendVarsPSID(vars, cwf)
#> $vars
#>   dataset year variable     label     name
#> 1     fam 2017   v2017a Concept A conceptA
#> 2     fam 2017   v2017b Concept B conceptB
#> 3     fam 2019   v2019a Concept A conceptA
#> 4     fam 2021   v2021a Concept A conceptA
#> 5     fam 2019   v2019b Concept B conceptB
#> 6     fam 2021   v2021b Concept B conceptB
#> 
#> $review
#>       name year variable anchor_year anchor_variable status
#> 1 conceptA 2019   v2019a        2017          v2017a  added
#> 2 conceptA 2021   v2021a        2017          v2017a  added
#> 3 conceptB 2019   v2019b        2017          v2017b  added
#> 4 conceptB 2021   v2021b        2017          v2017b  added
#> 
```
