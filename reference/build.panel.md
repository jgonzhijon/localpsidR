# build.panel: Build PSID panel data set

Builds a panel data set with id variables `pid` (unique person
identifier) and `year` from individual PSID family files and
supplemental wealth files.

## Usage

``` r
build.panel(
  datadir = NULL,
  fam.vars,
  ind.vars = NULL,
  heads.only = FALSE,
  current.heads.only = FALSE,
  sample = NULL,
  design = "balanced",
  loglevel = INFO,
  detect_waves = FALSE,
  head_code = NULL
)
```

## Arguments

- datadir:

  path to a directory containing family files ("FAM\<year\>ER.rda" or
  "FAM\<year\>ER.RData") and the individual index file
  ("IND\<year\>ER.rda" or "IND\<year\>ER.RData"). If `NULL`,
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) is used, which is
  empty unless files were placed there.

- fam.vars:

  data.frame of variable to retrieve from family files. Can contain see
  example for required format.

- ind.vars:

  data.frame of variables to get from individual file. In almost all
  cases this will be the type of survey weights you want to use. don't
  include id variables ER30001 and ER30002.

- heads.only:

  logical TRUE if user wants household heads only. Household heads in
  sample year.

- current.heads.only:

  logical TRUE if user wants current household heads only. Distinguishes
  mover outs heads.

- sample:

  string indicating which sample to select: "SRC" (survey research
  center), "SEO" (survey for economic opportunity), "immigrant"
  (immigrant sample), "latino" (Latino family sample). Defaults to NULL,
  so no subsetting takes place.

- design:

  either character *balanced* or *all* or integer. *balanced* means only
  individuals who appear in each wave are considered. *All* means all
  are taken. An integer value stands for minimum consecutive years of
  participation, i.e. design=3 means present in at least 3 consecutive
  waves.

- loglevel:

  one of INFO, WARN and DEBUG. INFO by default.

- detect_waves:

  logical; if TRUE, read psid_waves_detected.csv from datadir (written
  by getPSID() when it imports waves that are not built in) and use the
  detected variable names for those waves. Built-in waves always win.
  Defaults to FALSE.

- head_code:

  numeric code of "is the head" for detected waves. Defaults to NULL, in
  which case the code of the latest built-in wave is carried over.

## Value

resulting `data.table`. the variable `pid` is the unique person
identifier, constructed from ID1968 and pernum

## Details

build.panel reads FAM\<year\>ER and IND\<year\>ER files (.rda or .RData)
from datadir; obtain them with getPSID(). There is no automatic download
and no credentials. You need to supply the precise names of psid
variables - those variable names vary by year. E.g. *total family
income* will have different names in different waves. The function
[`getNamesPSID`](getNamesPSID.md) greatly helps collecting names for all
waves. With `detect_waves = TRUE`, build.panel also reads
`psid_waves_detected.csv` from `datadir` (written by
[`getPSID()`](getPSID.md) when it imports waves that are not built in);
built-in waves always win, the detected variable names should be checked
against the codebook, and `head_code` is the numeric code of "is the
head" for detected waves (by default carried over from the latest
built-in wave).

## Merging

The variables `interview number` in each family file map to the
`interview number` variable of a given year in the individual file. Run
`example(build.panel)` for a demonstration.

## Supplements

*Notice that support for wealth supplements is disabled!* Recent
releases of the main family file have wealth data included. Earlier
waves must be merged manually, again by variable `interview number` as
above.

## Examples

``` r

# ######################################
# reproducible example on artifical data. 
# run this with example(build.panel).
# ######################################

## make reproducible family data sets for 2 years
## variables are: family income (Money) and age

## Data acquisition step:
## run build.panel with sascii=TRUE

# testPSID creates artifical PSID data
td <- testPSID(N=12,N.attr=0)
fam1985 <- data.table::copy(td$famvars1985)
fam1986 <- data.table::copy(td$famvars1986)
IND2019ER <- data.table::copy(td$IND2019ER)

# create a temporary datadir
my.dir <- tempdir()
#save those in the datadir
# notice different R formats admissible
save(fam1985,file=paste0(my.dir,"/FAM1985ER.rda"))
save(fam1986,file=paste0(my.dir,"/FAM1986ER.RData"))
save(IND2019ER,file=paste0(my.dir,"/IND2019ER.RData"))

## end Data acquisition step.

# now define which famvars
famvars <- data.frame(year=c(1985,1986),
                      money=c("Money85","Money86"),
                      age=c("age85","age86"))

# create ind.vars
indvars <- data.frame(year=c(1985,1986),ind.weight=c("ER30497","ER30534"))

# call the builder
# data will contain column "relation.head" holding the relationship code.

d <- build.panel(datadir=my.dir,fam.vars=famvars,
                 ind.vars=indvars,
                 heads.only=FALSE)  
#> INFO [2026-11-11 10:49:34] found FAM1985ER.rda already downloaded
#> INFO [2026-11-11 10:49:34] found FAM1986ER.rda already downloaded
#> INFO [2026-11-11 10:49:34] everything already downloaded. Build dataset now
#> INFO [2026-11-11 10:49:34] localpsidR: Loading Family data from .rda files
#> INFO [2026-11-11 10:49:34] localpsidR: loaded individual file: /tmp/RtmpyOIcDJ/IND2019ER.RData
#> INFO [2026-11-11 10:49:34] localpsidR: total memory load in MB: 1
#> INFO [2026-11-11 10:49:34] localpsidR: currently working on data for year 1985
#> INFO [2026-11-11 10:49:34] localpsidR: currently working on data for year 1986
#> INFO [2026-11-11 10:49:34] balanced design reduces sample from 36 to 24
#> INFO [2026-11-11 10:49:34] End of build.panel

# see what happens if we drop non-heads
# only the ones who are heads in BOTH years 
# are present (since design='balanced' by default)
d <- build.panel(datadir=my.dir,fam.vars=famvars,
                 ind.vars=indvars,
                 heads.only=TRUE)  
#> INFO [2026-11-11 10:49:34] found FAM1985ER.rda already downloaded
#> INFO [2026-11-11 10:49:34] found FAM1986ER.rda already downloaded
#> INFO [2026-11-11 10:49:34] everything already downloaded. Build dataset now
#> INFO [2026-11-11 10:49:34] localpsidR: Loading Family data from .rda files
#> INFO [2026-11-11 10:49:34] localpsidR: loaded individual file: /tmp/RtmpyOIcDJ/IND2019ER.RData
#> INFO [2026-11-11 10:49:34] localpsidR: total memory load in MB: 1
#> INFO [2026-11-11 10:49:34] localpsidR: currently working on data for year 1985
#> INFO [2026-11-11 10:49:34] dropping non-heads leaves 10 obs
#> INFO [2026-11-11 10:49:34] localpsidR: currently working on data for year 1986
#> INFO [2026-11-11 10:49:34] dropping non-heads leaves 15 obs
#> INFO [2026-11-11 10:49:34] balanced design reduces sample from 20 to 4
#> INFO [2026-11-11 10:49:34] End of build.panel
print(d[order(pid)],nrow=Inf)
#>       money   age interview ID1968 pernum sequence relation.head ind.weight
#>       <num> <int>     <int>  <int>  <int>    <num>         <num>      <num>
#> 1: 78485.75    31         2   1197     14        1            10  0.2986580
#> 2: 78942.81    32         8   1197     14       20            10  0.4353866
#> 3: 16170.57    61        24   8981      1       20            10  0.0959265
#> 4: 16643.49    62        18   8981      1        1            10  0.9068795
#>        pid  year
#>      <num> <num>
#> 1: 1197014  1985
#> 2: 1197014  1986
#> 3: 8981001  1985
#> 4: 8981001  1986

# change sample design to "all": 
# we'll keep individuals if they are head in one year,
# and drop in the other
d <- build.panel(datadir=my.dir,fam.vars=famvars,
                 ind.vars=indvars,heads.only=TRUE,
                 design="all")  
#> INFO [2026-11-11 10:49:34] found FAM1985ER.rda already downloaded
#> INFO [2026-11-11 10:49:34] found FAM1986ER.rda already downloaded
#> INFO [2026-11-11 10:49:34] everything already downloaded. Build dataset now
#> INFO [2026-11-11 10:49:34] localpsidR: Loading Family data from .rda files
#> INFO [2026-11-11 10:49:34] localpsidR: loaded individual file: /tmp/RtmpyOIcDJ/IND2019ER.RData
#> INFO [2026-11-11 10:49:34] localpsidR: total memory load in MB: 1
#> INFO [2026-11-11 10:49:34] localpsidR: currently working on data for year 1985
#> INFO [2026-11-11 10:49:34] dropping non-heads leaves 10 obs
#> INFO [2026-11-11 10:49:34] localpsidR: currently working on data for year 1986
#> INFO [2026-11-11 10:49:34] dropping non-heads leaves 15 obs
#> INFO [2026-11-11 10:49:34] End of build.panel
print(d[order(pid)],nrow=Inf)
#>          money   age interview ID1968 pernum sequence relation.head ind.weight
#>          <num> <int>     <int>  <int>  <int>    <num>         <num>      <num>
#>  1:  56624.775    62        15    485     18        1            10 0.09403037
#>  2:  78485.745    31         2   1197     14        1            10 0.29865803
#>  3:  78942.807    32         8   1197     14       20            10 0.43538664
#>  4:  21502.318    61         1   1431      5        1            10 0.55247234
#>  5:   7093.813    61         4   2111      2       20            10 0.17235189
#>  6:  10547.023    38        12   2508      7        1            10 0.20403065
#>  7:  11519.488    64        24   3197     10        1            10 0.12649262
#>  8:  31686.708    46         7   3559     12       20            10 0.12585627
#>  9: 106183.879    45        11   3927     17        1            10 0.17861309
#> 10:   4156.180    41        19   4903     19        1            10 0.03102602
#> 11:   6638.331    21         3   5517     23       20            10 0.77829279
#> 12:  21908.241    67         7   5536     13        1            10 0.83644625
#> 13:  29156.657    63        17   5898      3        1            10 0.66609201
#> 14:  20408.597    40         2   6791     21        1            10 0.88571080
#> 15:  16549.515    43        23   6871     20       20            10 0.38709628
#> 16: 141303.199    77         5   7070     11        1            10 0.20390068
#> 17:  20482.871    28        10   7555      6       20            10 0.60536844
#> 18:  16170.575    61        24   8981      1       20            10 0.09592650
#> 19:  16643.490    62        18   8981      1        1            10 0.90687946
#> 20:  16313.029    79        22   9047      8        1            10 0.54859144
#>          money   age interview ID1968 pernum sequence relation.head ind.weight
#>          <num> <int>     <int>  <int>  <int>    <num>         <num>      <num>
#>         pid  year
#>       <num> <num>
#>  1:  485018  1986
#>  2: 1197014  1985
#>  3: 1197014  1986
#>  4: 1431005  1986
#>  5: 2111002  1985
#>  6: 2508007  1986
#>  7: 3197010  1986
#>  8: 3559012  1985
#>  9: 3927017  1986
#> 10: 4903019  1986
#> 11: 5517023  1986
#> 12: 5536013  1986
#> 13: 5898003  1986
#> 14: 6791021  1986
#> 15: 6871020  1985
#> 16: 7070011  1986
#> 17: 7555006  1986
#> 18: 8981001  1985
#> 19: 8981001  1986
#> 20: 9047008  1986
#>         pid  year
#>       <num> <num>

file.remove(paste0(my.dir,"/FAM1985ER.rda"),
            paste0(my.dir,"/FAM1986ER.RData"),
            paste0(my.dir,"/IND2019ER.RData"))
#> [1] TRUE TRUE TRUE

# END localpsidR example

# #####################################################################
# Please go to https://github.com/floswald/psidR for more example usage
# #####################################################################
```
