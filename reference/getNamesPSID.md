# GetPSID variables names from various years

The user can specify one variable name from any year. This function will
find that variable's correct name in any of the years specified by the
user. If user does not specify the `years` variable, return will
represent all years in which variable was present.

## Usage

``` r
getNamesPSID(aname, cwf, years = NULL, file = NULL)
```

## Arguments

- aname:

  A variable name in any of the PSID years

- cwf:

  A data.frame representation of the cross-walk file, (the psid.xlsx
  file).

- years:

  A vector of years. If NULL, all years in which that variable existed
  are returned. Years with no crosswalk column are dropped with a
  warning.

- file:

  optional file name to write csv

## Value

A vector of names, one for each year.

## Details

This uses the psid.xlsx crosswalk file from UMich, which is available at
http://psidonline.isr.umich.edu/help/xyr/psid.xlsx. In the example, the
package openxlsx's read.xlsx is used to import the crosswalk file.

Ask for one variable at a time.

## Author

Paul Johnson \<pauljohn@ku.edu\> and Florian Oswald

## Examples

``` r
# read UMich crosswalk from installed file
r = system.file(package="localpsidR")
cwf = openxlsx::read.xlsx(file.path(r,"psid-lists","psid.xlsx"))

# or download directly
# cwf <- read.xlsx("http://psidonline.isr.umich.edu/help/xyr/psid.xlsx")

# then get names with
getNamesPSID("ER17013", cwf, years = 2001)
#>    year variable
#> 1 Y2001  ER17013
getNamesPSID("ER17013", cwf, years = 2003)
#>    year variable
#> 1 Y2003  ER21017
getNamesPSID("ER17013", cwf, years = NULL)
#>     year variable
#> 1  Y1968     V117
#> 2  Y1969    V1008
#> 3  Y1970    V1239
#> 4  Y1971    V1942
#> 5  Y1972    V2542
#> 6  Y1973    V3095
#> 7  Y1974    V3508
#> 8  Y1975    V3921
#> 9  Y1976    V4436
#> 10 Y1977    V5350
#> 11 Y1978    V5850
#> 12 Y1979    V6462
#> 13 Y1980    V7067
#> 14 Y1981    V7658
#> 15 Y1982    V8352
#> 16 Y1983    V8961
#> 17 Y1984   V10419
#> 18 Y1985   V11606
#> 19 Y1986   V13011
#> 20 Y1987   V14114
#> 21 Y1988   V15130
#> 22 Y1989   V16631
#> 23 Y1990   V18049
#> 24 Y1991   V19349
#> 25 Y1992   V20651
#> 26 Y1993   V22406
#> 27 Y1994   ER2007
#> 28 Y1995   ER5006
#> 29 Y1996   ER7006
#> 30 Y1997  ER10009
#> 31 Y1999  ER13010
#> 32 Y2001  ER17013
#> 33 Y2003  ER21017
#> 34 Y2005  ER25017
#> 35 Y2007  ER36017
#> 36 Y2009  ER42017
#> 37 Y2011  ER47317
#> 38 Y2013  ER53017
#> 39 Y2015  ER60017
#> 40 Y2017  ER66017
#> 41 Y2019  ER72017
#> 42 Y2021  ER78017
#> 43 Y2023  ER82018
getNamesPSID("ER17013", cwf, years = c(2005, 2007, 2009))
#>    year variable
#> 1 Y2005  ER25017
#> 2 Y2007  ER36017
#> 3 Y2009  ER42017
```
