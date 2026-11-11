# Create a test PSID dataset

makes artifical PSID data with variables `age` and `income` for two
consecutive years 1985 and 1986.

## Usage

``` r
testPSID(N = 100, N.attr = 0)
```

## Arguments

- N:

  number of people in each wave

- N.attr:

  number of people lost to attrition

## Value

list with (fake) individual index file IND2009ER and family files for
1985 and 1986
