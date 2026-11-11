# Import locally downloaded PSID family and individual files

Imports one or more prefixes (default `"fam"` and `"ind"`) via
[`processPSID`](processPSID.md) and combines the per-prefix results.

## Usage

``` r
getPSID(inputdir, outputdir, prefixes = c("fam", "ind"), ...)
```

## Arguments

- inputdir:

  Directory holding the downloaded files.

- outputdir:

  Directory where the imported files are written.

- prefixes:

  Character vector of prefixes to process.

- ...:

  Further arguments passed to [`processPSID`](processPSID.md).

## Value

An invisible data.frame with columns `file`, `status` and `message`.

## See also

[`processPSID`](processPSID.md), [`build.panel`](build.panel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
getPSID("path/to/downloaded-psid", "path/to/psid-output")
} # }
```
