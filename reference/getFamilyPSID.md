# Import locally downloaded PSID family files

Imports family files (prefix `"fam"`) via
[`processPSID`](processPSID.md).

## Usage

``` r
getFamilyPSID(inputdir, outputdir, ...)
```

## Arguments

- inputdir:

  Directory holding the downloaded files.

- outputdir:

  Directory where the imported files are written.

- ...:

  Further arguments passed to [`processPSID`](processPSID.md).

## Value

An invisible data.frame with columns `file`, `status` and `message`.

## See also

[`processPSID`](processPSID.md), [`build.panel`](build.panel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
getFamilyPSID("path/to/downloaded-psid", "path/to/psid-output")
} # }
```
