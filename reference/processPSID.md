# Import locally downloaded PSID files

Imports PSID data files that the user downloaded manually (as zips,
folders, or loose `.txt` + `.sas` pairs) into `.rda` or `.rds` files
named `<PREFIX><year><suffix>.<format>` (for example `FAM2021ER.rda` or
`IND2023ER.rda`). These are the names that
[`build.panel`](build.panel.md) expects.

## Usage

``` r
processPSID(
  inputdir,
  outputdir,
  prefix,
  checkdir = outputdir,
  overwrite = FALSE,
  format = c("rda", "rds"),
  suffix = "ER"
)
```

## Arguments

- inputdir:

  Directory holding the downloaded files.

- outputdir:

  Directory where the imported files are written (created if missing).

- prefix:

  Job prefix, for example `"fam"` or `"ind"`.

- checkdir:

  Directory checked for already-imported files. Defaults to `outputdir`.

- overwrite:

  Logical; re-import jobs whose target file already exists.

- format:

  Either `"rda"` or `"rds"`.

- suffix:

  Name suffix appended after the year.

## Value

An invisible data.frame with columns `file`, `status` (`created`,
`skipped` or `error`) and `message`.

## Details

Output files use the `format` argument; `format = "rds"` files are not
readable by [`build.panel`](build.panel.md) in this version, so use the
default `"rda"` when the files are meant for
[`build.panel`](build.panel.md). Raw PSID files stay wherever the user
keeps them; this package never uploads or transmits them. For `fam` and
`ind` jobs it also writes `psid_waves_detected.csv` (variable names and
years only, no respondent data) into `outputdir` for waves that are not
built in.

## See also

[`build.panel`](build.panel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
processPSID("path/to/downloaded-psid", "path/to/psid-output", "fam")
} # }
```
