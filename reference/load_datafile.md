# read in a raw datafile from the downloaded data or the GitHub repo

does checking for whether a particular datafile exists and then reads it
in, using na_strings to determine what gets converted to NA. It can also
download the dataset if it's missing locally.

## Usage

``` r
load_datafile(
  datafile,
  na.strings = "",
  path = get_default_data_path(),
  download_if_missing = TRUE,
  quiet = TRUE,
  ...
)
```

## Arguments

- datafile:

  the path to the datafile within the folder for Portal data

- na.strings:

  a character vector of strings which are to be interpreted as
  [`NA`](https://rdrr.io/r/base/NA.html) values. Blank fields are also
  considered to be missing values in logical, integer, numeric and
  complex fields. Note that the test happens *after* white space is
  stripped from the input (if enabled), so `na.strings` values may need
  their own white space stripped in advance.

- path:

  either the file path that contains the PortalData folder or "repo",
  which then pulls data from the PortalData GitHub repository

- download_if_missing:

  if the specified file path doesn't have the PortalData folder, then
  download it

- quiet:

  logical, whether to perform operations silently

- ...:

  arguments passed to
  [`download_observations`](https://weecology.github.io/portalr/reference/download_observations.md)
