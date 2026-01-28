# Download the PortalData repo

Downloads specified version of the Portal data.

## Usage

``` r
download_observations(
  path = get_default_data_path(),
  version = "latest",
  source = "github",
  quiet = FALSE,
  verbose = FALSE,
  pause = 30,
  timeout = getOption("timeout"),
  force = FALSE
)
```

## Arguments

- path:

  `character` Folder into which data will be downloaded.

- version:

  `character` Version of the data to download (default = "latest"). If
  `NULL`, returns.

- source:

  `character` indicator of the source for the download. Either
  `"github"` (default) or `"zenodo"`.

- quiet:

  `logical` whether to download data silently.

- verbose:

  `logical` whether to provide details of downloading.

- pause:

  Positive `integer` or integer `numeric` seconds for pausing during
  steps around unzipping that require time delayment.

- timeout:

  Positive `integer` or integer `numeric` seconds for timeout on
  downloads. Temporarily overrides the `"timeout"` option in
  [`options`](https://rdrr.io/r/base/options.html).

- force:

  `logical` indicator of whether or not existing files or folders (such
  as the archive) should be over-written if an up-to-date copy exists
  (most users should leave as `FALSE`).

## Value

NULL invisibly.
