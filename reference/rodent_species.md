# Rodent species abbreviations

Creates a simple `character` vector of abbreviations for the Portal
[Rodents](https://portal.naturecast.org/).

## Usage

``` r
rodent_species(
  path = get_default_data_path(),
  type = "code",
  set = "all",
  total = FALSE
)

forecasting_species(
  path = get_default_data_path(),
  total = FALSE,
  type = "abbreviation"
)
```

## Arguments

- path:

  `character` Folder into which data will be downloaded.

- type:

  `character` value indicating the output type. Current options include
  `'abbreviation'` or `'code'` (default, two-letter abbreviation),
  `'g_species'` (abbreviated genus and species), `'Latin'` (full
  scientific names), `'common'` (common names), and `'table'` (a
  `data.frame` of all the options).

- set:

  `character` input of a specified set of species. Options include
  `"all"` (default, all species included) and `"forecasting"` (the
  species used in forecating pipelines).

- total:

  `logical` value indicating if `"total"` should be added or not.

## Value

`character` vector of species abbreviations.
