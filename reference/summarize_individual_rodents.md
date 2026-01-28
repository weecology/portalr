# Return cleaned Portal rodent individual data

This function cleans and subsets the data based on a number of
arguments. It returns stake number and individual level data.

## Usage

``` r
summarize_individual_rodents(
  path = get_default_data_path(),
  clean = TRUE,
  type = "Rodents",
  length = "all",
  unknowns = FALSE,
  time = "period",
  fillweight = FALSE,
  min_plots = 1,
  min_traps = 1,
  download_if_missing = TRUE,
  quiet = FALSE
)

summarise_individual_rodents(
  path = get_default_data_path(),
  clean = TRUE,
  type = "Rodents",
  length = "all",
  unknowns = FALSE,
  time = "period",
  fillweight = FALSE,
  min_plots = 1,
  min_traps = 1,
  download_if_missing = TRUE,
  quiet = FALSE
)
```

## Arguments

- path:

  either the file path that contains the PortalData folder or "repo",
  which then pulls data from the PortalData GitHub repository

- clean:

  logical, load only QA/QC rodent data (TRUE) or all data (FALSE)

- type:

  specify subset of species; either all "Rodents" or only "Granivores"

- length:

  specify subset of plots; use "All" plots or only "Longterm" plots (to
  be deprecated)

- unknowns:

  either removes all individuals not identified to species (unknowns =
  FALSE) or sums them in an additional column (unknowns = TRUE)

- time:

  specify the format of the time index in the output, either "period"
  (sequential Portal surveys), "newmoon" (lunar cycle numbering), "date"
  (calendar date), or "all" (for all time indices)

- fillweight:

  specify whether to fill in unknown weights with other records from
  that individual or species, where possible

- min_plots:

  minimum number of plots within a period for an observation to be
  included

- min_traps:

  minimum number of traps for a plot to be included

- download_if_missing:

  if the specified file path doesn't have the PortalData folder, then
  download it

- quiet:

  logical, whether to run without producing messages

## Value

a data.frame
