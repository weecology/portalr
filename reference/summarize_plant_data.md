# Generate summaries of Portal plant data

This function is a generic interface into creating summaries of the
Portal plant species data. It contains a number of arguments to specify
both the kind of data to summarize, at what level of aggregation,
various choices for dealing with data quality, and output format.

`plant_abundance` generates a table of plant abundance

## Usage

``` r
summarize_plant_data(
  path = get_default_data_path(),
  level = "Site",
  type = "All",
  length = "all",
  plots = length,
  unknowns = FALSE,
  correct_sp = TRUE,
  shape = "flat",
  output = "abundance",
  na_drop = switch(tolower(level), quadrat = FALSE, plot = FALSE, treatment = TRUE, site
    = TRUE, TRUE),
  zero_drop = switch(tolower(level), quadrat = TRUE, plot = FALSE, treatment = TRUE, site
    = TRUE, TRUE),
  min_quads = 1,
  effort = TRUE,
  download_if_missing = TRUE,
  quiet = FALSE
)

plant_abundance(..., shape = "flat")

summarise_plant_data(
  path = get_default_data_path(),
  level = "Site",
  type = "All",
  length = "all",
  plots = length,
  unknowns = FALSE,
  correct_sp = TRUE,
  shape = "flat",
  output = "abundance",
  na_drop = switch(tolower(level), quadrat = FALSE, plot = FALSE, treatment = TRUE, site
    = TRUE, TRUE),
  zero_drop = switch(tolower(level), quadrat = TRUE, plot = FALSE, treatment = TRUE, site
    = TRUE, TRUE),
  min_quads = 1,
  effort = TRUE,
  download_if_missing = TRUE,
  quiet = FALSE
)
```

## Arguments

- path:

  either the file path that contains the PortalData folder or "repo",
  which then pulls data from the PortalData GitHub repository

- level:

  summarize by "Plot", "Treatment", "Site", or "Quadrat"

- type:

  specify subset of species; If type=Annuals, removes all non-annual
  species. If type=Summer Annuals, returns all annual species that can
  be found in the summer If type=Winter Annuals, returns all annual
  species that can be found in the winter If type=Non-woody, removes
  shrub and subshrub species If type=Perennials, returns all perennial
  species (includes shrubs and subshrubs) If type=Shrubs, returns only
  shrubs and subshrubs

- length:

  specify subset of plots; use "All" plots or only "Longterm" plots (to
  be deprecated)

- plots:

  specify subset of plots; can be a vector of plots, or specific sets:
  "all" plots or "Longterm" plots (plots that have had the same
  treatment for the entire time series)

- unknowns:

  either removes all individuals not identified to species (unknowns =
  FALSE) or sums them in an additional column (unknowns = TRUE)

- correct_sp:

  correct species names suspected to be incorrect in early data (T/F)

- shape:

  return data as a "crosstab" or "flat" list

- output:

  specify whether to return "abundance", or "cover" \[cover data starts
  in summer 2015\]

- na_drop:

  logical, drop NA values (representing insufficient sampling) filling
  missing combinations of year-month-treatment/plot-species with NA
  could represent one of a few slightly different meanings: 1) that
  combo doesn't exist 2) that combo was skipped that month, or 3) that
  combo was trapped, but is unusable (a negative period code))

- zero_drop:

  logical, drop 0s (representing sufficient sampling, but no detection)

- min_quads:

  numeric \[1:16\], minimum number of quadrats (out of 16) for a plot to
  be included

- effort:

  logical as to whether or not the effort columns should be included in
  the output

- download_if_missing:

  if the specified file path doesn't have the PortalData folder, then
  download it

- quiet:

  logical, whether to run without version messages

- ...:

  arguments passed to `summarize_plant_data`

## Value

a data.frame in either "long" or "wide" format, depending on the value
of \`shape\`
