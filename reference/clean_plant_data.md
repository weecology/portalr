# Do basic cleaning of Portal plant data

This function does basic quality control of the Portal plant data. It is
mainly called from
[`summarize_plant_data`](https://weecology.github.io/portalr/reference/summarize_plant_data.md),
with several arguments passed along.

The specific steps it does are, in order: (1) correct species names
according to recent vouchers, if requested (2) restrict species to
annuals or non-woody (3) remove records for unidentified species (5)
exclude the plots that aren't long-term treatments

## Usage

``` r
clean_plant_data(
  data_tables,
  type = "All",
  unknowns = FALSE,
  correct_sp = TRUE
)
```

## Arguments

- data_tables:

  the list of data_tables, returned from calling
  [`load_plant_data`](https://weecology.github.io/portalr/reference/load_rodent_data.md)

- type:

  specify subset of species; If type=Annuals, removes all non-annual
  species. If type=Non-woody, removes shrub and subshrub species If
  type=Perennials, returns all perennial species (includes shrubs and
  subshrubs) If type=Shrubs, returns only shrubs and subshrubs If
  type=Winter-annual, returns all annuals found in winter IF
  type=Summer-annual, returns all annuals found in summer

- unknowns:

  either removes all individuals not identified to species (unknowns =
  FALSE) or sums them in an additional column (unknowns = TRUE)

- correct_sp:

  T/F whether or not to use likely corrected plant IDs, passed to
  `rename_species_plants`
