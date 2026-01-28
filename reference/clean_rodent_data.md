# Do basic cleaning of Portal rodent data

This function does basic quality control of the Portal rodent data. It
is mainly called from
[`summarize_rodent_data`](https://weecology.github.io/portalr/reference/summarize_rodent_data.md),
with several arguments passed along.

The specific steps it does are, in order: (1) add in missing weight data
(2) remove records with "bad" period codes or plot numbers (3) remove
records for unidentified species (4) exclude non-granivores (5) exclude
incomplete trapping sessions (6) exclude the plots that aren't long-term
treatments

## Usage

``` r
clean_rodent_data(
  rodent_data,
  species_table,
  fillweight = FALSE,
  type = "Rodents",
  unknowns = FALSE
)
```

## Arguments

- rodent_data:

  the raw rodent data table

- species_table:

  the species table

- fillweight:

  specify whether to fill in unknown weights with other records from
  that individual or species, where possible

- type:

  specify subset of species; either all "Rodents" or only "Granivores"

- unknowns:

  either removes all individuals not identified to species (unknowns =
  FALSE) or sums them in an additional column (unknowns = TRUE)
