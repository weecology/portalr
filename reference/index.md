# Package index

## Data Retrieval

Functions to download and update data

- [`download_observations()`](https://weecology.github.io/portalr/reference/download_observations.md)
  : Download the PortalData repo
- [`check_for_newer_data()`](https://weecology.github.io/portalr/reference/check_for_newer_data.md)
  : Check for latest version of data files
- [`check_default_data_path()`](https://weecology.github.io/portalr/reference/use_default_data_path.md)
  [`get_default_data_path()`](https://weecology.github.io/portalr/reference/use_default_data_path.md)
  [`use_default_data_path()`](https://weecology.github.io/portalr/reference/use_default_data_path.md)
  : Manage the default path for downloading Portal Data into
- [`get_dataset_citation()`](https://weecology.github.io/portalr/reference/get_dataset_citation.md)
  : Return Citation for Portal Data

## Data Summaries

Functions to return data summaries

- [`summarize_rodent_data()`](https://weecology.github.io/portalr/reference/summarize_rodent_data.md)
  [`abundance()`](https://weecology.github.io/portalr/reference/summarize_rodent_data.md)
  [`biomass()`](https://weecology.github.io/portalr/reference/summarize_rodent_data.md)
  [`energy()`](https://weecology.github.io/portalr/reference/summarize_rodent_data.md)
  [`rates()`](https://weecology.github.io/portalr/reference/summarize_rodent_data.md)
  [`summarise_rodent_data()`](https://weecology.github.io/portalr/reference/summarize_rodent_data.md)
  : Generate summaries of Portal rodent data
- [`summarize_plant_data()`](https://weecology.github.io/portalr/reference/summarize_plant_data.md)
  [`plant_abundance()`](https://weecology.github.io/portalr/reference/summarize_plant_data.md)
  [`summarise_plant_data()`](https://weecology.github.io/portalr/reference/summarize_plant_data.md)
  : Generate summaries of Portal plant data
- [`summarize_individual_rodents()`](https://weecology.github.io/portalr/reference/summarize_individual_rodents.md)
  [`summarise_individual_rodents()`](https://weecology.github.io/portalr/reference/summarize_individual_rodents.md)
  : Return cleaned Portal rodent individual data
- [`ndvi()`](https://weecology.github.io/portalr/reference/ndvi.md) :
  NDVI by calendar month or lunar month
- [`weather()`](https://weecology.github.io/portalr/reference/weather.md)
  : Weather by day, calendar month, or lunar month
- [`bait_presence_absence()`](https://weecology.github.io/portalr/reference/bait_presence_absence.md)
  : Ant Bait Presence Absence
- [`colony_presence_absence()`](https://weecology.github.io/portalr/reference/colony_presence_absence.md)
  : Ant Colony Presence Absence
- [`shrub_cover()`](https://weecology.github.io/portalr/reference/shrub_cover.md)
  : Generate percent cover from Portal plant transect data
- [`fcast_ndvi()`](https://weecology.github.io/portalr/reference/fcast_ndvi.md)
  : Forecast ndvi using a seasonal auto ARIMA
- [`get_future_newmoons()`](https://weecology.github.io/portalr/reference/get_future_newmoons.md)
  : Get future newmoon dates and numbers
- [`phenocam()`](https://weecology.github.io/portalr/reference/phenocam.md)
  : Phenocam data products by day, calendar month, or lunar month

## Unprocessed Data

Functions to load in raw or otherwise unprocessed data

- [`load_datafile()`](https://weecology.github.io/portalr/reference/load_datafile.md)
  : read in a raw datafile from the downloaded data or the GitHub repo
- [`load_rodent_data()`](https://weecology.github.io/portalr/reference/load_rodent_data.md)
  [`load_plant_data()`](https://weecology.github.io/portalr/reference/load_rodent_data.md)
  [`load_ant_data()`](https://weecology.github.io/portalr/reference/load_rodent_data.md)
  [`load_trapping_data()`](https://weecology.github.io/portalr/reference/load_rodent_data.md)
  : Read in the Portal data files
- [`print(`*`<portal_data_list>`*`)`](https://weecology.github.io/portalr/reference/print.md)
  : Prints a portal_data_list object

## Data Processing

Functions to do various data processing steps

- [`add_seasons()`](https://weecology.github.io/portalr/reference/add_seasons.md)
  [`yearly()`](https://weecology.github.io/portalr/reference/add_seasons.md)
  : Add Seasons
- [`clean_plant_data()`](https://weecology.github.io/portalr/reference/clean_plant_data.md)
  : Do basic cleaning of Portal plant data
- [`clean_rodent_data()`](https://weecology.github.io/portalr/reference/clean_rodent_data.md)
  : Do basic cleaning of Portal rodent data
- [`fill_missing_ndvi()`](https://weecology.github.io/portalr/reference/fill_missing_ndvi.md)
  : Fill in historic ndvi data to the complete timeseries being fit
- [`find_incomplete_censuses()`](https://weecology.github.io/portalr/reference/find_incomplete_censuses.md)
  : Period code for incomplete censuses

## Utilities

Formatting

- [`na_conformer()`](https://weecology.github.io/portalr/reference/na_conformer.md)
  : Conform NA entries to "NA" entries
- [`return_if_null()`](https://weecology.github.io/portalr/reference/return_if_null.md)
  : If a Value is NULL, Trigger the Parent Function's Return
- [`rodent_species()`](https://weecology.github.io/portalr/reference/rodent_species.md)
  [`forecasting_species()`](https://weecology.github.io/portalr/reference/rodent_species.md)
  : Rodent species abbreviations
- [`portalr-package`](https://weecology.github.io/portalr/reference/portalr.md)
  [`portalr`](https://weecology.github.io/portalr/reference/portalr.md)
  : Creates summaries of the Portal data
