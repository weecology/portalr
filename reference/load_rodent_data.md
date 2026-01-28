# Read in the Portal data files

Loads Portal data files from either a user-defined path or the online
Github repository. If the user-defined path is un- available, the
default option is to download to that location.

`load_rodent_data` loads the rodent data files

`load_plant_data` loads the plant data files

`load_ant_data` loads the ant data files

`load_trapping_data` loads just the rodent trapping files

## Usage

``` r
load_rodent_data(
  path = get_default_data_path(),
  download_if_missing = TRUE,
  clean = TRUE,
  quiet = FALSE,
  ...
)

load_plant_data(
  path = get_default_data_path(),
  download_if_missing = TRUE,
  quiet = FALSE,
  ...
)

load_ant_data(
  path = get_default_data_path(),
  download_if_missing = TRUE,
  quiet = FALSE,
  ...
)

load_trapping_data(
  path = get_default_data_path(),
  download_if_missing = TRUE,
  clean = TRUE,
  quiet = FALSE,
  ...
)
```

## Arguments

- path:

  either the file path that contains the PortalData folder or "repo",
  which then pulls data from the PortalData GitHub repository

- download_if_missing:

  if the specified file path doesn't have the PortalData folder, then
  download it

- clean:

  logical, load only QA/QC rodent data (TRUE) or all data (FALSE)

- quiet:

  logical, whether to run without version messages

- ...:

  arguments passed to
  [`download_observations`](https://weecology.github.io/portalr/reference/download_observations.md)

## Value

`load_rodent_data` returns a list of 5 dataframes:

|                  |                                            |
|------------------|--------------------------------------------|
| `rodent_data`    | raw data on rodent captures                |
| `species_table`  | species code, names, types                 |
| `trapping_table` | when each plot was trapped                 |
| `newmoons_table` | pairs census periods with newmoons         |
| `plots_table`    | rodent treatment assignments for each plot |

`load_plant_data` returns a list of 7 dataframes:

|                    |                                                                                 |
|--------------------|---------------------------------------------------------------------------------|
| `quadrat_data`     | raw plant quadrat data                                                          |
| `species_table`    | species code, names, types                                                      |
| `census_table`     | indicates whether each quadrat was counted in each census; area of each quadrat |
| `date_table`       | start and end date of each plant census                                         |
| `plots_table`      | rodent treatment assignments for each plot                                      |
| `transect_data`    | raw plant transect data with length and height (2015-present)                   |
| `oldtransect_data` | raw plant transect data as point counts (1989-2009)                             |

`load_ant_data` returns a list of 4 dataframes:

|                 |                                     |
|-----------------|-------------------------------------|
| `bait_data`     | raw ant bait data                   |
| `colony_data`   | raw ant colony data                 |
| `species_table` | species code, names, types          |
| `plots_table`   | treatment assignments for each plot |

`load_trapping_data` returns a list of 2 dataframes:

|                  |                                    |
|------------------|------------------------------------|
| `trapping_table` | when each plot was trapped         |
| `newmoons_table` | pairs census periods with newmoons |
