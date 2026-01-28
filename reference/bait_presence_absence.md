# Ant Bait Presence Absence

Get ant species presence/absence by year/plot/stake from bait census
data

Bait census data is more consistent over time than the colony census
data. This function assumes that all species present in at least one
census were censused in all years.

## Usage

``` r
bait_presence_absence(
  path = get_default_data_path(),
  level = "Site",
  download_if_missing = TRUE,
  quiet = FALSE
)
```

## Arguments

- path:

  either the file path that contains the PortalData folder or "repo",
  which then pulls data from the PortalData GitHub repository

- level:

  level at which to summarize data: 'Site', 'Plot', or 'Stake'

- download_if_missing:

  if the specified file path doesn't have the PortalData folder, then
  download it

- quiet:

  logical, whether to run without version messages

## Value

data frame with year, species, (plot if applicable), and presence \[1,
0\]
