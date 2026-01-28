# Add Seasons

Higher-order data summaries, by 6-month seasons, 3-month seasons, or
year. Also applies specified functions to the specified summary level.

`yearly` generates a table of yearly means

## Usage

``` r
add_seasons(
  data,
  level = "site",
  season_level = 2,
  date_column = "yearmon",
  summary_funs = NA,
  path = get_default_data_path(),
  download_if_missing = TRUE,
  clean = TRUE
)

yearly(...)
```

## Arguments

- data:

  data frame containing columns: date, period, newmoonnumber, or year
  and month

- level:

  summarize by "Plot", "Treatment", or "Site"

- season_level:

  either year, 2: winter = Oct-March summer = April-Sept 4: winter =
  Dec-Feb spring = March-May summer = Jun-Aug fall = Sep-Nov

- date_column:

  either "date" (must be in format "y-m-d"), "period", "newmoonnumber",
  or "yearmon" (data must contain "year" and "month")

- summary_funs:

  A function specified by its name (e.g. "mean"). Default is NA
  (returned with seasons added but not summarized).

- path:

  either the file path that contains the PortalData folder or "repo",
  which then pulls data from the PortalData GitHub repository

- download_if_missing:

  if the specified file path doesn't have the PortalData folder, then
  download it

- clean:

  logical, load only QA/QC rodent data (TRUE) or all data (FALSE)

- ...:

  arguments passed to `add_seasons`

## Value

a data.frame with additional "season" and "year" column, and other
columns summarized as specified. If no summary function is specified,
"season" and "year" columns are added to original dataframe, as well as
a "seasonyear" column which correctly assigns months to seasons for
grouping (eg December 2000 in winter 2001, rather than winter 2000).
