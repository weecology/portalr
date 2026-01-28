# Weather by day, calendar month, or lunar month

Summarize hourly weather data to either daily, monthly, or lunar monthly
level.

## Usage

``` r
weather(
  level = "daily",
  fill = FALSE,
  horizon = 365,
  temperature_limit = 4,
  path = get_default_data_path()
)
```

## Arguments

- level:

  specify 'monthly', 'daily', or 'newmoon'

- fill:

  specify if missing data should be filled, passed to
  `fill_missing_weather`

- horizon:

  Horizon (number of days) to use when calculating cumulative values (eg
  warm weather precip)

- temperature_limit:

  Temperature limit (in C) to use when calculating cumulative values (eg
  warm weather precip)

- path:

  either the file path that contains the PortalData folder or "repo",
  which then pulls data from the PortalData GitHub repository
