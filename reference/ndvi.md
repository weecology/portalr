# NDVI by calendar month or lunar month

Summarize NDVI data to monthly or lunar monthly level

## Usage

``` r
ndvi(
  level = "monthly",
  sensor = "landsat",
  fill = FALSE,
  corrected = TRUE,
  forecast = FALSE,
  path = get_default_data_path(),
  download_if_missing = TRUE
)
```

## Arguments

- level:

  specify "monthly" or "newmoon"

- sensor:

  specify "landsat", "modis", "gimms", or "all"

- fill:

  specify if missing data should be filled, passed to
  `fill_missing_ndvi`

- corrected:

  specify if data should be corrected using calibration between landsat
  sensors

- forecast:

  specify ndvi should be forecast from the end of the data to the
  present, passed to `fcast_ndvi`

- path:

  either the file path that contains the PortalData folder or "repo",
  which then pulls data from the PortalData GitHub repository

- download_if_missing:

  if the specified file path doesn't have the PortalData folder, then
  download it
