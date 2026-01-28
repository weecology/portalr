# Fill in historic ndvi data to the complete timeseries being fit

Fill in historic ndvi data to the complete timeseries being fit

## Usage

``` r
fill_missing_ndvi(ndvi, level, last_time, moons = NULL)
```

## Arguments

- ndvi:

  ndvi data

- level:

  specify "monthly" or "newmoon"

- last_time:

  the last time step to have been completed

- moons:

  moon data (required if level = "newmoons" and forecasts are needed)

## Value

a data.frame with time and ndvi values

## Details

missing values during the time series are replaced using na.interp,
missing values at the end of the time series are forecast using
auto.arima with seasonality (using Fourier transform)
