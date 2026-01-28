# Forecast ndvi using a seasonal auto ARIMA

Forecast ndvi using a seasonal auto ARIMA

## Usage

``` r
fcast_ndvi(hist_ndvi, level, lead, moons = NULL)
```

## Arguments

- hist_ndvi:

  historic ndvi data

- level:

  specify "monthly" or "newmoon"

- lead:

  number of steps forward to forecast

- moons:

  moon data (required if level = "newmoon")

## Value

a data.frame with time and ndvi values

## Details

ndvi values are forecast using auto.arima with seasonality (using a
Fourier transform)
