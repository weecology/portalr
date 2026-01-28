# Phenocam data products by day, calendar month, or lunar month

Summarize phenocam data products to either daily, monthly, or lunar
monthly level.

## Usage

``` r
phenocam(level = "daily", path = get_default_data_path())
```

## Arguments

- level:

  specify 'monthly', 'daily', or 'newmoon'

- path:

  either the file path that contains the PortalData folder or "repo",
  which then pulls data from the PortalData GitHub repository
