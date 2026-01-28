# Conform NA entries to "NA" entries

Given the species abbreviation *Neotoma albigula* (NA), when data are
read in, there can be an `NA` when it should be an `"NA"`. This function
conforms the entries to be proper character values.

## Usage

``` r
na_conformer(dfv, colname = "species")
```

## Arguments

- dfv:

  Either \[1\] a `data.frame` containing `colname` as a column with
  `NA`s that need to be conformed to `"NA"`s or \[2\] a vector with
  `NA`s that need to be conformed to `"NA"`s.

- colname:

  `character` value of the column name in `tab` to conform the `NA`s to
  `"NA"`s.

## Value

`x` with any `NA` in `colname` replaced with `"NA"`.

## Examples

``` r
 na_conformer(c("a", "b", NA, "c"))
#> [1] "a"  "b"  "NA" "c" 
```
