# Period code for incomplete censuses

Determines incomplete censuses by finding dates when some plots were
trapped, but others were not.

## Usage

``` r
find_incomplete_censuses(trapping_table, min_plots, min_traps)
```

## Arguments

- trapping_table:

  Data_table of when plots were censused.

- min_plots:

  minimum number of plots within a period for an observation to be
  included

- min_traps:

  minimum number of traps for a plot to be included

## Value

Data.table of period codes when not all plots were trapped.
