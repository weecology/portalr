# Changelog

## [portalr 0.4.4](https://github.com/weecology/portalr/releases/tag/v0.4.4)

CRAN release: 2025-06-30

*2025-06-25*

- Separate fill argument into fill and forecast arguments in `ndvi`
  function.
- Fix bug in `ndvi` function that returned a tibble, except with
  fill=TRUE which returned a data.frame.
- Add unique id to individual rodents table.
- Fix bug in `na_drop = FALSE` that failed to complete missing rows to
  the treatment level when `time = "newmoon"`.

## [portalr 0.4.3](https://github.com/weecology/portalr/releases/tag/v0.4.3)

CRAN release: 2024-09-23

*2024-09-23*

- [`load_rodent_data()`](https://weecology.github.io/portalr/reference/load_rodent_data.md)
  now returns an object with an S3 class, and provides a useful message
  on `print`.
- Users can now pass arguments to
  [`download_observations()`](https://weecology.github.io/portalr/reference/download_observations.md)
  from
  [`load_rodent_data()`](https://weecology.github.io/portalr/reference/load_rodent_data.md)
  and other calling functions.
- Fix bug in `na_drop = FALSE` that failed to complete missing rows to
  the species level when `time = "newmoon"`.
- Fix bug in
  [`ndvi()`](https://weecology.github.io/portalr/reference/ndvi.md) that
  filtered by sensor only for higher levels.

## [portalr 0.4.2](https://github.com/weecology/portalr/releases/tag/v0.4.2)

CRAN release: 2024-08-28

*2024-08-08*

- `portalr` now uses [testthat 3rd
  edition](https://testthat.r-lib.org/articles/third-edition.html)
- `portalr` no longer imports clisymbols and crayon. Its messages are
  generated with cli.
- `format_todo()`, `format_code()`, and `format_value()` can be replaced
  with [inline markup](https://cli.r-lib.org/articles/usethis-ui.html)
  (i.e. `{.val}`, `{.code}`, etc.) and may be removed from the package.

## [portalr 0.4.1](https://github.com/weecology/portalr/releases/tag/v0.4.1)

CRAN release: 2023-08-23

*2023-08-23*

#### Regional weather filling includes more stations

## [portalr 0.4.0](https://github.com/weecology/portalr/releases/tag/v0.4.0)

CRAN release: 2023-04-21

*2023-04-21*

#### NEWS file included for the first time

#### `download_observations` argument update

- `from_zenodo` replaced with `source`

#### `get_future_moons` replaced with `get_future_newmoons`

- arguments updated from `moons` and `num_future_moons` to `newmoons`
  and `nfuture_newmoons`
- now defaulty for number of moons to add is `NULL` and there is a call
  to `return_if_null` on it

#### addition of new functions (pulled from portalcasting)

- [`return_if_null()`](https://weecology.github.io/portalr/reference/return_if_null.md)
- [`rodent_species()`](https://weecology.github.io/portalr/reference/rodent_species.md):
  helper function that now operates off of the rodent table, not
  hard-coded
- [`na_conformer()`](https://weecology.github.io/portalr/reference/na_conformer.md):
  makes sure the species abbreviation for Neotoma albigula is a
  character value
