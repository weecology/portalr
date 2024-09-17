# portalr
* `load_rodent_data()` now returns an object with an S3 class, and provides a useful message on `print`
* Users can now pass arguments to `download_observations()` from `load_rodent_data()` and other calling functions


Version numbers follow [Semantic Versioning](https://semver.org/).

# [portalr 0.4.2](https://github.com/weecology/portalr/releases/tag/v0.4.2)
*2024-08-08*

* `portalr` now uses [testthat 3rd edition](https://testthat.r-lib.org/articles/third-edition.html)
* `portalr` no longer imports clisymbols and crayon. Its messages are generated with cli.
* `format_todo()`, `format_code()`, and `format_value()` can be replaced with
  [inline markup](https://cli.r-lib.org/articles/usethis-ui.html) (i.e. `{.val}`, `{.code}`, etc.) and
  may be removed from the package.

# [portalr 0.4.1](https://github.com/weecology/portalr/releases/tag/v0.4.1)
*2023-08-23*

### Regional weather filling includes more stations

# [portalr 0.4.0](https://github.com/weecology/portalr/releases/tag/v0.4.0)
*2023-04-21*

### NEWS file included for the first time

### `download_observations` argument update
* `from_zenodo` replaced with `source`

### `get_future_moons` replaced with `get_future_newmoons`
* arguments updated from `moons` and `num_future_moons` to `newmoons` and `nfuture_newmoons`
* now defaulty for number of moons to add is `NULL`  and there is a call to `return_if_null` on it

### addition of new functions (pulled from portalcasting)
* `return_if_null()` 
* `rodent_species()`: helper function that now operates off of the rodent table, not hard-coded
* `na_conformer()`: makes sure the species abbreviation for Neotoma albigula is a character value
