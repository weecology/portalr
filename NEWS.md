# portalr

Version numbers follow [Semantic Versioning](https://semver.org/).

# [portalr 0.4.0](https://github.com/weecology/portalcasting/releases/tag/v0.4.0)
*In Progress*

### NEWS file included for the first time

### `download_observations` argument update
* `from_zenodo` replaced with `source`

### `get_future_moons` replaced with `get_future_newmoons`
* arguments updated from `moons` and `num_future_moons` to `newmoons` and `nfuture_newmoons`
* now defaulty for number of moons to add is `NULL`  and there is a call to `return_if_null` on it

### addition of new functions (pulled from portalcasting)
* `return_if_null` 
* `rodent_species`: helper function that now operates off of the rodent table, not hard-coded
