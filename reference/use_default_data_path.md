# Manage the default path for downloading Portal Data into

`check_default_data_path` checks if a default data path is set, and
prompts the user to set it if it is missing.

`get_default_data_path` gets the value of the data path environmental
variable

`use_default_data_path` has 3 steps. First, it checks for the presence
of a pre-existing setting for the environmental variable. Then it checks
if the folder exists and creates it, if needed. Then it provides
instructions for setting the environmental variable.

## Usage

``` r
check_default_data_path(
  ENV_VAR = "PORTALR_DATA_PATH",
  MESSAGE_FUN = message,
  DATA_NAME = "Portal data"
)

get_default_data_path(fallback = "~", ENV_VAR = "PORTALR_DATA_PATH")

use_default_data_path(path = NULL, ENV_VAR = "PORTALR_DATA_PATH")
```

## Arguments

- ENV_VAR:

  the environmental variable to check (by default
  \`"PORTALR_DATA_PATH"“)

- MESSAGE_FUN:

  the function to use to output messages

- DATA_NAME:

  the name of the dataset to use in output messages

- fallback:

  the default value to use if the setting is missing

- path:

  `character` Folder into which data will be downloaded.

## Value

FALSE if there is no path set, TRUE otherwise

None
