# default data path functions work if unset

    Code
      check_default_data_path()
    Message
      You don't appear to have a defined location for storing Portal data.
      i Call `use_default_data_path("path")` if you wish to set the default data path.
      i Portal data will be downloaded into '<user_path>' otherwise.
    Output
      [1] FALSE

---

    Code
      use_default_data_path(data_path)
    Message
      * Call `usethis::edit_r_environ()` to open ".Renviron".
      * Store your data path with a line like:
        PORTALR_DATA_PATH="<portalr_path>"
      * Make sure ".Renviron" ends with a newline!
    Output
      NULL

