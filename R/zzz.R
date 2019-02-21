.onAttach <- function(libname, pkgname) {
  if (interactive() &&
      check_default_data_path(MESSAGE_FUN = packageStartupMessage) &&
      check_for_newer_data())
  {
    packageStartupMessage("The data in the default path `",
                          normalizePath(get_default_data_path()),
                          "` is either missing or out of date.\n",
                          "Consider updating it using `download_observations()`.")
  }
  invisible()
}
