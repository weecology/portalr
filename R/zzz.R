.onAttach <- function(libname, pkgname) {
  if (check_for_newer_data(base_folder = "~"))
  {
    packageStartupMessage("The data in the default path `", normalizePath("~"), "` is either missing or out of date.\n",
                          "Consider updating it using `download_observations()`.")
  }

  invisible()
}
