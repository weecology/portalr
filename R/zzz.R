.onAttach <- function(libname, pkgname) {
  if (interactive() &&
      check_default_data_path(MESSAGE_FUN = packageStartupMessage) &&
      check_for_newer_data())
  {
  	default_path <- normalizePath(get_default_data_path())
  	msg <- cli::format_message(c(
  		"The data in the default path {.path {default_path}} is \\
  		  either missing or out of date.",
  		  i = "Consider updating it using `download_observations()`."
  	))
    packageStartupMessage(msg)
  }
  invisible()
}
