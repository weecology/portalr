


#' @title Download the PortalData repo
#'
#' @description Downloads specified version of the Portal data.
#'
#' @param path \code{character} Folder into which data will be downloaded.
#'
#' @param version \code{character} Version of the data to download (default = "latest"). If \code{NULL}, returns.
#'
#' @param quiet \code{logical} whether to download data silently.
#'
#' @param verbose \code{logical} whether to provide details of downloading.
#'
#' @param pause Positive \code{integer} or integer \code{numeric} seconds for pausing during steps around unzipping that require time delayment.
#'
#' @param timeout Positive \code{integer} or integer \code{numeric} seconds for timeout on downloads. Temporarily overrides the \code{"timeout"} option in \code{\link[base]{options}}.
#'
#' @param source \code{character} indicator of the source for the download. Either \code{"github"} (default) or \code{"zenodo"}.
#'
#' @param force \code{logical} indicator of whether or not existing files or folders (such as the archive) should be over-written if an up-to-date copy exists (most users should leave as \code{FALSE}).
#'
#' @return NULL invisibly.
#'
#' @export
#'
download_observations <- function (path    = get_default_data_path(),
                                   version = "latest",
                                   source  = "github",
                                   quiet   = FALSE,
                                   verbose = FALSE,
                                   pause   = 30,
                                   timeout = getOption("timeout"),
                                   force   = FALSE) {

  return_if_null(x = version)
  latest_requested <- identical(version, "latest")

  timeout_backup <- getOption("timeout")
  on.exit(options(timeout = timeout_backup))
  options(timeout = timeout)

  if (source == "zenodo") {

    base_url <- "https://zenodo.org/api/records/"

    got <- GET(base_url, query = list(q = "conceptrecid:1215988",
                                      size = 9999,
                                      all_versions = "true"))

    stop_for_status(got, task = paste0("locate Zenodo concept record"))

    contents <- content(got)
    hits=getElement(contents, name = "hits")
    hits=getElement(hits, name = "hits")

    metadata <- lapply(FUN = getElement,
                       X = hits,
                       name = "metadata")
    versions <- sapply(FUN = getElement,
                       X = metadata,
                       name = "version")
    pub_date <- sapply(FUN = getElement,
                       X = metadata,
                       name = "publication_date")

    selected <- ifelse(version == "latest",
                       which.max(as.Date(pub_date)),
                       which(versions == version))

    if (length(selected) == 0){

      stop(paste0("Failed to locate version `", version, "`"))

    }

    zipball_url <- hits[[selected]]$files[[1]]$links$self
    version <- ifelse(version == "latest",
                      metadata[[selected]]$version, version)

  } else if (source == "github") {

    base_url <- "https://api.github.com/repos/weecology/PortalData/releases/"
    url <- ifelse(version == "latest",
                  paste0(base_url, "latest"),
                  paste0(base_url, "tags/", version))

    got <- GET(url)

    stop_for_status(got, task = paste0("locate version `", version, "`"))

    zipball_url <- content(got)$zipball_url

    version <- ifelse(version == "latest", content(got)$name, version)

  } else {

    stop("`source` must be either 'zenodo' or 'github'")

  }


  temp <- file.path(tempdir(), "PortalData.zip")
  final <- file.path(path, "PortalData")
  version_file <- file.path(final, "version.txt")

  if (!force & file.exists(version_file)) {

    existing_version <- scan(file  = version_file,
                             what  = character(),
                             quiet = TRUE)


    if (existing_version == version) {

      # Avoid showing message in test (except if latest version is requested)
      # use rlang::local_interactive() to simulate this message in non-interactive session.
      if (!quiet && (rlang::is_interactive() || latest_requested)) {
    	message("Existing local version is up-to-date with remote version (", version, ") requested and `force` is FALSE, download is skipped")
      }

      return(invisible())

    }

  }
  # Avoid showing message in test (inform of version if latest is requested)
  # use rlang::local_interactive() to simulate this message in non-interactive session.
  if (!quiet && (rlang::is_interactive() || latest_requested)) {
    message("Downloading version `", version, "` of the data...")
  }



  result <- tryCatch(
              expr  = download.file(url      = zipball_url,
                                    destfile = temp,
                                    quiet    = !verbose,
                                    mode     = "wb"),
              error = function(x){NA})

  if (is.na(result)) {

    warning("Archive version `", version, "` could not be downloaded")
    return(invisible( ))

  }


  if (file.exists(final)) {

    old_files <- list.files(path         = final,
                            full.names   = TRUE,
                            all.files    = TRUE,
                            recursive    = TRUE,
                            include.dirs = FALSE)

    file.remove(old_files)

    unlink(x         = final,
           recursive = TRUE)

  }

  temp_unzip <- unzip(temp, list = TRUE)$Name[1]

  unzip(temp, exdir = path)

  Sys.sleep(pause)

  file.remove(temp)
  file.rename(file.path(path, temp_unzip), final)

  invisible()
}

#' @title Check for latest version of data files
#'
#' @description Check the latest version against the data that exists on the GitHub repo
#'
#' @param path Folder in which data will be checked
#'
#' @return bool TRUE if there is a newer version of the data online
#'
#' @export
#'
check_for_newer_data <- function (path = get_default_data_path()) {

  tryCatch(
    path <- file.path(path),
    error = function(e) stop("Unable to locate ", path, call. = FALSE))

  version_file <- file.path(path, "PortalData", "version.txt")

  if (!file.exists(version_file)) {
    return(TRUE)
  }


  url <- "https://api.github.com/repos/weecology/PortalData/releases/latest"
  got <- tryCatch(GET(url),
                  error = function(e) NULL)
  return_if_null(x = got, value = FALSE)

  stop_for_status(got, task = paste0("locate latest GitHub version"))

  github_version_str <- content(got)$name


  pattern <- "([0-9]+)\\.([0-9]+)\\.([0-9]+)"
  version_str <- as.character(read.table(version_file)[1, 1])
  local_version <- c(as.numeric(gsub(pattern, "\\1", version_str)),
                     as.numeric(gsub(pattern, "\\2", version_str)),
                     as.numeric(gsub(pattern, "\\3", version_str)))

  github_version <- c(as.numeric(gsub(pattern, "\\1", github_version_str)),
                      as.numeric(gsub(pattern, "\\2", github_version_str)),
                      as.numeric(gsub(pattern, "\\3", github_version_str)))

  if (github_version[1] > local_version[1])
    return(TRUE)

  if (github_version[1] == local_version[1] &&
      github_version[2] > local_version[2])
    return(TRUE)

  if (github_version[1] == local_version[1] &&
      github_version[2] == local_version[2] &&
      github_version[3] > local_version[3])
    return(TRUE)

  return(FALSE)
}

#' @rdname use_default_data_path
#'
#' @description \code{check_default_data_path} checks if a default data path is
#'   set, and prompts the user to set it if it is missing.
#'
#' @inheritParams use_default_data_path
#' @param MESSAGE_FUN the function to use to output messages
#' @param DATA_NAME the name of the dataset to use in output messages
#' @return FALSE if there is no path set, TRUE otherwise
#'
#' @export
#'
check_default_data_path <- function(ENV_VAR = "PORTALR_DATA_PATH",
                                    MESSAGE_FUN = message, DATA_NAME = "Portal data")
{
  if (is.na(get_default_data_path(fallback = NA, ENV_VAR)))
  {
  	msg <- cli::format_message(
  		c(
  			"You don't appear to have a defined location for storing {DATA_NAME}.",
  			"i" = "Call {.code use_default_data_path(\"path\")} if you wish to set the default data path.",
  			"i" = "{DATA_NAME} will be downloaded into {.path {path.expand('~')}} otherwise."
  		)
  	)
  	MESSAGE_FUN(msg)
    return(FALSE)
  }
  return(TRUE)
}

#' @rdname use_default_data_path
#'
#' @description \code{get_default_data_path} gets the value of the data path
#'   environmental variable
#'
#' @inheritParams use_default_data_path
#' @param fallback the default value to use if the setting is missing
#'
#' @export
#'
get_default_data_path <- function(fallback = "~", ENV_VAR = "PORTALR_DATA_PATH")
{
  Sys.getenv(ENV_VAR, unset = fallback)
}

#' @name use_default_data_path
#' @aliases get_default_data_path
#'
#' @title Manage the default path for downloading Portal Data into
#'
#' @description \code{use_default_data_path} has 3 steps. First, it checks for
#'   the presence of a pre-existing setting for the environmental variable.
#'   Then it checks if the folder exists and creates it, if needed. Then it
#'   provides instructions for setting the environmental variable.
#' @inheritParams download_observations
#' @param ENV_VAR the environmental variable to check (by default
#'   `"PORTALR_DATA_PATH"``)
#'
#' @return None
#'
#' @export
use_default_data_path <- function(path = NULL, ENV_VAR = "PORTALR_DATA_PATH")
{
  # check for prexisting setting
  curr_data_path <- Sys.getenv(ENV_VAR, unset = NA)
  if (!is.na(curr_data_path))
  {
    warning("A default data path exists:", Sys.getenv(ENV_VAR), ".")
  }

  # check if a path is provided
  if (is.null(path))
  {
    stop("Please provide a path to store downloaded data.")
  }

  # check if path is valid
  if (!dir.exists(path))
  {
    dir.create(path)
  }

  # display message and copy new path setting to clipboard
  path_setting_string <- paste0(ENV_VAR, "=", '"', path, '"')
  cli::cli_inform(c(
    "*" = "Call {.run usethis::edit_r_environ()} to open {.val .Renviron}.",
    "*" = "Store your data path with a line like:",
    " " = path_setting_string
  ))
  if (rlang::is_interactive() && clipr::clipr_available()) {
    clipr::write_clip(path_setting_string)
    cli::cli_inform("  [Copied to clipboard]")
  }
  cli::cli_inform(c(
      "*" = "Make sure {.val .Renviron} ends with a newline!"
  ))
  return()
}
