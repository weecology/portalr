#' @title Platform-Independent Normalized File Paths
#'
#' @description Combines \code{\link[base]{normalizePath}} and 
#'  \code{\link[base]{file.path}} to produce normalized canonical paths
#'  for files in a platform-independent fashion.
#'
#' @param ...  \code{character} vectors for file paths. 
#'  See \code{\link[base]{file.path}}.
#'
#' @param fsep Path separator to use (assumed to be ASCII).
#'  See \code{\link[base]{file.path}}.
#'
#' @param winslash Separator to be used on Windows - ignored elsewhere. 
#'  See \code{\link[base]{normalizePath}}.
#'
#' @param mustWork \code{logical}: if \code{TRUE} then an error is given if 
#'  the result cannot be determined; if \code{NA}, then a warning.
#'  See \code{\link[base]{normalizePath}}.
#'
#' @return \code{character} vector of the full file paths.
#'  See \code{\link[base]{normalizePath}} and \code{\link[base]{file.path}}.
#'
#' @examples
#'  normalized_file_path("~")
#'  normalized_file_path(".", "folder", "file.ext", mustWork = FALSE)
#'  # compare to 
#'  normalizePath(c(".", "folder", "file.ext"), mustWork = FALSE)
#'  file.path(".", "folder", "file.ext")
#'
#' @export
#'
normalized_file_path <- function (..., 
                                  fsep     = .Platform$file.sep,
                                  winslash = "\\", 
                                  mustWork = NA) {

  normalizePath(path     = file.path(..., 
                                     fsep = fsep), 
                winslash = winslash, 
                mustWork = mustWork)

}