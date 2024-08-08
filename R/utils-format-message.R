#' Format content as a variable value
#'
#' @param ... strings
#' @return a formatted string to output
#'
#' @export
format_value <- function(...)
{
    x <- paste0(..., collapse = "")
    x <- encodeString(x, quote = "'")
    cli::col_blue(x)
}

#' Format content as an action to be performed by the user
#'
#' @param ... strings
#' @return a formatted string to output
#'
#' @export
format_todo <- function(...)
{
    paste0(cli::col_red(cli::symbol$bullet), " ", ..., collapse = "")
}

#' Format content as code
#'
#' @param ... strings
#' @return a formatted string to output
#'
#' @export
format_code <- function(...)
{
    x <- paste0(..., collapse = "")
    x <- encodeString(x, quote = "`")
    cli::col_silver(x)
}
