#' @title If a Value is NULL, Trigger the Parent Function's Return
#'
#' @description If the focal input is \code{NULL}, return \code{value} from the parent function. Should only be used within a function.
#'
#' @param x Focal input.
#'
#' @param value If \code{x} is \code{NULL}, \code{\link{return}} this input from the parent function. 
#'
#' @return If \code{x} is not \code{NULL}, \code{NULL} is returned. If \code{x} is \code{NULL}, the result of \code{\link{return}} with \code{value} as its input evaluated within the parent function's environment is returned.
#' 
#' @examples
#'  ff <- function(x = 1, null_return = "hello"){
#'    return_if_null(x, null_return)
#'    x
#'  }
#'  ff()
#'  ff(NULL)
#'
#' @export 
#'
return_if_null <- function (x, value = NULL) {

  if (is.null(x)) {

    do.call(what  = return, 
            args  = list(value), 
            envir = sys.frame(-1))

  } 

}