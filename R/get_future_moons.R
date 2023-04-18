#' @title Get future newmoon dates and numbers
#' @description Get next newmoon dates and assign newmoon numbers for forecasting
#' @param newmoons current newmoon table
#' @param nfuture_newmoons number of future newmoons to get
#' @return expected newmoons table for requested future newmoons
#'
#' @export
#'
get_future_newmoons <- function(newmoons, nfuture_newmoons = NULL) {

  return_if_null(x = nfuture_newmoons)

  #starting 6 days after latest new moon, to avoid overlap
  most_recent_date <- lubridate::as_date(tail(newmoons$newmoondate, 1)) + 6

  dates <- lubridate::as_date(most_recent_date:(most_recent_date + nfuture_newmoons * 31))

  futuremoondates <- data.frame(newmoondate = dates,
                                phase = lunar::lunar.phase(dates, name = 8)) %>%
    dplyr::filter(.data$phase == "New") %>%
    dplyr::mutate(group = cumsum(c(1, diff.Date(.data$newmoondate)) > 5)) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarize(newmoondate = median(.data$newmoondate))

  newmoondates <- futuremoondates$newmoondate[seq(nfuture_newmoons)]
  if (length(newmoondates) != nfuture_newmoons) {
    stop('Not enough new moons obtained. Expected ', nfuture_newmoons, ' got ', length(newmoondates))
  }

  newmoons <- data.frame(newmoonnumber = max(newmoons$newmoonnumber) + seq_along(newmoondates),
                         newmoondate   = as.Date(newmoondates),
                         period        = NA,
                         censusdate    = as.Date(NA))
  return(newmoons)
}
