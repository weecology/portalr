#' @title Get future moon dates
#' @description Get next 12 new moon dates and assign newmoon numbers for forecasting
#' @param moons current newmoonnumber table
#' @param num_future_moons number of future moons to get
#' @return expected moons table for 12 future new moons
#'
#' @export
#'
get_future_moons <- function(moons, num_future_moons = 12) {

  #starting 6 days after latest new moon, to avoid overlap
  most_recent_date <- lubridate::as_date(tail(moons$newmoondate, 1)) + 6

  dates <- lubridate::as_date(most_recent_date:(most_recent_date + num_future_moons * 31))

  futuremoondates <- data.frame(newmoondate = dates, phase = lunar::lunar.phase(dates, name = 8)) %>%
    dplyr::filter(.data$phase == "New") %>%
    dplyr::mutate(group = cumsum(c(1, diff.Date(.data$newmoondate)) > 5)) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarize(newmoondate = median(.data$newmoondate))

  newmoondates <- futuremoondates$newmoondate[seq(num_future_moons)]
  if (length(newmoondates) != num_future_moons) {
    stop('Not enough new moons obtained. Expected ', num_future_moons, ' got ', length(newmoondates))
  }

  newmoons <- data.frame(newmoonnumber = max(moons$newmoonnumber) + seq_along(newmoondates),
                        newmoondate = as.Date(newmoondates),
                        period = NA,
                        censusdate = as.Date(NA))
  return(newmoons)
}
