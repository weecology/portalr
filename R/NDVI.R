#' @importFrom magrittr "%>%"
#' @importFrom lubridate "%m+%"


#' @title NDVI by calendar month or lunar month
#'
#' @description Summarize NDVI data to monthly or lunar monthly level
#'
#' @param level specify "monthly" or "newmoon"
#' @param fill specify if missing data should be filled, passed to
#'   \code{fill_missing_ndvi}
#' @param path specify where to locate Portal data
#'
#' @export
#'
ndvi <- function(level = "monthly", fill = FALSE, path = '~') {
  NDVI <- read.csv(full_path('PortalData/NDVI/monthly_NDVI.csv', path),
                   na.strings = c(""), stringsAsFactors = FALSE)
  moon_dates <- read.csv(full_path('PortalData/Rodents/moon_dates.csv', path),
                         na.strings = c(""), stringsAsFactors = FALSE)

  if (!all(c("year", "month") %in% names(NDVI))) {
    NDVI$month <- lubridate::month(paste0(NDVI$date, "-01"))
    NDVI$year <- lubridate::year(paste0(NDVI$date, "-01"))
    NDVI$ndvi <- NDVI$NDVI
  }
  if (!"date" %in% names(NDVI)) {
    NDVI$date <- as.Date(paste(NDVI$year, NDVI$month, "01", sep = "-"))
  }

  if (level == "monthly") {

    NDVI <- NDVI %>%
      dplyr::group_by(year, month) %>%
      dplyr::summarize(ndvi = mean(ndvi, na.rm = T), date = min(date)) %>%
      dplyr::arrange(date) %>%
      dplyr::ungroup() %>%
      dplyr::select(ndvi, date)
    if (fill == TRUE) {
       curr_yearmonth <- format(Sys.Date(), "%Y-%m")
       last_time <- as.Date(paste(curr_yearmonth, "-01", sep = ""))
       NDVI <- fill_missing_ndvi(NDVI, "monthly", last_time)
    }
  } else if (level == "newmoon") {
    nm_number <- moon_dates$newmoonnumber[-1]
    nm_start <- as.Date(moon_dates$newmoondate[-nrow(moon_dates)])
    nm_end <- as.Date(moon_dates$newmoondate[-1])
    nm_match_number <- NULL
    nm_match_date <- NULL
    for (i in 1:length(nm_number)) {
      temp_dates <- as.character(seq.Date(nm_start[i] + 1, nm_end[i], 1))
      temp_numbers <- rep(nm_number[i], length(temp_dates))
      nm_match_date <- c(nm_match_date, temp_dates)
      nm_match_number <- c(nm_match_number, temp_numbers)
    }
    nm_match_date <- as.Date(nm_match_date)

    NDVI$newmoonnumber <- nm_match_number[match(NDVI$date, nm_match_date)]
    NDVI <- NDVI %>%
      dplyr::group_by(newmoonnumber) %>%
      dplyr::summarize(ndvi = mean(ndvi, na.rm = T)) %>%
      dplyr::arrange(newmoonnumber)

    if (fill == TRUE) {
       today <- Sys.Date()
       prev_time <- moon_dates$newmoonnumber[moon_dates$newmoondate < today]
       last_time <- tail(prev_time, 1)
       NDVI <- fill_missing_ndvi(NDVI, "newmoon", last_time, moon_dates)
    }
  }

  return(NDVI)
}


##############################################################################
#' Fill in historic ndvi data to the complete timeseries being fit
#'
#' @details missing values during the time series are replaced using
#'  na.interp, missing values at the end of the time series are forecast using
#'   auto.arima with seasonality (using Fourier transform)
#'
#' @param ndvi ndvi data
#' @param level specify "monthly" or "newmoon"
#' @param last_time the last time step to have been completed
#' @param moons moon data (required if level = "newmoons" and forecasts are
#'   needed)
#'
#' @return a data.frame with time and ndvi values
#'
#' @export
#'
fill_missing_ndvi <- function(ndvi, level, last_time, moons = NULL)
{
  if (level == "monthly") {
    hist_time_obs <- ndvi$date
    min_hist_time <- min(hist_time_obs)
    max_hist_time <- max(hist_time_obs)
    hist_time <- seq.Date(min_hist_time, max_hist_time , "month")
    hist_ndvi <- data.frame(date = hist_time, ndvi = NA)
    time_match <- match(hist_ndvi$date, ndvi$date)
    hist_ndvi$ndvi <- ndvi$ndvi[time_match]
    ndvi_interp <- forecast::na.interp(hist_ndvi$ndvi)
    hist_ndvi$ndvi <- as.numeric(ndvi_interp)

    if (max_hist_time < last_time) {
      lead_fcast <- length(seq.Date(max_hist_time, last_time , "month")[-1])
      ndvi_fcast <- fcast_ndvi(hist_ndvi, "monthly", lead_fcast)
      hist_ndvi <- rbind(hist_ndvi, ndvi_fcast)
    }
  }
  if (level == "newmoon") {
    hist_time_obs <- ndvi$newmoonnumber
    min_hist_time <- min(hist_time_obs)
    max_hist_time <- max(hist_time_obs)
    hist_time <- min_hist_time:max_hist_time
    hist_ndvi <- data.frame(newmoonnumber = hist_time, ndvi = NA)
    time_match <- match(hist_ndvi$newmoonnumber, ndvi$newmoonnumber)
    hist_ndvi$ndvi <- ndvi$ndvi[time_match]
    ndvi_interp <- forecast::na.interp(hist_ndvi$ndvi)
    hist_ndvi$ndvi <- as.numeric(ndvi_interp)

    if (max_hist_time < last_time) {
      lead_fcast <- length((max_hist_time + 1):last_time)
      ndvi_fcast <- fcast_ndvi(hist_ndvi, "newmoon", lead_fcast, moons)
      hist_ndvi <- rbind(hist_ndvi, ndvi_fcast)
    }
  }

  return(hist_ndvi)
}

############################################################################
#' Forecast ndvi using a seasonal auto ARIMA
#'
#' @details ndvi values are forecast using auto.arima with seasonality (using
#'  a Fourier transform)
#'
#' @param hist_ndvi historic ndvi data
#' @param level specify "monthly" or "newmoon"
#' @param moons moon data (required if level = "newmoon")
#' @param lead number of steps forward to forecast
#'
#' @return a data.frame with time and ndvi values
#'
#' @export
#'
fcast_ndvi <- function(hist_ndvi, level, lead, moons = NULL){

  if (lead == 0) {
    return(hist_ndvi)
  }

  if (level == "monthly") {
    date_fit <- hist_ndvi$date
    last_date <- max(date_fit)
    date_fcast <- last_date %m+% months(1:lead)
    time_to_fcast <- date_fcast
  } else if (level == "newmoon") {
    nm_to_fit <- hist_ndvi$newmoonnumber
    which_nm_fit <- which(moons$newmoonnumber %in% nm_to_fit)
    date_fit <- moons$newmoondate[which_nm_fit]
    date_fit <- as.Date(as.character(date_fit))

    last_nm <- max(nm_to_fit)
    time_to_fcast <- last_nm + 1:lead
    which_nm_fcast <- which(moons$newmoonnumber %in% time_to_fcast)
    if (length(which_nm_fcast) < length(time_to_fcast)) {
      nfuture_nm <- length(time_to_fcast) - length(which_nm_fcast)
      future_nm <- get_future_moons(moons, nfuture_nm)
      moons$newmoondate <- as.character(moons$newmoondate)
      future_nm$newmoondate <- as.character(future_nm$newmoondate)
      moons <- rbind(moons, future_nm)
      which_nm_fcast <- which(moons$newmoonnumber %in% time_to_fcast)
    }
    date_fcast <- moons$newmoondate[which_nm_fcast]
    date_fcast <- as.Date(as.character(date_fcast))
  }

  jday_fit <- as.numeric(format(date_fit, "%j"))
  yr_fit <- format(date_fit, "%Y")
  nye_fit <- as.Date(paste(yr_fit, "-12-31", sep = ""))
  nye_jday_fit <- as.numeric(format(nye_fit, "%j"))
  fr_of_yr_fit <- jday_fit / nye_jday_fit
  cos_fit <- cos(2 * pi * fr_of_yr_fit)
  sin_fit <- sin(2 * pi * fr_of_yr_fit)
  xreg_fit <- data.frame(cos_seas = cos_fit, sin_seas = sin_fit)
  xreg_fit <- as.matrix(xreg_fit)

  jday_fcast <- as.numeric(format(date_fcast, "%j"))
  yr_fcast <- format(date_fcast, "%Y")
  nye_fcast <- as.Date(paste(yr_fcast, "-12-31", sep = ""))
  nye_jday_fcast <- as.numeric(format(nye_fcast, "%j"))
  fr_of_yr_fcast <- jday_fcast / nye_jday_fcast
  cos_fcast <- cos(2 * pi * fr_of_yr_fcast)
  sin_fcast <- sin(2 * pi * fr_of_yr_fcast)
  xreg_fcast <- data.frame(cos_seas = cos_fcast, sin_seas = sin_fcast)
  xreg_fcast <- as.matrix(xreg_fcast)

  mod <- forecast::auto.arima(hist_ndvi$ndvi, xreg = xreg_fit)
  fcast <- forecast::forecast(mod, xreg = xreg_fcast)
  fcast_ndvi <- as.numeric(fcast$mean)

  if (level == "newmoon") {
    ndvi_tab <- data.frame(newmoonnumber = time_to_fcast, ndvi = fcast_ndvi)
  } else if (level == "monthly") {
    ndvi_tab <- data.frame(date = time_to_fcast, ndvi = fcast_ndvi)
  }

  return(ndvi_tab)
}

