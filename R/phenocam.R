#' @title Phenocam data products by day, calendar month, or lunar month
#'
#' @description Summarize phenocam data products to either daily, monthly, or lunar monthly level.
#'
#' @param level specify 'monthly', 'daily', or 'newmoon'
#' @inheritParams summarize_rodent_data
#'
#' @export
#'
phenocam <- function(level = "daily", path = get_default_data_path())
{
  level <- tolower(level)

  url <- "http://phenocam.sr.unh.edu/data/archive/portal/ROI/portal_SH_1000_1day.csv"

  got <- tryCatch(GET(url),
                  error = function(e) NULL)
  return_if_null(x = got)


  got <- tryCatch(stop_for_status(got),
                  error = function(e) NULL)
  return_if_null(x = got)


  pheno <- read.csv(url,
                    skip = 22, header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE,
                    colClasses = c("Date", rep("integer", 3), "character",
                                   rep("numeric", 22), rep("character", 5))) %>%
    dplyr::arrange(.data$date)

  if (level == "monthly") {

    ##########Summarize by Month -----------------

    pheno <- pheno %>%
      dplyr::mutate(month = lubridate::month(date)) %>%
      dplyr::group_by(.data$year, .data$month) %>%
      dplyr::summarize(mean_image_count = mean(.data$image_count, na.rm = TRUE),
                       midday_gcc = mean(.data$midday_gcc, na.rm = TRUE),
                       midday_rcc = mean(.data$midday_rcc, na.rm = TRUE),
                       gcc_mean = mean(.data$gcc_mean, na.rm = TRUE),
                       rcc_mean = mean(.data$rcc_mean, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$year, .data$month)

  } else if (level == "newmoon") {

    ##########Summarize by lunar month -----------------
    moon_dates <- load_datafile("Rodents/moon_dates.csv", na.strings = c(""), path = path)
    newmoon_number <- moon_dates$newmoonnumber[-1]
    newmoon_start <- as.Date(moon_dates$newmoondate[-nrow(moon_dates)])
    newmoon_end <- as.Date(moon_dates$newmoondate[-1])
    newmoon_match_number <- NULL
    newmoon_match_date <- NULL
    for (i in seq(newmoon_number)) {
      temp_dates <- as.character(seq.Date(newmoon_start[i] + 1, newmoon_end[i], 1))
      temp_numbers <- rep(newmoon_number[i], length(temp_dates))
      newmoon_match_date <- c(newmoon_match_date, temp_dates)
      newmoon_match_number <- c(newmoon_match_number, temp_numbers)
    }
    newmoon_match_date <- as.Date(newmoon_match_date)
    pheno$newmoonnumber <- newmoon_match_number[match(pheno$date, newmoon_match_date)]

    pheno <- pheno %>%
      dplyr::group_by(.data$newmoonnumber) %>%
      dplyr::summarize(mean_image_count = mean(.data$image_count, na.rm = TRUE),
                       midday_gcc = mean(.data$midday_gcc, na.rm = TRUE),
                       midday_rcc = mean(.data$midday_rcc, na.rm = TRUE),
                       gcc_mean = mean(.data$gcc_mean, na.rm = TRUE),
                       rcc_mean = mean(.data$rcc_mean, na.rm = TRUE)) %>%
      dplyr::arrange(.data$newmoonnumber)
  }
  return(pheno)
}
