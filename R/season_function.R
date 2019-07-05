#' @title Add Seasons
#'
#' @description Higher-order data summaries, by 6-month seasons, 3-month seasons, or year.
#' Also applies specified functions to the specified summary level.
#'
#' @param data data frame containing columns: date, period, newmoonnumber, or year and month
#' @param season_level either year,
#'                       2: winter = Oct-March
#'                          summer = April-Sept
#'                       4: winter = Dec-Feb
#'                          spring = March-May
#'                          summer = Jun-Aug
#'                          fall = Sep-Nov
#' @param date_column either "date" (must be in format "y-m-d"), "period", "newmoonnumber",
#'                    or "yearmon" (data must contain "year" and "month")
#' @param summary_funs A function or list of functions specified by their name (e.g. "mean").
#'                     Default is NA (returned with seasons added but not summarized).
#' @inheritParams summarize_rodent_data
#'
#' @return a data.frame with additional "season" and "year" column, and other columns summarized as specified
#'
#'
#' @export
#'
add_seasons <- function(data, level = "site", season_level = 2,
                        date_column = "yearmon", summary_funs = NA,
                        path = get_default_data_path(),
                        download_if_missing = TRUE, clean = TRUE)
{

  date_column <- tolower(date_column)
  if (!is.na(summary_funs)) {sumfun <- get(summary_funs)}
  grouping <- switch(level,
                     "plot" = c("year", "season", "treatment", "plot"),
                     "treatment" = c("year", "season", "treatment"),
                     "site" = c("year", "season"))
  if("species" %in% colnames(data)) {grouping <- c(grouping, "species")}

  newmoons_table <- load_datafile(file.path("Rodents", "moon_dates.csv"),
                                  na.strings = "NA", path, download_if_missing)
  #### Get month and year column
  if (date_column == "period" || date_column == "newmoonnumber") {

    date_vars <- setdiff(c("newmoonnumber", "newmoondate", "censusdate", "period"),
                         date_column)
    full_data <- data %>%
      dplyr::left_join(newmoons_table, by = date_column)  %>%
      dplyr::mutate(year = lubridate::year(.data$censusdate),
                    month = lubridate::month(.data$censusdate)) %>%
      dplyr::select(-dplyr::one_of(date_vars)) %>%
      dplyr::group_by(.data$year, .data$month)

  } else if (date_column == "date") {
    full_data <- data %>%
      dplyr::mutate(year = lubridate::year(.data$date),
                    month = lubridate::month(.data$date)) %>%
      dplyr::group_by(.data$year, .data$month)

    } else if (date_column == "yearmon") {
    full_data <- data %>%
      dplyr::group_by(.data$year, .data$month)

    } else {
    print("date_column must equal period, newmoonnumber, date, or yearmon")
  }

  #### Add seasons and summarize
  if (season_level == 2 || season_level == 4) {

    if (season_level == 4)
    {
      seasons <- rep(c("winter", "spring", "summer", "fall"), each = 3)
      names(seasons) <- c(12, 1:11)
    } else if (season_level == 2) {
      seasons <- rep(c("winter", "summer"), each = 6)
      names(seasons) <- c(11:12, 1:10)
    }

    full_data$season <- seasons[match(unlist(full_data$month), names(seasons))]

  } else if (season_level == "year" && !(is.na(summary_funs))) {
    grouping <- grouping[-2]
  } else {
    stop("`season_level` must equal 2, 4, or year")
  }

  if (!is.na(summary_funs))
  {
    date_vars <- c("month", "day", "date", "newmoonnumber", "period")
    full_data <- full_data %>% dplyr::group_by_at(grouping) %>%
      dplyr::summarize_all(list(summary_funs), na.rm = TRUE) %>%
      dplyr::select(-.data$month, -dplyr::contains("day"),
                    -dplyr::contains("newmoonnumber"),
                    -dplyr::contains("date"), -dplyr::contains("period"))
  }

  return(full_data)
}

#' @rdname add_seasons
#'
#' @description \code{yearly} generates a table of yearly means
#'
#' @param ... arguments passed to \code{\link{add_seasons}}
#'
#' @examples
#' \donttest{
#' yearly(abundance(path = "repo", time = "newmoon"),
#'        date_column = "newmoonnumber", path = "repo")
#' }
#' @export
#'
yearly <- function(...) {
  add_seasons(..., season_level = "year", summary_funs = "mean")
}
