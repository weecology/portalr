#' @name summarize_individual_rodents
#'
#' @title Return cleaned Portal rodent individual data
#'
#' @description This function cleans and subsets the data based on a number
#'   of arguments. It returns stake number and individual level data.
#'
#' @inheritParams summarize_rodent_data
#'
#' @return a data.frame
#'
#' @export
#'
summarize_individual_rodents <- function(path = get_default_data_path(),
                                         clean = TRUE, type = "Rodents",
                                         length = "all", unknowns = FALSE, time = "period",
                                         fillweight = FALSE, min_plots = 1, min_traps = 1,
                                         download_if_missing = TRUE)
{

  #### Get Data ----
  data_tables <- load_rodent_data(path, download_if_missing = download_if_missing,
                                  clean = clean)

  #### Do initial cleaning ----
  rodents <- clean_rodent_data(data_tables, fillweight, type,
                               unknowns)

  #### Filter by length and add treatment types ----
  trapping <- filter_plots(data_tables$trapping, length)
  rodents <- join_trapping_to_rodents(rodents, trapping, data_tables$trapping,
                                      min_plots, min_traps) %>%
    join_plots(data_tables$plots_table) %>%
    dplyr::select(period, month, day = day.x, year, treatment, plot, stake,
                  species, sex, hfl, wgt, tag, ltag)

  #### use new moon number as time index if time == "newmoon" ----
  return(add_time(rodents, data_tables$newmoons_table, time))
}

#' @rdname summarize_individual_rodents
#' @export
summarise_individual_rodents <- summarize_individual_rodents
