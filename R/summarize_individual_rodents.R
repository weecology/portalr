#' @name summarize_individual_rodents
#'
#' @title Portal rodent individual-level data
#'
#' @description Generic interface into creating summaries of the Portal 
#'              rodent data at the individual level. It contains a number of 
#'              arguments to specify the kind of data to summarize (at what 
#'              level of aggregation) and various choices for dealing with 
#'              data quality and output format. 
#'              \cr \cr
#'              \code{summarise_rodent_output} provides alternative spelling. 
#'
#' @inheritParams summarize_rodent_data
#'
#' @return a data.frame
#'
#' @export
#'
summarize_individual_rodents <- function (path = get_default_data_path(),
                                          clean = TRUE, 
                                          level = "Site",
                                          type = "Rodents", 
                                          length = "all", 
                                          plots = length,
                                          unknowns = FALSE, 
                                          time = "period",
                                          fillweight = FALSE, 
                                          min_plots = 1, 
                                          min_traps = 1,
                                          download_if_missing = TRUE, 
                                          quiet = FALSE) {

  if (!missing("length")) {
  
    warning("`length` is deprecated; use `plots` to specify subsets")

  }

  data <- load_rodent_data(path = path, 
                           download_if_missing = download_if_missing,
                           clean = clean, quiet = quiet)

  rodents <- clean_rodent_data(rodent_data = data$rodent_data, 
                               species_table = data$species_table,
                               fillweight = FALSE, type = type,
                               unknowns = unknowns) %>%
             clean_tags(clean = clean, quiet = quiet)


  trapping <- filter_plots(data$trapping, plots = plots) %>%
              join_plots(plots_table = data$plots_table)


#
#  working here
#

  join_trapping_to_rodents(rodents, trapping, data$trapping, min_plots, 
                           min_traps) %>%
  join_plots(data$plots_table) %>%
  dplyr::select(c("id", "period", "month", "day" = "day.x", "year",
                    "treatment", "plot", "stake", "species",
                    "sex", "reprod", "age", "testes", "vagina","pregnant",
                    "nipples","lactation",
                    "hfl", "wgt", "tag", "note2", "ltag", "note3")) %>%
  add_time(rodents, data$newmoons_table, time)
#
# through here
#

}

#' 
#' @rdname summarize_individual_rodents
#' 
#' @export
#' 
summarise_individual_rodents <- function (...) { 
  summarize_individual_rodents(...)
}
