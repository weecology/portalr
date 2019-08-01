#' @name clean_rodent_data
#'
#' @title Do basic cleaning of Portal rodent data
#'
#' @description This function does basic quality control of the Portal rodent
#'   data. It is mainly called from \code{\link{summarize_rodent_data}}, with
#'   several arguments passed along.
#'
#'   The specific steps it does are, in order:
#'     (1) add in missing weight data
#'     (2) remove records with "bad" period codes or plot numbers
#'     (3) remove records for unidentified species
#'     (4) exclude non-granivores
#'     (5) exclude incomplete trapping sessions
#'     (6) exclude the plots that aren't long-term treatments
#'
#' @param rodent_data the raw rodent data table
#' @param species_table the species table
#' @param fillweight specify whether to fill in unknown weights with other
#'   records from that individual or species, where possible
#' @param type specify subset of species; either all "Rodents" or only
#'   "Granivores"
#' @param unknowns either removes all individuals not identified to species
#'   (unknowns = FALSE) or sums them in an additional column (unknowns = TRUE)
#'
#' @export
#'
clean_rodent_data <- function(rodent_data, species_table,
                              fillweight = FALSE, type = "Rodents",
                              unknowns = FALSE)
{
  rodent_data %>%
    dplyr::left_join(species_table, by = "species") %>%
    fill_weight(fillweight) %>%
    remove_suspect_entries() %>%
    process_unknownsp(unknowns) %>%
    process_granivores(type) %>%
    dplyr::mutate(species = as.factor(.data$species),
                  wgt = as.numeric(.data$wgt),
                  energy = 5.69 * (.data$wgt  ^ 0.75))
}


#' Plot-level rodent data
#'
#' @param rodent_data cleaned rodent data
#' @param trapping_data trapping table with treatment column
#' @inheritParams summarize_rodent_data
#'
#' @return fully crossed period x plot x species flat table of observations
#'   with effort (number of traps) and treatment columns. Any plot not
#'   sufficiently (as defined by min_traps) sampled is returned with NA
#'   for effort and the output value of interest
#'
#' @noRd
#'
make_plot_data <- function(rodent_data, trapping_data, output, min_traps = 1) {

  grouping <- rlang::quos(.data$period, .data$plot, .data$species)
  wt <- switch(output,
               "abundance" = NULL,
               "biomass" = rlang::quo(.data$wgt),
               "energy" = rlang::quo(.data$energy))
  filler <- list(n = as.integer(0))

  rodent_data %>%
    dplyr::count(!!!grouping, wt = !!wt)  %>%
    tidyr::complete(!!!grouping, fill = filler) %>%
    dplyr::right_join(trapping_data, by = c("period", "plot")) %>%
    dplyr::select(c("period", "plot", "species", "n", "effort", "treatment")) %>%
    dplyr::filter(!is.na(.data$species)) %>%
    dplyr::mutate(n = replace(.data$n, .data$effort < min_traps, NA),
                  effort = replace(.data$effort, .data$effort < min_traps, NA)) %>%
    dplyr::rename(!!output := .data$n)
}

#' Rodent data summarized at the relevant level (plot, treatment, site)
#'
#' @param plot_data rodent data summarized at the plot level
#' @param trapping_table trapping table with trap effort per plot per census
#' @inheritParams summarize_rodent_data
#'
#' @return fully crossed flat table of observations with effort (number of
#'   traps and number of plots). The crossing depends on the level:
#'   "plot" is period x treatment x plot x species, "treatment" is
#'   period x treatment x species, and "site" is period x species. Any
#'   observations not sufficiently (as defined by min_plots, and
#'   hierarchically by min_traps) sampled are returned with NA
#'   for ntraps, nplots, and the output value of interest
#'
#' @noRd
make_level_data <- function(plot_data, trapping_table, level, output,
                            min_plots = 1, min_traps = 1)
{
  true_length <- function(x) {
    length(which(!is.na(x)))
  }

  plot_data <- dplyr::rename(plot_data, n := !!output)
  grouping <- switch(level,
                     "plot" = c("period", "treatment", "plot", "species"),
                     "treatment" = c("period", "treatment", "species"),
                     "site" = c("period", "species"))

  if (level == "plot")
  {
    level_data <- plot_data %>%
      dplyr::mutate(n = replace(.data$n, is.na(.data$n), 0L),
                    ntraps = replace(.data$effort, is.na(.data$effort), 0),
                    nplots = ifelse(is.na(.data$effort), 0, 1)) %>%
      dplyr::select(c("period", "treatment", "plot", "species", "n", "ntraps", "nplots"))
  } else {
    level_data <- plot_data %>%
      dplyr::mutate(plot_sampled = as.numeric(!is.na(.data$effort))) %>%
      dplyr::group_by_at(grouping) %>%
      dplyr::summarize(n = sum(.data$n, na.rm = TRUE),
                       ntraps = sum(.data$effort, na.rm = TRUE),
                       nplots = sum(.data$plot_sampled)) %>%
      dplyr::ungroup()
  }

  # set data for incomplete censuses to NA
  incomplete <- find_incomplete_censuses(trapping_table, min_plots, min_traps)

  if (NROW(incomplete) > 0)
  {
    level_data <- level_data %>%
      dplyr::mutate(n = replace(.data$n, .data$period %in% incomplete$period, NA),
                    ntraps = replace(.data$ntraps, .data$period %in% incomplete$period, NA),
                    nplots = replace(.data$nplots, .data$period %in% incomplete$period, NA))
  }

  if (level == "plot")
  {
    level_data <- level_data %>%
      dplyr::mutate(n = replace(.data$n, .data$ntraps < min_traps, NA))
  }

  level_data %>%
    dplyr::rename(!!output := .data$n) %>%
    tibble::as_tibble()
}

#' Rodent data prepared for output
#'
#' @param level_data rodent data summarized at the level of interest
#' @inheritParams summarize_rodent_data
#'
#' @return fully crossed flat table of observations with effort (number of
#'   traps and number of plots). The crossing depends on the level:
#'   "plot" is period x treatment x plot x species, "treatment" is
#'   period x treatment x species, and "site" is period x species. Any
#'   observations not sufficiently (as defined by min_plots, and
#'   hierarchically by min_traps) sampled are returned with NA
#'   for ntraps, nplots, and the output value of interest
#'
#' @noRd
#'
prep_rodent_output <- function(level_data, effort, na_drop,
                               zero_drop, shape, level, output)
{
  species <- na.omit(as.character(unique(level_data$species)))

  if (effort == FALSE) {
    level_data <- dplyr::select(level_data, -.data$nplots, -.data$ntraps)
  } else if (level == "plot") {
    level_data <- dplyr::select(level_data, -.data$nplots)
  }

  if (na_drop) {
    level_data <- na.omit(level_data)
  }

  if (shape == "crosstab") {
    level_data <- make_crosstab(level_data, output, NA)
  }

  if (zero_drop) {
    if (shape == "crosstab") {
      level_data <- level_data %>%
        dplyr::filter(rowSums(dplyr::select_at(., species)) != 0)
    } else { # shape == "flat"
      level_data <- level_data %>%
        dplyr::filter(!!output != 0)
    }
  }

  return(level_data)
}

#' @title Remove suspect trapping periods and unknown plots.
#'
#' @description
#' Removes records with negative period code or
#' with missing plot numbers.
#'
#' @param rodent_data Data.table of raw rodent data.
#'
#' @return Data.table with suspect data removed.
#'
#' @noRd
remove_suspect_entries <- function(rodent_data) {
  rodent_data %>%
    dplyr::filter(.data$period > 0, !is.na(.data$plot))
}

#' @title Processes unknown species.
#'
#' @description
#' Removes any records for unidentified species if unknowns=FALSE.
#' If unknowns=TRUE, then their designation in the output file is
#' given as 'other'.
#'
#' @param rodent_data Data.table with raw rodent data.
#' @param unknowns String. If unknowns=False, unknown species removed.
#'
#' @return Data.table with species info added and unknown species processed
#' according to the argument unknowns.
#'
#' @noRd
process_unknownsp <- function(rodent_data, unknowns) {
  if (unknowns)
  {
    #Rename all unknowns and non-target rodents to "other"
    rodent_species_merge <- rodent_data %>%
      dplyr::filter(.data$rodent == 1) %>%
      dplyr::mutate(species = replace(.data$species, .data$unidentified == 1, "other")) %>%
      dplyr::mutate(species = replace(.data$species, .data$censustarget == 0, "other"))
  } else {
    rodent_species_merge <- rodent_data %>%
      dplyr::filter(.data$rodent == 1, .data$unidentified == 0,
                    .data$censustarget == 1)
  }
  return(rodent_species_merge)
}

#' @title Filters out non-granivores.
#' @description If type=granivores, removes all non-granivore species.
#' @param rodent_species_merge Data table with raw rodent records
#'                             merged with species attributes from
#'                             species_table.
#' @param type String. If type=granivores', non-granivores removed.
#'
#' @return data.table with granivores processed according to argument 'type'.
#'
#' @noRd
process_granivores <- function(rodent_species_merge, type) {
  if (type %in% c("Granivores", "granivores")) {
    granivore_data <- rodent_species_merge %>%
      dplyr::filter(.data$granivore == 1)
    return(granivore_data)
  } else {
    return(rodent_species_merge)
  }
}

#' @title Join rodent and trapping tables
#' @description Joins rodent data with list of trapping dates, by period and plot
#' @param full_trapping Unfiltered data_table of when plots were censused.
#' @inheritParams clean_rodent_data
#' @inheritParams find_incomplete_censuses

#'
#' @return Data.table of raw rodent data with trapping info added.
#'
#' @noRd
join_trapping_to_rodents <- function(rodent_data, trapping_table,
                                     full_trapping, min_plots, min_traps) {

  incomplete_samples <- find_incomplete_censuses(full_trapping, min_plots, min_traps)
  trapping_table <- dplyr::filter(trapping_table, !.data$period %in% incomplete_samples$period)

  dplyr::right_join(rodent_data, trapping_table,
                    by = c("month", "year", "period", "plot"))
}


#' @title Period code for incomplete censuses
#' @description Determines incomplete censuses by finding dates when some plots
#'   were trapped, but others were not.
#' @param trapping_table Data_table of when plots were censused.
#' @inheritParams summarize_rodent_data
#'
#' @return Data.table of period codes when not all plots were trapped.
#'
#' @export
find_incomplete_censuses <- function(trapping_table, min_plots, min_traps) {

  trapping_table %>%
    dplyr::group_by(.data$period) %>%
    dplyr::mutate(sampled = as.numeric(.data$effort >= min_traps)) %>%
    dplyr::summarize(nplots = sum(.data$sampled)) %>%
    dplyr::filter(.data$nplots < min_plots) %>%
    dplyr::select(.data$period)
}

#' @title Fill Weight
#'
#' @description fill in missing weight values with either a recently recorded
#'   weight for that individual or species average
#'
#' @param rodent_data raw rodent data
#' @param tofill logical whether to fill in missing values or not
#'
#' @noRd
fill_weight <- function(rodent_data, tofill)
{
  if (!tofill) return(rodent_data)

  ## [1] filter for missing weight, but known species and tag
  missing_wgt_idx <- (is.na(rodent_data$wgt) | rodent_data$wgt <= 0) &
    (!is.na(rodent_data$species)) &
    (!is.na(rodent_data$tag) & rodent_data$tag != "0" &
       rodent_data$tag != "-1" & rodent_data$tag != "0.00E+00")

  ## [2] substitute from same species and tag and valid weights
  for (this_row in which(missing_wgt_idx))
  {
    rodents_with_wgt <- dplyr::filter(rodent_data,
                                      .data$tag == rodent_data$tag[this_row],
                                      .data$species == rodent_data$species[this_row],
                                      .data$wgt > 0)

    # if there are weights for the same individual
    if (nrow(rodents_with_wgt) > 0) {
      period_dist <- abs(rodent_data$period[this_row] - rodents_with_wgt$period)
      closest_records <- rodents_with_wgt$wgt[which.min(period_dist)]
      rodent_data$wgt[this_row] <- mean(closest_records, na.rm = TRUE)
    }
  }

  ## [3] fill in species weight for all remaining missing weights
  #      (i) see who is still missing weight
  missing_wgt_idx <- is.na(rodent_data$wgt) | rodent_data$wgt <= 0

  #      (ii) see who is a juvenile and species has juvenile weight
  juv_idx <- !is.na(rodent_data$age) & (rodent_data$age == "J") &
    !is.na(rodent_data$juvwgt)

  #      (iii) fill in juvenile weight for known juveniles
  rodent_data$wgt[missing_wgt_idx & juv_idx] <- rodent_data$juvwgt[missing_wgt_idx & juv_idx]

  #      (iv) fill in average weight for everyone else
  rodent_data$wgt[missing_wgt_idx & !juv_idx] <- rodent_data$meanwgt[missing_wgt_idx & !juv_idx]

  #      (v) remove added columns for juvenile and average weight
  rodent_data <- dplyr::select(rodent_data, -.data$juvwgt, -.data$meanwgt)

  #      (vi) if all else fails, convert to 0, so that sums will work correctly
  rodent_data$wgt[is.na(rodent_data$wgt)] <- 0

  return(rodent_data)
}

