% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rodent_species.R
\name{rodent_species}
\alias{rodent_species}
\title{Rodent species abbreviations}
\usage{
rodent_species(
  path = get_default_data_path(),
  type = "code",
  set = "all",
  total = FALSE
)
}
\arguments{
\item{type}{\code{character} value indicating the output type. Current options include \code{'abbreviation'} (default, two-letter abbreviation), \code{'Latin'} (full scientific names), \code{'common'} (common names).}

\item{set}{\code{character} input of a specified set of species. Options include \code{"all"} (default, all species included) and \code{"forecasting"} (the species used in forecating pipelines).}

\item{total}{\code{logical} value indicating if \code{"total"} should be added or not.}
}
\value{
\code{character} vector of species abbreviations.
}
\description{
Creates a simple \code{character} vector of abbreviations for the Portal \href{https://portal.naturecast.org/profiles.html}{Rodents}.
}
\examples{
 rodent_species()
 rodent_species(type = "Latin")
 rodent_species(type = "code")
 rodent_species(type = "common")
 rodent_species(type = "code", total = TRUE)
 rodent_species(set = "forecasting")

}