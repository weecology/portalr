#' @importFrom utils tail

#' @title Get next 12 new moon dates and assign newmoon numbers for forecasting
#' @param moons current newmoonnumber table
#' @param num_future_moons number of future moons to get
#' @return expected moons table for 12 future new moons
#'
#' @export
#'
get_future_moons <- function(moons, num_future_moons=12){
  most_recent_year = as.numeric(format(as.Date(tail(moons$newmoondate,1)), "%Y"))
  most_recent_month = as.numeric(format(as.Date(tail(moons$newmoondate,1)), "%m")) +1
  if (most_recent_month == 13) {
    most_recent_month = 1
    most_recent_year = most_recent_year +1
  }
  newmoondates = htmltab::htmltab(doc=paste("http://aa.usno.navy.mil/cgi-bin/aa_phases.pl?year=",most_recent_year,"&month=",most_recent_month,"&day=1&nump=50&format=t", sep=""),which=1)
  newmoondates = gsub('.{6}$', '', newmoondates$"Date and Time (Universal Time)"[newmoondates$"Moon Phase" == "New Moon"])
  newmoondates = as.Date(newmoondates, format = "%Y %b %d")
  newmoondates = newmoondates[1:num_future_moons]
  if(length(newmoondates)!=num_future_moons){
    stop('Not enough moons obtained. Expected ',num_future_moons,' got ',length(newmoondates))
  }

  newmoons = data.frame(newmoonnumber = max(moons$newmoonnumber)+1:length(newmoondates),
                        newmoondate = as.Date(newmoondates),
                        period = NA,
                        censusdate = as.Date(NA))
  return(newmoons)
}
