#' @title Makes a standardized monthly time series for Portal rodents
#' @description The Portal rodent data is collected roughly monthly.
#' However, some time series methods require regular monthly data with no gaps.
#' This function deals with the issue that some monthly censuses occur slightly
#' before or after their intended sample month. The function takes a univariate
#' time series and pairs samples up with their intended month, averages out
#' double censuses that occur in the same month and interpolates gaps in the data.
#' To work properly the data should be a dataframe containing a single time series where
#' each date contains a single value.
#'
#' @param data Dataframe with columns date (date of the period (e.g. 2016-01-01)), period
#' (period code for the census) and value (numeric value to be analyzed)
#' @param date_format format for the dattes in date column (e.g. "\%Y-\%m-\%d")
#'
#' @return dataframe containing 2 columns: newdate and value
#'
#' @noRd
#'
make_timeseries = function(data, date_format="%Y-%m-%d"){
  # Master function for creating the time series

  data$date = as.Date(data$date, format=date_format)
  data$newdate = as.Date(paste(lubridate::year(data$date),lubridate::month(data$date),"15", sep="-"))

  # How many months between subsequent trapping sessions?
  # Calc_diff determines number of months between periods.
  # if diff > 1, then one or more months is skipped after that session
  # if diff == 0 then the next record is in the same month
  # negative diffs arise when diffs taken across end of year. These are
  # adjusted in calc_diff to represent the actual number of months.

  data = calc_diff(data)

  # Adjusting months:
  #   Sometimes a census is conducted late or early in the monthly
  #   cycle. This results in 2 censuses in the same month and in the time
  #   series this can look like a month was "missed". The next
  #   three calls adjust those censuses to their appropriate month.
  data = shift_current_month(data) # late sample from previous month
  data = shift_next_month(data) # early sample from next month
  data = shift_current_next(data) # cascade - shift current & next back

  # Because shifting months can have cascading effects, the data needs to be
  #  refiltered to catch newly created shift gaps
  data = shift_current_month(data)
  data = shift_next_month(data)
  data = shift_current_next(data)

  # The year 1981 has a long string of censuses that were conducted late in
  # the monthly cycle (i.e. in the next month).
  # They require a specialized fix to adjust censuses back into their place.
  fix_1981 = c(45,46,47)
  for (i in fix_1981){
    data = fix_period(data, i, -1)
  }
  data = calc_diff(data)

  # To interpolate data, need to insert records
  # for the missing months with NA values for abundance
  # for the interpolation to operate on.

  # Insert records where a single month is missing
  diff2s = filter_diffs(data, 2)
  for (period in diff2s){
    data = insert_missing_months(data, period)
  }

  # Insert records where two consecutive months are missing
  diff3s = filter_diffs(data, 3)
  for (period in diff3s){
    data = insert_missing_months(data, period)
  }

  # Rearrange data for interpolation & interpolate
  data = dplyr::arrange(data, date)
  data = calc_diff(data)
#  data$values = zoo::na.approx(data$values) # default is linear interpolation


  # Repeated samples in a month are averaged
  data = data %>% dplyr::group_by(newdate) %>% dplyr::summarise(values = mean(values))
  return(data)
}



#####################################
# SUPPORTING Functions
#####################################

filter_diffs = function(data, diff_value){
  # Returns the period code for months that match
  #   the specified diff_value. Diffs are the number
  #   of months between subsequent censuses
  subset = data %>% dplyr::filter(mo_diff == diff_value) %>% dplyr::select(period)
  return(subset$period)
}

calc_diff = function(data){
  # Calculates the number of months between subsequent censuses
  #   (diff) and adds to data.frame
  diff = diff(lubridate::month(data$newdate))
  mo_diff = append(diff, 99)
  data$mo_diff = mo_diff
  data$mo_diff = ifelse(data$mo_diff ==-11, 1, data$mo_diff)
  data$mo_diff = ifelse(data$mo_diff ==-10, 2, data$mo_diff)
  data$mo_diff = ifelse(data$mo_diff ==-9, 3, data$mo_diff)
  return(data)
}

shift_current_month = function(data){
  # Adjusts month for a diff pattern of:
  #   prev_month = 2, focal_month=0, next_month=1
  #   which matches an example trapping pattern of
  #   Jan trapped, Feb not trapped, Mar trapped twice, Apr trapped
  #   where the early march trapping is just late for February.
  diffs = filter_diffs(data, 0)
  for(i in diffs){
    prev_period = data %>% dplyr::filter(period == i - 1)
    next_period = data %>% dplyr::filter(period == i + 1)
    if (nrow(prev_period) == 0){
      prev_period = data %>% dplyr::filter(period == i - 2)
    }
    if (nrow(next_period) == 0){
      next_period = data %>% dplyr::filter(period == i + 2)
    }
    if(prev_period$mo_diff == 2 && next_period$mo_diff == 1){
      data = fix_period(data, i, -1)
      }
  }
  data = calc_diff(data)
}

shift_next_month = function(data){
  # Adjusts month for a diff pattern of:
  #   prev_month = 1, focal_month=0, next_month=2
  #   which matches an example trapping pattern of
  #   January trapped, February trapped twice, March missed
  #   where the late February census was early for March.
  diffs = filter_diffs(data, 0)
  for(i in diffs){
    prev_period = data %>% dplyr::filter(period == i - 1)
    next_period = data %>% dplyr::filter(period == i + 1)
      if (nrow(prev_period) == 0){
        prev_period = data %>% dplyr::filter(period == i - 2)
      }
      if (nrow(next_period) == 0){
        next_period = data %>% dplyr::filter(period == i + 2)
      }
      if(prev_period$mo_diff == 1 && next_period$mo_diff == 2){
        data = fix_period(data, i+1, 1)
      }
    }
  data = calc_diff(data)
  }


shift_current_next = function(data){
  # Adjusts month for a diff pattern of:
  #   prev_month = 2, focal_month=1, next_month=0
  #   which matches an example trapping pattern of
  #   January trapped, February missed, March trapped, April trapped twice
  #   In this scenario, the February census was conducted in early March and
  #   March census was conducted in early April.
  diffs = filter_diffs(data, 1)
  print(diffs)
  for(i in diffs){
    prev_period = data %>% dplyr::filter(period == i - 1)
    next_period = data %>% dplyr::filter(period == i + 1)
    if (nrow(prev_period) == 0){
      prev_period = data %>% dplyr::filter(period == i - 2)
    }
    if (nrow(next_period) == 0){
      next_period = data %>% dplyr::filter(period == i + 2)
    }
    if(prev_period$mo_diff == 2 && next_period$mo_diff == 0){
      data = fix_period(data, i+1, -1)
      data = fix_period(data, i, -1)
    }
  }
  data = calc_diff(data)
}

fix_period = function(data, i_period, adj){
  # Returns modified monthly date for a period.
  modify = data %>% dplyr::filter(period==i_period) %>%
    dplyr::mutate(newdate = newdate %m+% months(adj))
  data <- data %>% dplyr::mutate(newdate = replace(newdate, period == i_period,
                                            modify$newdate))
}

insert_missing_months = function(data, i_period){
  gap_begin = data %>% dplyr::filter(period == i_period)
  counter = gap_begin$mo_diff-1
  new_records = c()
  for (i in 1:counter){
    new_record = gap_begin %>% dplyr::mutate(newdate = newdate %m+% months(i))
    new_record$values = NA
    new_record$mo_diff = 9
    new_records = rbind(new_records, new_record)
  }
  data = rbind(data, new_records)
}



