### Function for Adding Seasons to Data ###

add_seasons <- function(data, num_of_seasons){
  # function for adding seasons to data
  if (num_of_seasons == 4) {
    # make seasonal year column
    data$season4_yr = data$year             
    for (n in 1:length(data$species)) {      
      if (data$month[n] %in% c(1,2)) {     
        data$season4_yr[n] = data$year[n] - 1
      }
    }
    # make season column
    data$season4 = 0
    for (i in 1:length(data$species)){
      if (data$month[i] %in% c(1,2,12)){
        data$season4[i] = "winter"
      } else if (data$month[i] %in% c(3,4,5)){
        data$season4[i] = "spring"
      } else if (data$month[i] %in% c(6,7,8)){
        data$season4[i] = "summer"
      } else {
        data$season4[i] = "fall"
      }
    }
  } else if (num_of_seasons == 2) {
    # make seasonal year column
    data$season2_yr = data$year             
    for (i in 1:length(data$species)) {      
      if (data$month[i] %in% c(1,2,3)) {     
        data$season2_yr[i] = data$year[i] - 1
      }
    }
    # make season column
    data$season2 = 0
    for (i in 1:length(data$species)){
      if (data$month[i] %in% c(1,2,3,10,11,12)){
        data$season2[i] = "winter"
      } else {
        data$season2[i] = "summer"
      }
    }
  } else {
    print("num_of_seasons must equal 2 or 4")
  }
  return(data)
}