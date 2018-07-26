context("Check make_timeseries")

data=load_data('.')
rodents=abundance(path='.',level="Site",shape="flat") %>%
  left_join(data$newmoons_table) %>%
  select(period,species,value=abundance,date=censusdate)

timeseries = make_timeseries(rodents)
