gps_pts %>%
  tidyr::nest(data = location) %>%
  dplyr::mutate(
    clim_data = purrr::map_df(data, ~ 
                                 NicheMapR::micro_global(
                                   loc = c( "long", "lat" ) , 
                                   timeinterval = 365  # Generate daily data
                                   
                                 ))
  )



data <- by_location %>%
  purrr::map_df(
    ~ NicheMapR::micro_global(
      loc = c( .x["long"], .x["lat"] ) , 
      timeinterval = 365  # Generate daily data
    )
  )







# Split the GPS locality data into groups 
by_location <- split(gps_pts, gps_pts$location)

# Iterate over each GPS location and extract 'metout' data
# - This returns a list for each location 
data_metout <- by_location %>%
  purrr::map(
    ~ NicheMapR::micro_global(
      loc = c( .x["long"], .x["lat"] ) , 
      timeinterval = 365  # Generate daily data
    )$metout
  ) 
data_metout

# Coerce the list into a dataframe 
# 
data_metdf <- data_metout %>%
  purrr::map_df(
    # Stores the listname as a column identifying each location
    .id = "loc", 
    ~ as.data.frame(
      .x
      )
  ) %>%
  # Clean column names 
  janitor::clean_names()
data_metdf

