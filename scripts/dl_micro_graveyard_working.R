# Step 1: Split the GPS data by locality 
by_location <- gps_pts %>%
  tidyr::nest(.by = "location")
by_location

# Iterate over each GPS location and extract 'metout' data
# - This returns a list for each location 
data_metout <- by_location %>%
  dplyr::mutate(data = purrr::map(
    .x = data,
    .f = function(my_data) {
      my_data %>%
        dplyr::mutate(micro_data = purrr::map2(
          .x = long,
          .y = lat ,
          .f = function(x, y) {
            NicheMapR::micro_global(
              loc = c(x, y) ,
              timeinterval = 365
            )$metout
                                    }
          ))
          }
    )) %>%
      tidyr::unnest(cols = data) %>%
      dplyr::ungroup() %>%
  tidyr::unnest(cols = micro_data, names_repair = "universal") %>%
  # Repair column names 
  dplyr::rename(
    doy = micro_data[,1]
  )
data_metout










# Iterate over each GPS location and extract 'metout' data
# - This returns a list for each location 
data_metout <- by_location %>%
  dplyr::mutate(data = purrr::map(
    .x = data,
    .f = function(my_data) {
      my_data %>%
        dplyr::mutate(micro_data = purrr::map2(
          .x = long,
          .y = lat ,
          .f = function(x, y) {
              as.data.frame(NicheMapR::micro_global(
                loc = c(x, y) ,
                timeinterval = 365
              )$metout)
            }
          )
        )
      }
    )
  ) %>%
  # Series of steps to unnest the results
  as.data.frame() %>% 
  mutate(data = map(.x = data,
                    .f = ~as.data.frame(.x))) %>%
  as.data.frame() %>%
  tidyr::unnest(col = data) %>%
  as_tibble() %>%
  tidyr::unnest(col = micro_data) %>%
  janitor::clean_names()
data_metout
