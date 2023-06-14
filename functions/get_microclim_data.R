# FUNCTION: get_microclim_data
# - Wrapper function to download microclimate data from the 
#   'NicheMapR' R package (Kearney, 2023), which is an implementation
#   of the NicheMapR microclimate model, which uses the 
#   global climate database derived from New et al. (2002). 

# Inputs:
# - data: A data.frame or tibble with three columns, in order:
#        (1) a locality ID column, (2) longitude, and (3)latitude
# - locality_col: Name of the column in {{ data }} containing a string of the 
#                 locality ID 
# - height: Reference height at which to extract meteorological data 
#           (defaults to 1.2m above the ground)

# Load required packages 
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(
  dplyr,
  magrittr,
  purrr,
  NicheMapR
)

# Simulate a dataframe of GPS points
gps_pts <-
  tibble::tribble(
    ~location, ~long, ~lat,
    "Durban", 30.90, -29.84,
    "Wisconsin", -89.40, 43.07
  ) %>%
  dplyr::mutate(location = as.factor(location))
head(gps_pts)

#################
# Define function 
#################

get_microclim_data <- function(data, locality_col, height = 1.2){

  # Step 1: Split the GPS data by locality 
  by_location <- {{ data }} %>%
    tidyr::nest(.by = {{ locality_col }})
  
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
              base::as.data.frame(NicheMapR::micro_global(
                loc = c(x, y) ,
                timeinterval = 365,
                Refhyt = {{ height }}
              )$metout)
            }
          )
        )
      }
    )
    ) %>%
    # Series of steps to unnest the results
    base::as.data.frame() %>% 
    dplyr::mutate(data = purrr::map(
      .x = data,
      .f = ~ base::as.data.frame(.x))
    ) %>%
    base::as.data.frame() %>%
    tidyr::unnest(col = data) %>%
    tibble::as_tibble() %>%
    tidyr::unnest(col = micro_data) %>%
    janitor::clean_names()

} # End function definition 

# Test function
test_foo <- get_microclim_data(
  data = gps_pts,
  locality_col = "location"
)
test_foo



