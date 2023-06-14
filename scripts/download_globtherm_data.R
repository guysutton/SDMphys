# Load GlobalTherm database into R 
library(traitdata)
library(tidyverse)

# Load database
data(globTherm)
head(globTherm)

# Clean column names 
globTherm <- globTherm %>%
  janitor::clean_names() 

# Subset to keep only records with CTmin and Insecta
# - We have 36 species in total 
df <- globTherm %>% 
  dplyr::select(
    phylum,
    class, 
    order, 
    family, 
    scientific_name = scientific_name_std,
    min_metric,
    tmin,
    n = n_22
  ) %>%
  dplyr::filter(
    min_metric %in% c("ctmin") & 
      class %in% c("Insecta")
    ) %>%
  # Remove rows with no species names 
  tidyr::drop_na(scientific_name)