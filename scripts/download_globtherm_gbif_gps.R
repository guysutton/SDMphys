# Download GBIF records for all GlobTherm species 

# ---------------------------------------------------------
# Session setup
# ---------------------------------------------------------


# Load required packages
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(
  tidyverse,
  rgbif,
  terra,
  data.table,
  tidyr,
  rnaturalearth,
  readr,
  readxl
)

# Change ggplot theme
theme_set(
  theme_classic() +
    theme(
      panel.border = element_rect(colour = "black",
                                  fill = NA),
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(margin = unit(c(2, 0, 0, 0),
                                                "mm")),
      axis.title.y = element_text(margin = unit(c(0, 4, 0, 0),
                                                "mm")),
      legend.position = "none"
    )
)

# Set the theme for the maps
theme_opts <- list(
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = 'white', colour = NA),
    plot.background = element_rect(),
    axis.line = element_blank(),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.title.x = element_text(colour = "black"),
    axis.title.y = element_text(colour = "black"),
    plot.title = element_text(colour = "black"),
    panel.border = element_rect(fill = NA),
    legend.key = element_blank()
  )
)

# ---------------------------------------------------------
# Download GPS records from GBIF
# ---------------------------------------------------------

# The list of species to assess
sp_list <- df

# Match the watchlist names with names in GBIF 
gbif_taxon_keys <- 
  sp_list %>% 
  # dplyr::slice(1:10) %>%                   # Select first 20 species  
  dplyr::pull("scientific_name") %>%         # Get species names  
  rgbif::name_backbone_checklist()  %>%      # Match to backbone
  dplyr::filter(!matchType == "NONE") %>%    # Get matched names
  dplyr::pull(usageKey)                      # Get the GBIF taxonkeys

# Download the records using GBIF API
df_gps <- rgbif::occ_download(
  rgbif::pred_in("taxonKey", gbif_taxon_keys),
  # Request simple CSV (hopefully should reduce required memory)
  format = "SIMPLE_CSV"
)
head(df_gps)

# Check if download is ready
rgbif::occ_download_wait(df_gps) 

# Retrieve download and save on PC 
df_get <- rgbif::occ_download_get(
  key = df_gps, 
  overwrite = TRUE,
  path = here::here("./data/gbif_zip/")
)

# Load data into R 
df_load <- rgbif::occ_download_import(
  df_get,
  quote = " "
)

# ---------------------------------------------------------
# Clean GBIF records 
# ---------------------------------------------------------

# Clean records 
df_clean <- df_load %>%
  dplyr::select(
    order,
    family,
    species,
    scientific_name = scientificName,
    country = countryCode,
    lat = decimalLatitude,
    lon = decimalLongitude
  ) %>%
  # Drop records if either lat or lon are NA
  tidyr::drop_na(lat, lon)
head(df_clean)

# Calculate how many records are available for each species? 
df2_rec <- df_clean %>%
  dplyr::group_by(species, .drop = FALSE) %>%
  dplyr::summarise(
    n_records_in_mu = n()
  )
head(df2_rec)

# Save table of clean GPS records to file 
write_csv(
  df_clean, 
  "./data/data_clean/globtherm_gbif_insecta.csv"
)