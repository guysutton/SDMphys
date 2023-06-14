# Script: Apthona nonstriata full workthrough 

# -----------------------------------------------------------------------------
# Session setup
# -----------------------------------------------------------------------------

# Load required packages
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(
  tidyverse,
  dismo,
  raster,
  here,
  corrplot,
  Hmisc,
  patchwork,
  ecospat,
  # kuenm,
  gridSVG,
  gridExtra,
  grid,
  ENMeval,
  spThin,
  viridis,
  viridisLite,
  mapdata,
  maptools,
  scales,
  geosphere,
  rgdal,
  ggtext,
  rJava,
  rgeos,
  sp,
  sf,
  ggspatial,
  ecospat,
  rnaturalearth,
  rnaturalearthdata,
  # megaSDM,
  caret, 
  terra,
  geodata,
  usdm,
  tidyterra
)

# Load GitHub packages
library(InformationValue)

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


# -----------------------------------------------------------------------------
# Importing species GPS data from .xlsx or .csv files
# -----------------------------------------------------------------------------

# Import a .csv file containing GPS data 
sp_gps <- readr::read_csv(
 "./worked_example/data/apthona_nonstriata_gbif.csv"
 ) %>%
  # Clean column names
  janitor::clean_names() %>%
  # Remove any duplicate GPS points 
  dplyr::distinct(lon, lat, .keep_all = TRUE)
head(sp_gps)

# -----------------------------------------------------------------------------
# Validate GPS cleaning analysis
# -----------------------------------------------------------------------------

# Get world map 
world_map <- rnaturalearth::ne_countries(
  scale = "medium", 
  returnclass = "sf"
) 

# Plot GPS points on world map to check our locality data is correct 
ggplot() +
  # Add raster layer of world map 
  geom_sf(data = world_map) +
  # Add GPS points 
  geom_point(
    data = sp_gps, 
    aes(
      x = lon, 
      y = lat
    )
  )  +
  # Set world map CRS 
  coord_sf(
    crs = 4326,
    expand = FALSE
  )

# We are going to build the model with native-range data only
# - Let's subset the data to only keep these records
#   - We can hack this by keep all the records that occur 
#     west of the 60 degree east longitude line 
sp_data <- sp_gps %>%
  dplyr::filter(lon < 60)

# Check that we kept the correct localities 
ggplot() +
  # Add raster layer of world map 
  geom_sf(data = world_map) +
  # Add GPS points 
  geom_point(
    data = sp_data, 
    aes(
      x = lon, 
      y = lat
    )
  )  +
  # Set world map CRS 
  coord_sf(
    crs = 4326,
    expand = FALSE
  )


# -----------------------------------------------------------------------------
# Download climate raster layers 
# -----------------------------------------------------------------------------

# Download the WORLDCLIM raster layers for current time period to your PC
# - This will download and store all 19 WORLDCLIM layers to a folder
#   of your choice (given using 'path = ...' below)
# - Raster layers are stored as 'SpatRaster' so they are compatible with the 
#   'terra' R package 

# -------- Uncomment this code to download WORLDCLIM layers -----------

# # Create a directory to store climate data
# dir.create(
#     "./worked_example/data/environmental_layers/current",
#     recursive = TRUE
# )
# 
# # Download climate layers
# wc_current <- geodata::worldclim_global(
#   var = "bio",
#   res = 2.5,      # Minute degree resolution of raster layers
#   path = here::here("./worked_example/data/environmental_layers/current/"),
#   version = "2.1"
# )

# ----------------------------------------------------------------------

# Load the WORLDCLIM rasters layers we already have downloaded
# - We don't need to run the download code above each new R session
pred_climate <- terra::rast(list.files(
  here::here("./worked_example/data/environmental_layers/current/wc2.1_2.5m/"),
  full.names = TRUE,
  pattern = '.tif'
  )
)

# Plot each of the 19 WORLDCLIM layers to check they imported correctly 
# terra::plot(pred_climate)

# Plot the first layer only (bio1)
# - 'bio1' is mean annual temperature 
terra::plot(pred_climate[[1]])

# Set the CRS projection for the current climate layers 
# - Use the correct wkt CRS format - no more PROJ4 strings! 
terra::crs(pred_climate) <- "epsg:4326"
terra::crs(pred_climate, describe = T)

# -----------------------------------------------------------------------------
# Thin GPS records to only one record per cell 
# -----------------------------------------------------------------------------

# For now, we need to thin the GPS records to keep only 1 GPS point per 
#   raster cell
# - SDM's trained on spatially biased GPS data may misrepresent the relationships 
#   between environmental predictors and species distributions 
# - These models may better capture the structure of spatial sampling biases in the data
#   than species distributions 
# - More on this later...

# Convert one of our environmental predictors into a raster layer 
r <- raster::raster(pred_climate[[1]])

# Extract longitude and latitude into a dataframe
xy <- sp_data %>%
  dplyr::select(
    lon,
    lat
  ) %>%
  # Coerce into a data.frame object (can't be a tibble!!!)
  as.data.frame(.)
head(xy)

# Retain only 1 GPS record per grid cell
set.seed(2012)
sp_data_thin <- dismo::gridSample(
  xy = xy,     # Data.frame containing lon/lat columns only 
  r = r ,      # Environmental raster (must be a raster, not spatRast object)
  n = 1        # Number of records to keep per cell
)

# Let's compare how many GPS points were in the data before and after 
# performing the spatial thinning procedure?
# - We removed 896 GPS points as they occupied the same raster grid cell 
nrow(sp_data)
nrow(sp_data_thin)

# -----------------------------------------------------------------------------
# Extract pseudo-absence points (i.e. background points)
# -----------------------------------------------------------------------------

# Load KG layer
kg_layer <- rgdal::readOGR(
  here::here("./data/shapefiles/koppen_geiger"), 
  "WC05_1975H_Koppen", 
  verbose = FALSE
)

# Re-project KG-layer
geo_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
kg_layer <- sp::spTransform(kg_layer, geo_proj)

# Plot to make sure KG layer imported correctly 
sp::plot(kg_layer)

# Coerce focal taxon GPS records into SpatialPointsDataFrame (SPDF)
records_spatial <- sp::SpatialPointsDataFrame(
  coords = cbind(sp_data_thin$lon, sp_data_thin$lat),
  data = sp_data_thin,
  proj4string = CRS(
    '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  )
)

# Plot to check we can overlay points on KG map
sp::plot(kg_layer)
points(sp_data_thin$lon,
       sp_data_thin$lat,
       pch = 21,
       bg = 'mediumseagreen')

# Select KG ecoregions in which there is at least one GPS record
kg_contain <- kg_layer[records_spatial, ]

# Plot regions containing at least one record
# sp::plot(kg_contain)
sp::plot(
  kg_layer,
  add = TRUE,
  col = 'gray70'
)

# Fill the KG zones with a khaki colour if they contain at least 1 GPS point
sp::plot(
  kg_contain, 
  add = TRUE, 
  col = 'khaki')
# Overlay GPS points 
points(sp_data_thin$lon, 
       sp_data_thin$lat, 
       pch = 21, 
       bg = 'mediumseagreen', 
       cex = 1)

# Define background area by masking climate and topographic raster layers 
# to just the KG zones with at least 1 GPS record
# - First, we have to convert the KG zones containing GPS records 
#   back into an 'sf' object
kg_contain <- sf::st_as_sf(kg_contain)
bg_area <- terra::mask(pred_climate, kg_contain)  

# Plot to check the mask worked
terra::plot(bg_area[[1]])

# Sample random points from the background area defined by the KG zones occupied
# - We use these background points as 'pseudo-absences' to test how well our climate
#   model can distinguish between GPS points occupied by our focal species and these 
#   'pseudo-absence' points (pretty ropey assumption to make that the focal species
#    is absent from these GPS points!!!)
set.seed(2023)
bg_points <- terra::spatSample(
  x = bg_area,                # Raster of background area to sample points from 
  size = nrow(sp_data_thin),  # How many background points do we want?
  method = "random",          # Random points
  replace = FALSE,            # Sample without replacement
  na.rm = TRUE,               # Remove background points that have NA climate data
  as.df = TRUE,               # Return background points as data.frame object
  xy = TRUE,                  # Return lat/lon values for each background point
  exhaustive = TRUE           # Forces the correct sample size to be drawn 
  ) %>%
  # Rename lon and lat columns to be consistent with GPS data for focal species 
  dplyr::rename(
    lon = x,
    lat = y
  )
head(bg_points)

# Check background points have been drawn from the correct geographic mask
terra::plot(kg_layer)
points(bg_points$lon, 
       bg_points$lat, 
       pch = 21, 
       bg = 'mediumseagreen', 
       cex = 1)

# -----------------------------------------------------------------------------
# Reduce multicollinearity in WORLDCLIM rasters 
# -----------------------------------------------------------------------------

# Focal taxon points 
head(sp_data_thin)

# Let's store just the lon and lat columns from our GPS dataset
sp_gps <- sp_data_thin %>%
  dplyr::select(lon, lat)
head(sp_gps)

# Extract climate at these points 
clim_sp <- terra::extract(
  x = pred_climate,         # SpatRast containing climate and topo layers
  y = sp_gps               # SpatVect or data.frame containing GPS of study taxon (lon, lat)
) %>%
  # Remove rows where no climate or topo data is available 
  tidyr::drop_na() %>%
  # Clean the column headers 
  janitor::clean_names() %>%
  # Drop the ID column 
  dplyr::select(-c(id))
head(clim_sp)

# Extract WORLDCLIM values at each species GPS point used to make models
speciesEnv <- base::data.frame(raster::extract(pred_climate,
                                               cbind(sp_gps$lon,
                                                     sp_gps$lat)))

# Must remove NA values first
corData <- speciesEnv %>%
  tidyr::drop_na()

# Assess VIF iteratively 
var_step <- usdm::vifcor(corData, th = 0.7)
var_step

# Subset predictors to the set of uncorrelated predictors identified above 
reduced_pred <- terra::subset(
  # SpatRast containing WORLDCLIM/topo layers 
  x = pred_climate,     
  # Provide names of predictors to keep
  subset = c(                   
    "wc2.1_2.5m_bio_2",
    "wc2.1_2.5m_bio_7",
    "wc2.1_2.5m_bio_8",
    "wc2.1_2.5m_bio_9",
    "wc2.1_2.5m_bio_14",
    "wc2.1_2.5m_bio_15",
    "wc2.1_2.5m_bio_18"
  )
)

# Plot to make sure we kept only the uncorrelated predictors 
terra::plot(reduced_pred)

# -----------------------------------------------------------------------------
# Setup data to run spatial autocorrelation analysis 
# -----------------------------------------------------------------------------

# Extract climate data at focal taxon study points 
gps_climate <- terra::extract(
  x = reduced_pred,     # SpatRast containing reduced clim and topo layers
  y = sp_data_thin,      # SpatVect or data.frame containing lon then lat
  xy = TRUE             # Return lon and lat columns for each GPS point 
)
head(gps_climate)

# -----------------------------------------------------------------------------
# Spatial autocorrelation analysis 
# -----------------------------------------------------------------------------

# At what distance does spatial autocorrelation occur?
# - The x-axis represents meters 
# - The citation for this method:
#   - Legendre, P. and M.J. Fortin. 1989. Spatial pattern and ecological analysis. 
#     Vegetation, 80, 107-138.
set.seed(2012)
spatial_corr <- ecospat::ecospat.mantel.correlogram(
  dfvar = gps_climate,       # Data frame with environmental variables
  colxy = 8:9,               # Columns containing lat/long
  n = 500,                   # Number of random occurrences
  colvar = 1:7,              # Columns containing climate variables
  max = 30000,               # Computes autocorrelation up to 30km (30000m)
  nclass = 30,               # How many points to compute correlation at (n = 30)
  nperm = 100
)

# No evidence for spatial autocorrelation above 1km.  

# Before we spatially thin GPS records, we need to process the data
# - We need to add a column giving the species name and remove the 
#   ID column (which would otherwise be treated as a climate layer)
gps_spatial <- gps_climate %>%
  dplyr::select(-c(ID)) %>%
  dplyr::mutate(species = "A. nonstriata")
head(gps_spatial)

# Thin by spatial autocorrelation value
# - Here, we are going to remove any points that are less than 1km apart 
set.seed(2012)
speciesThinned <- spThin::thin(
  loc.data = gps_spatial,  # Data.frame of lon/lat (and can have species names)
  lat.col = "y",           # Name of latitude column in `loc.data`
  long.col = "x",          # Name of longitude column in `loc.data`
  spec.col = "species",    # Name of species column in `loc.data`
  thin.par = 1,           # Remove points up to 15km apart 
  reps = 100,              # Number of times to repeat thinning (don't edit)
  max.files = 1,           # Number of CSV files to produce (don't edit)
  out.base = "gps_thinned_aph_non",  # Name of .csv file
  out.dir = here::here("./data/gps/spatial_thin/")    # Where to store .csv file
)

# Import the thinned GPS records
sp_gps <-
  readr::read_csv(
    here::here(
      "./data/gps/spatial_thin/gps_thinned_aph_non_thin1.csv"
    )
  )
head(sp_gps)

# How many records were removed?
# - No records were removed 
nrow(gps_spatial)   # No. of records before spatial thinning 
nrow(sp_gps)        # No. of records after spatial thinning 


# -----------------------------------------------------------------------------
# Setup data to run model tuning experiments 
# -----------------------------------------------------------------------------

# We need a data.frame of the lon and lat (in that order) for our 
# focal taxon's GPS records
sp_gps <- sp_gps %>%
  dplyr::select(
    lon = x,
    lat = y
  )
head(sp_gps)

# We need a 'RasterStack' containing our reduced set of environmental predictors 
# - We already have this data stored in 'reduced_pred'
# terra::plot(reduced_pred)

# Crop predictors to the study area 
e <- ext(-10, 150, 35, 70)
rc <- terra::crop(reduced_pred, e)

# We also need a data.frame of the lon and lat (in that order) for our background points 
bg_pts <- bg_points %>%
  dplyr::select(
    lon, 
    lat
  )
head(bg_pts)

# We need a list of the feature class (fc) and regularisation multipliers (rm) to test
list_settings <- list(
  fc = c("L","Q", "H", "LQH"),   
  rm = 1:8          
) 

# -----------------------------------------------------------------------------
# Run model tuning experiments 
# -----------------------------------------------------------------------------

# Set reproducible seed
set.seed(2023)

# Run model tuning 
tuning_results <- 
  ENMeval::ENMevaluate(
    occs = sp_gps,             # Data.frame with only lon THEN lat columns
    envs = rc,                 # 'spatRaster' of reduced clim and topo layers
    bg = bg_pts,               # Data.frame with lon then lat then bg predictors
    tune.args = list_settings, # Named list of FC and RM values to test 
    partitions = "block",      # Specify spatial vs random cross-validation 
    algorithm = "maxent.jar",  # Don't change!!! 
    doClamp = FALSE            # Don't change - we need to extrapolate!!! 
  )

# Store the model tuning results in a dataframe
tuning_df <- ENMeval::eval.results(tuning_results)
tuning_df

# Save model tuning results to PC
readr::write_csv(
  x = tuning_df,
  file = here::here("./worked_example/model/model_tuning/model_tuning_results.csv")
)

# Import the saved model tuning results
tuning_results <- readr::read_csv(
  here::here("./worked_example/model/model_tuning/model_tuning_results.csv")
)
head(tuning_results)

# Plot the model tuning results
ENMeval::evalplot.stats(
  e = tuning_results,      # Variable containing ENMevaluate results 
  stats = c(               # Which metrics to plot?
    "auc.val",             # Make a plot for AUC
    "or.mtp",              # Make a plot for minimum training presence (MTP)
    "or.10p"               # Make a plot for 10th percentile omission rate (OR10)
  ),   
  color = "fc",            # Colours lines/bars by feature class (fc)
  x.var = "rm",            # Variable to plot on x-axis
  error.bars = FALSE       # Don't show error bars 
)


# -----------------------------------------------------------------------------
# Setup data to fit MaxEnt models 
# -----------------------------------------------------------------------------

# We need a data.frame with columns containing climate and topo data for 
# the study species GPS points 
clim_species <- terra::extract(
  x = reduced_pred,     # SpatRast containing reduced clim and topo layers
  y = sp_gps,           # SpatVect or data.frame containing GPS of presence points (lon, lat)
  xy = FALSE            # Don't need lon and lat columns for each GPS point 
)
head(clim_species)

# We need a data.frame with columns containing climate data for each background point
clim_bg <- terra::extract(
  x = reduced_pred,          # SpatRast containing reduced clim and topo layers
  y = bg_pts,                # SpatVect or data.frame containing GPS of background points (lon, lat)
  xy = FALSE                 # Don't need lon and lat columns for each GPS point 
)
head(clim_bg)

# Combine the climate data for the focal species and background points 
data <- dplyr::bind_rows(
  clim_species,
  clim_bg
) %>%
  dplyr::select(-c(ID))
head(data)

# Provide a vector containing 0 (indicating background points) and 1 (indicating 
# presence points)
p_vector <- c(
  replicate(nrow(clim_species), "1"),
  replicate(nrow(clim_bg), "0")
) 

# -----------------------------------------------------------------------------
# Fit MaxEnt model 
# -----------------------------------------------------------------------------

# Fit MaxEnt model with optimal model settings configurations 
mod1 <- dismo::maxent(
  x = data,
  p = p_vector,
  path = here::here("./worked_example/model/model_fit/native_model"),
  replicates = 10,
  args = c(
    # Insert the optimal RM value here
    'betamultiplier=1.0',
    # Turn these on/off to change FC combinations 
    # - To only use quadratic features, turn all to false except quadratic
    'linear=true',
    'quadratic=true',
    'product=true',
    'threshold=true',
    'hinge=true',
    # Don't change anything from here down 
    'threads=2',
    #'doclamp=true',
    'fadebyclamping=true',
    'responsecurves=true',
    'jackknife=true',
    'askoverwrite=false',
    'responsecurves=true',
    'writemess=true',
    'writeplotdata=true',
    'writebackgroundpredictions=true'
  )
)


# -----------------------------------------------------------------------------
# Plot map - Australia 
# -----------------------------------------------------------------------------

# Get map to project our model over
wld_ext <- rnaturalearth::ne_countries(scale = "medium",
                                       returnclass = "sf") %>%
  dplyr::filter(admin %in% c("Australia"))

# Mask reduced set of WORLDCLIM layers to the extent of map
wld_map <- terra::mask(reduced_pred, wld_ext)
terra::plot(wld_map)

# Set the CRS projection
terra::crs(wld_map) <- "epsg:4326"
terra::crs(wld_map, describe = T)

# Extract MaxEnt predictions 
predict_maxent <- terra::predict(mod1, wld_map)
# terra::plot(predict_maxent)

# Set the CRS projection
terra::crs(predict_maxent) <- "epsg:4326"
terra::crs(predict_maxent, describe = T)

# Save raster 
f <- file.path(
  here::here("./worked_example/model/model_fit/native_model/"), 
  "raster_pred_aus.tif"
)
terra::writeRaster(
  predict_maxent, 
  f, 
  overwrite = TRUE
)

# Plot map  
plot_aus <- ggplot() +
  # Plot world boundary
  geom_sf(data = wld_ext, fill = NA) +
  # Plot MaxEnt prediction raster
  geom_spatraster(
    data = predict_maxent,
    maxcell = 5e+6         # maxcell = Inf
  ) +
  geom_sf(data = wld_ext, fill = NA) +
  # geom_point(data = sp_data_inv, 
  #           aes(x = lon, y = lat)) + 
  # Control raster colour and legend
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Suitability"
  ) +
  # Crops map to just the geographic extent of Australia
  coord_sf(
    xlim = c(110, 155),
    ylim = c(-10, -45),
    crs = 4326,
    expand = FALSE
  ) +
  # Create title for the legend
  theme(legend.position = "right") +  # Inside box c(0.2, 0.2)
  # Add scale bar to bottom-right of map
  annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.15
  ) +
  # Add north arrow
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.03, "in"),
    pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  )
plot_aus

# Save figure to PC
ggsave(
  "./worked_example/model/model_fit/native_model/figure_raster_pred_aus.png",
  dpi = 600,
  height = 6,
  width = 6
)







# -----------------------------------------------------------------------------
# Model validation - Australia 
# -----------------------------------------------------------------------------

# Import raw GPS data
sp_gps_val <- 
  # Import a .csv file containing GPS data 
  readr::read_csv(
    "./worked_example/data/apthona_nonstriata_gbif.csv"
  ) %>%
  # Clean column names
  janitor::clean_names() %>%
  # Remove any duplicate GPS points 
  dplyr::distinct(lon, lat, .keep_all = TRUE) %>%
  dplyr::filter(lon > 60)
head(sp_gps_val)

# Get world map 
world_map <- rnaturalearth::ne_countries(
  scale = "medium", 
  returnclass = "sf"
) 

# Plot GPS points on world map to check our locality data is correct 
ggplot() +
  # Add raster layer of world map 
  geom_sf(data = world_map) +
  # Add GPS points 
  geom_point(
    data = sp_gps_val, 
    aes(
      x = lon, 
      y = lat
    )
  )  +
  # Set world map CRS 
  coord_sf(
    crs = 4326,
    expand = FALSE
  )

# Coerce focal taxon GPS records into SpatialPointsDataFrame (SPDF)
records_spatial <- sp::SpatialPointsDataFrame(
  coords = cbind(sp_gps_val$lon, sp_gps_val$lat),
  data = sp_gps_val,
  proj4string = CRS(
    '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  )
)

# Select KG ecoregions in which there is at least one GPS record
kg_contain <- kg_layer[records_spatial, ]

# Define background area by masking climate and topographic raster layers 
# to just the KG zones with at least 1 GPS record
# - First, we have to convert the KG zones containing GPS records 
#   back into an 'sf' object
kg_contain <- sf::st_as_sf(kg_contain)
bg_area <- terra::mask(pred_climate, kg_contain)  

# Plot to check the mask worked
terra::plot(bg_area[[1]])

# Sample random points from the background area defined by the KG zones occupied
# - We use these background points as 'pseudo-absences' to test how well our climate
#   model can distinguish between GPS points occupied by our focal species and these 
#   'pseudo-absence' points (pretty ropey assumption to make that the focal species
#    is absent from these GPS points!!!)
set.seed(2023)
bg_points <- terra::spatSample(
  x = bg_area,                # Raster of background area to sample points from 
  size = nrow(sp_gps_val),    # How many background points do we want?
  method = "random",          # Random points
  replace = FALSE,            # Sample without replacement
  na.rm = TRUE,               # Remove background points that have NA climate data
  as.df = TRUE,               # Return background points as data.frame object
  xy = TRUE,                  # Return lat/lon values for each background point
  exhaustive = TRUE
) %>%
  # Rename lon and lat columns to be consistent with GPS data for focal species 
  dplyr::rename(
    lon = x,
    lat = y
  )
head(bg_points)

# Check background points have been drawn from the correct geographic mask
terra::plot(kg_layer)
points(bg_points$lon, 
       bg_points$lat, 
       pch = 21, 
       bg = 'mediumseagreen', 
       cex = 1)

# Clean background points 
bg_gps <- bg_points %>%
  dplyr::mutate(
    presence = "0"
  ) %>%
  dplyr::select(
    lon,
    lat,
    bio2 = wc2.1_2.5m_bio_2,
    bio7 = wc2.1_2.5m_bio_7,
    bio8 = wc2.1_2.5m_bio_8,
    bio9 = wc2.1_2.5m_bio_9,
    bio14 = wc2.1_2.5m_bio_14,
    bio15 = wc2.1_2.5m_bio_15,
    bio18 = wc2.1_2.5m_bio_18,
    presence
  )
head(bg_gps)

# Let's store just the lon and lat columns from our GPS dataset
sp_gps <- sp_gps_val %>%
  dplyr::select(lon, lat)
head(sp_gps)

# Extract climate at these points 
clim_sp <- terra::extract(
  x = pred_climate,        # SpatRast containing climate and topo layers
  y = sp_gps,              # SpatVect or data.frame containing GPS of study taxon (lon, lat),
  xy = TRUE
) %>%
  # Remove rows where no climate or topo data is available 
  tidyr::drop_na() %>%
  # Clean the column headers 
  janitor::clean_names() %>%
  # Drop the ID column 
  dplyr::select(-c(id))
head(clim_sp)

# Clean GPS points 
clim_sp <- clim_sp %>%
  dplyr::mutate(
    presence = "1"
  ) %>%
  dplyr::select(
    lon = x,
    lat = y, 
    bio2 = wc2_1_2_5m_bio_2,
    bio7 = wc2_1_2_5m_bio_7,
    bio8 = wc2_1_2_5m_bio_8,
    bio9 = wc2_1_2_5m_bio_9,
    bio14 = wc2_1_2_5m_bio_14,
    bio15 = wc2_1_2_5m_bio_15,
    bio18 = wc2_1_2_5m_bio_18,
    presence
  )
head(clim_sp)

# Combine background and actual GPS points
df_gps <- 
  dplyr::bind_rows(
    clim_sp, 
    bg_gps
  ) 
head(df_gps)

# Import Maxent prediction raster 
clim_map <- terra::rast(
  here::here("./worked_example/model/model_fit/native_model/raster_pred_aus.tif")
)
terra::plot(clim_map)

# Extract MaxEnt scores 
df_suit <- terra::extract(
  x = clim_map,
  y = df_gps[, 1:2]
)
head(df_suit)

# Combine MaxEnt scores with source modifiers
df <- dplyr::bind_cols(df_gps, df_suit)
head(df)

# Fit model 
m1 <- glm(as.factor(presence) ~
            maxent,
          data = df,
          family = binomial(link = "logit"))
summary(m1)
margins::margins(m1)

# Predictions 
preds <-
  ggeffects::ggeffect(
    model = m1, 
    terms = c("maxent [0:1 by = 0.01]"),
    type = "fe"
  ) %>%
  as.data.frame(.)
head(preds)

# Make plot 
preds %>%
  ggplot(data = ., aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2) + 
  geom_line() +
  labs(
    x = "MaxEnt suitability",
    y = "Probability of presence"
  )

# Save figure to PC
# ggsave(
#   "./models/native_model/figures/maxent_validation_rsa.png",
#   dpi = 600,
#   height = 6,
#   width = 8
# )

# Extract model predictions  
mod_pred <- predict(m1, df, type = "response")
head(mod_pred)

# Remove NA rows 
mod_preds <- 
  data.frame(df, mod_pred) %>%
  tidyr::drop_na(mod_pred)
head(mod_preds)

# We need to find the optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(
  actuals = as.numeric(mod_preds$presence), 
  predictedScores = mod_preds$mod_pred,
  optimiseFor = "Both"
)[1]
optimal

# Calculate sensitivity
sens_aus <- InformationValue::sensitivity(
  actuals = as.numeric(mod_preds$presence), 
  predictedScores = mod_preds$mod_pred,
  threshold = optimal
)
sens_aus

# Calculate sensitivity
misclass_aus <- InformationValue::misClassError(
  actuals = as.numeric(mod_preds$presence), 
  predictedScores = mod_preds$mod_pred,
  threshold = optimal
)
misclass_aus

# Calculate AUCROC
auc_aus <- InformationValue::plotROC(
  actuals = as.numeric(mod_preds$presence), 
  predictedScores = mod_preds$mod_pred
)
auc_aus
