
library(landscapemetrics)
library(stars)
library(tidyverse)
library(ctmm)

# Source contrast-weighted edge density function
source('R_functions/Contrast_Edges.R')

# Load rasters
raster_list <- readRDS('output/raster_list.rds')
# Load weights
raster_weights <- readRDS('output/raster_weights.rds')
# Load utilization distributions
uds <- readRDS('output/akde_uds.rds')

# Calculate landscape metrics for total landscape

# Initiate df
lc_metrics <- data.frame()

for(yr in 16:20) {
  
  # AGGREGATION METRIC
  cwed <- lsm_l_cwed(landsc = raster_list[[paste0('lc_', yr)]], 
                     c_weights = raster_weights) %>%
    pull(value)
  # COMPOSITION METRIC
  pforest <- lsm_c_pland(raster_list[[paste0('lc_', yr)]]) %>%
    filter(class == 9) %>%
    pull(value)
  # DIVERSITY METRIC
  shdi <- lsm_l_shdi(raster_list[[paste0('lc_', yr)]]) %>%
    pull(value)
  
  # Compile DF row
  metrics_row <- data.frame(year = paste0('20', yr),
                            cwed, pforest, shdi)
  # Bind to data frame
  lc_metrics <- rbind(lc_metrics, metrics_row)
  
}

# Calculate landscape metrics for home ranges

# Initiate df
hr_metrics <- data.frame()

for(i in 1:length(uds)) { 
  
  # Make sf object out of ud
  id_ud <- ctmm::SpatialPolygonsDataFrame.UD(uds[[i]]) %>%
    as('sf')
  
  # Get current year of home range (last two numbers)
  yr <- as.numeric(gsub("(.*_){2}(\\d+)_.+", "\\2", names(uds)[i])) %% 100
  # Get elk name
  id <- substr(gsub("(.*_){1}(\\d+)_.+", "\\1", names(uds)[i]), 1,
               nchar(gsub("(.*_){1}(\\d+)_.+", "\\1", names(uds)[i]))-1)
  # Get period
  prd <- paste0(as.numeric(gsub('.*-([0-9]+).*','\\1',
                  names(uds)[i])) - 91, '-',
                gsub('.*-([0-9]+).*','\\1', names(uds)[i]))
  
  # Crop the habitat raster to the ud, matching by year
  cropped_rast <- st_crop(raster_list[[paste0('lc_', yr)]], id_ud)
  
  # Calculate landscape metrics

  # AGGREGATION METRIC: 
  # Contrast-weighted edge density; this will measure how much edge changes
  # between land use classes between years; higher weight to dissimilar classes
  cwed <- lsm_l_cwed(landsc = cropped_rast, c_weights = raster_weights) %>%
    pull(value)
  # COMPOSITION METRIC:
  # Class-level total percentage of landscape covered by forest
  # This will give us an idea of crop turnover between years
  # (Percentage makes it comparable among landscapes of different extent)
  pforest <- lsm_c_pland(cropped_rast) %>%
    filter(class == 9) %>%
    pull(value)
  # DIVERSITY METRIC:
  # Shannon's diversity index
  shdi <- lsm_l_shdi(cropped_rast) %>%
    pull(value)
  
  # Compile DF row
  metrics_row <- data.frame(year = yr,
                            animal_ID = id,
                            period = prd,
                            cwed, pforest, shdi)
  # Bind to data frame
  hr_metrics <- rbind(hr_metrics, metrics_row)
  
}

# Save metrics
# Home range
saveRDS(hr_metrics, 'output/hr_raster_metrics.rds')
# Landscape
saveRDS(lc_metrics, 'output/lc_raster_metrics.rds')

# Remove raster data
system("rm -rf ~/Documents/R-Projects/site_fidelity/rasters/")
