
#################################################
###                                           ###
### Calculate mean NDVI for home ranges       ###
###                                           ###
### Summarize NDVI across home ranges (2nd    ###
### year for each individual), then weight    ###
### by mean NDVI in that year for the entire  ###
### landscape.                                ###
###                                           ###
#################################################

library(sf)
library(reticulate)
library(rgee)
library(tidyverse)

# Set up Earth Engine credentials
# Authorizes access to GEE API and Google Drive to store data
# (Might need to re-authenticate after changing password credentials)
ee_Initialize(user = 'j.newedi@gmail.com', drive = T)

# Source NDVI function
source('R_functions/Get_NDVI.R')

# Load elk home ranges and list of home ranges
hr_bounds <- readRDS('output/akde_uds.rds')
hr_list <- readRDS('output/akde_list.rds')

# Load elk data and bind together
lotek_dat <- readRDS('input/vita_elk_lotek_feb_2016-july_2019_cleaned.rds')
vertex_dat <- readRDS('input/vita_elk_vectronic_feb_2019-march_2021_cleaned.rds')
elk_dat <- rbind(lotek_dat, vertex_dat)

# Transform to projected coordinates
elk_dat_utm <- st_transform(elk_dat, crs = st_crs(26914))

# Create bounding box around the elk data
range_bb <- st_bbox(elk_dat_utm)
# Add 2 km buffer
range_bb <- st_bbox(c(xmin = range_bb[[1]] - 2000, ymin = range_bb[[2]] - 2000, 
                      xmax = range_bb[[3]] + 2000, ymax = range_bb[[4]] + 2000),
                    crs = st_crs(26914))

# Define geometry region for landscape
geometry <- ee$Geometry$Rectangle(
  coords = as.numeric(range_bb),
  proj = "EPSG:26914",
  geodesic = FALSE
)

# Summarize NDVI
# Initiate df
NDVI_dat <- data.frame()
# Start loop
for(i in 1:nrow(hr_list)) {
  
  # Calculate landscape NDVI
  lc_NDVI <- suppressMessages(
    get_NDVI(year = hr_list[i ,]$year, 
             period = hr_list[i ,]$period, 
             bounds = geometry))
  # Calculate home range NDVI
  hr_NDVI <- suppressMessages(
    get_NDVI(id = hr_list[i ,]$animal_ID, 
             year = hr_list[i ,]$year, 
             period = hr_list[i ,]$period, 
             bounds = geometry, HRdf = hr_bounds))
  
  # Bind together
  NDVI_row <- data.frame(animal_ID = hr_list[i ,]$animal_ID,
                         year = hr_list[i ,]$year,
                         period = hr_list[i ,]$period,
                         hr_NDVI,
                         lc_NDVI)
  NDVI_dat <- rbind(NDVI_dat, NDVI_row)
  
}

# Save NDVI data
saveRDS(NDVI_dat, 'output/NDVI_dat.rds')



