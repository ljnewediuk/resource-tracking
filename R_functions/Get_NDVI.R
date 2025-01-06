
#################################################
###                                           ###
### Function to calculate NDVI                ###
###                                           ###
### Returns NDVI value between -1 and 1 for   ###
### either the entire landscape or a          ###
### specified home range                      ###
###                                           ###
#################################################

library(sf)
library(reticulate)
library(rgee)
library(tidyverse)

# Variables:
#   id: (optional) character, identifier for the individual if specifying HRdf, 
#   year: numeric, within which to calculate NDVI
#   period: character, range of dates within which to calculate NDVI, 
#   bounds: ee.Geometry object specifying the boundaries of the study area
#   HRdf: (optional) list of ctmm objects outlining individual home ranges

get_NDVI <- function(id = NULL, year, period, bounds, HRdf = NULL) {
  
  # Set start dates
  if(period == '122-213') {
    start_dt <- ee$Date(paste0(year, '-05-02'))
    end_dt <- ee$Date(paste0(year, '-08-01'))
  }
  if(period == '214-305') {
    start_dt <- ee$Date(paste0(year, '-08-02'))
    end_dt <- ee$Date(paste0(year, '-11-01'))
  }
  
  # Get EE image
  #   USGS Landsat 8 Collection 1 Tier 1 and Real-Time data Raw Scenes
  #   Filter within study boundaries, sort by cloud cover and choose the least
  #   cloudy image
  img <- ee$ImageCollection('LANDSAT/LC08/C01/T1_RT')$filterBounds(
    bounds)$filterDate(start_dt, end_dt)$sort(
      'CLOUD_COVER')$reduce(ee$Reducer$mean())
  
  # Make function to calculate NDVI (Near Infrared - Red)
  calcNDVI <- function(image) {
    return(image$normalizedDifference(c("B5_mean", "B4_mean")))
  }
  
  # Calculate NDVI
  img_ndvi <- calcNDVI(img)
  
  # If masking with a home range, do so now before proceeding
  if(! is.null(HRdf)) {
    # Create mask from HR
    mask <- ctmm::as.sf(HRdf[paste(id, year, period, sep = '_')][[1]], 
                        level.UD=0.95) %>%
      slice(2:2) %>%
      sf_as_ee()
    # Clip
    img_ndvi <- img_ndvi$clip(mask)
    # Calculate mean and pull NDVI value
    ndvi_means <- ee_extract(img_ndvi, mask, fun = ee$Reducer$mean()) %>%
      pull(nd)

    # Otherwise, just use entire study area
  } else {
    img_ndvi <- img_ndvi
    # Calculate mean and pull NDVI value
    ndvi_means <- ee_extract(img_ndvi, geometry, fun = ee$Reducer$mean()) %>%
      pull(nd)
  }
  
  # Return NDVI for the mean estimated bounds of the HR
  return(ndvi_means)
  
}

