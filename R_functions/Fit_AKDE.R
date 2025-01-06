
#################################################
### Converts cleaned GPS location data        ###
### into home range contour based on the      ###
### autocorrelated-Kernel density estimator   ###
### (Winner et al. 2018 MEE)                  ###
###                                           ###
### Best movement model is selected based     ###
### on log-likelihood (AICc)                  ###
###                                           ###
### Returns a ctmm object for each specified  ###
### individual/year/sampling period that      ###
### represents the best model                 ###
###                                           ###
#################################################

# Required columns:
#   animal_ID, lat & long, dat_time, Julian, year
# Variables:
#   dt: dataframe object, 
#   id: character (unique identifier),
#   year: numeric, 
#   period: character vector (c(start Jday, end Jday)), 
#   tz: character (timezone, defaults to 'GMT')
#   epsg: coordinate reference system for data in EPSG (default 26914)
#   format: character (date format, defaults to '%Y-%m-%d %H:%M:%OS')

library(sf)
library(sp)
library(tidyverse)
library(ctmm)

fit_akde <- function(dt, id, yr, period, type = 'model', 
                     tz = 'GMT', epsg = 26914, format = '%Y-%m-%d %H:%M:%OS') {
  # Subset the data to 
  dat_sub <- dt %>%
    filter(animal_ID == id) %>%
    filter(year == yr) %>%
    mutate(timestamp = as.character(dat_time))  %>%
    mutate(individual.local.identifier = paste(animal_ID, yr, sep='_')) %>%
    rename('location.long' = long,
           'location.lat' = lat) %>%
    filter(Julian >= period[1] & Julian <= period[2]) 
  
  # Print message if elkyear does not have data within last 10 d of period
  if(! max(dat_sub$Julian) %in% tail(period[1]: period[2], n = 10)) {
    
    cat('Not enough data to estimate HR for ', id, '; moving to next', sep = '')
    return(NULL)
  }
  
  # Use Try to continue function if not enough data
  try({
    
    # Filter only locs from year of interest
    dat_telem <- dat_sub %>% 
      select(location.long, location.lat, 
             individual.local.identifier, timestamp) %>%
      # Need to drop geometry for as.telemetry function
      st_drop_geometry() %>%
      # Convert to ctmm::telemetry object
      as.telemetry(tz = 'GMT', format = '%Y-%m-%d %H:%M:%OS')
    
    # Set projection
    projection(dat_telem) <- CRS(paste0('+init=epsg:', epsg))
    
    # Fit movement model to data, starting with initial parameter guesses
    model_est <- ctmm.guess(dat_telem, interactive = F)
    # Fit multiple models for selection
    ctmm_list <- ctmm.select(dat_telem, 
                             CTMM    = model_est,
                             verbose = TRUE,
                             level   = 1,
                             method  = "pHREML",
                             control=list(method="pNewton",zero=TRUE))
    # Select model with lowest AICc (at top of model list)
    best_fit <- ctmm_list[[1]]
    
    # Calculate akde home range estimate
    best_ud <- akde(dat_telem, best_fit)
    # Return either the home range or the model
    if(type == 'model') return(best_fit)
    if(type == 'ud') return(best_ud)
    
  }, silent = T)
  
}

