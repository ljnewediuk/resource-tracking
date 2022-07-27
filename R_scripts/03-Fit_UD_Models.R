
# Load packages
library(tidyverse)

# Source the function to fit UDs
source('R_functions/Fit_AKDE.R')

# Load data
dat <- readRDS('input/vita_elk_vectronic_feb_2019-march_2021_12_hr.rds') %>%
  rbind(readRDS('input/vita_elk_lotek_feb_2016-july_2019_cleaned.rds')) %>%
  # Add columns for Julian date and year
  mutate(Julian = lubridate::yday(dat_time), 
         year = lubridate::year(dat_time))

# Make table of elkyears to calculate UDs
elkyears <- dat %>%
  st_drop_geometry() %>%
  group_by(animal_ID, year) %>%
  summarize(n())

# Specify whether to fit akde models or utilization distributions
# output <- 'model'
output <- 'ud'

# Initiate list for models
akde_list <- list()
# Initiate data.frame for tracking models
akde_df <- data.frame()

# Specify date ranges over which to calculate models
hr_dates <- list(c(122, 213), c(214, 305))

# Compile UDs for individuals into list to get overlaps
for(i in 1:nrow(elkyears)) {
  
  # Compile for May-July and August-October HRs
  for(j in 1:2) {
    
    # Overlap function
    akde_mod <- fit_akde(dt = dat, id = elkyears[i ,]$animal_ID, 
                       yr = elkyears[i ,]$year, period = hr_dates[[j]], 
                       type = output)
    
    # Make sure the model worked properly
    if(class(akde_mod)[1] == 'ctmm' | class(akde_mod)[1] == 'UD') {
      
      # Compile into list, with current ID as last element of list
      akde_list[[length(akde_list) + 1]] <- akde_mod
      # Name last element of list after elk
      names(akde_list)[length(akde_list)] <- 
        paste0(elkyears[i ,]$animal_ID, '_', 
               elkyears[i ,]$year, '_', hr_dates[[j]][[1]], '-', hr_dates[[j]][[2]])
      
      # Track name of elk, year, and period if model was produced
      akde_info <- data.frame(animal_ID = elkyears[i ,]$animal_ID,
                              year = elkyears[i ,]$year,
                              period = paste(hr_dates[[j]][[1]], 
                                             hr_dates[[j]][[2]], sep = '-'))
      # Bind together
      akde_df <- rbind(akde_df, akde_info)
      
      # If a model was not produced, move to next
    } else {
      
      next
      
    }
  }
}

# Save UDs and models
saveRDS(akde_list, paste0('output/', 'akde_', output, 's.rds'))
saveRDS(akde_df, 'output/akde_list.rds')

